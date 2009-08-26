;; Antiweb (C) Doug Hoyte

;; Default HTTP modules

(antiweb-module mod-rewrite
  :rewrite-phase
    `(macrolet ((done-rewrite ()
                  '(return-from mod-rewrite-block nil)))
       (block mod-rewrite-block
         ,@(xconf-get-all handler :rewrite))))



(antiweb-module mod-dir
  :directory-handling-phase
    `(let* ((real-path ,(http-root-translate handler 'http-path))
            (stat (cffi:with-foreign-string (pstr real-path)
                    (aw_stat_returning_a_static_struct pstr))))
      (unless (cffi:null-pointer-p stat)

       (unless (zerop (aw_stat_is_dir stat))
         (if (zerop (aw_stat_is_world_readable stat))
           (err-and-linger 403 "Directory is not world readable"))

         (unless (#~m|/$| http-path)
           (send-http-response-headers 301
             ("Location" "http://~a~a/" http-host http-path)
             ("Content-Length" "0"))
           (return-this-closure keepalive-closure))

         (tagbody
           ,@(mapcar
               #`(let* ((index-file (format nil "~a~a" http-path ,a1))
                        (real-index-file ,(http-root-translate handler 'index-file))
                        (stat (cffi:with-foreign-string (pstr real-index-file)
                                (aw_stat_returning_a_static_struct pstr))))
                   (unless (cffi:null-pointer-p stat)
                     (setq http-path index-file)
                     (go escape)))
               (xconf-get handler :index))

           ,(if (xconf-get handler :dir-listings)
              '(progn
                 (if (eq http-method 'post)
                   (err-and-linger 405 "Can't POST to a directory"))
                 (setq $u-keepalive nil)
                 (send-http-response-headers 200
                   ("Content-Type" "text/html; charset=utf-8"))
                 (write-to-conn-from-string c
                   (format nil #"<html><head><title>Antiweb Directory Listing: ~a~a</title>
<style type="text/css"> <!--
body,h1 { margin:10px; font-family: Verdana, Arial, sans-serif; }
--> </style></head><h1>~a~a</h1><body>"#
                               http-host http-path http-host http-path))
                 (cffi:with-foreign-string (pstr real-path)
                   (aw_send_dir_listings c pstr))
                 (linger-this-http-conn c)
                 (write-to-conn-from-string c
                   (format nil #"<br><hr>End of Antiweb ~a Directory Listing</body></html>"# AW_VERSION))
                 (return-this-closure 'sent-dir-listing))
              '(err-and-linger 404 "Directory index doesn't exist"))
           escape)))))



(antiweb-module mod-awp
  :awp-phase
    (if (xconf-get handler :awp)
      `(when-match (,(list-of-file-extensions-to-regex-with-path-info '(awp)) http-path)
         (setq http-path $1)
         (setq $u-path-info $2)
         (let* ((real-path ,(http-root-translate handler 'http-path))
                (stat (cffi:with-foreign-string (pstr real-path)
                        (aw_stat_returning_a_static_struct pstr))))
           (if (cffi:null-pointer-p stat)
             (err-and-linger 404 "File doesn't exist"))
           (if (zerop (aw_stat_is_world_readable stat))
             (err-and-linger 403 "AWP file isn't world readable"))
           (when (zerop (length $u-path-info))
             (send-http-response-headers 301
               ("Location" "http://~a~a/" http-host http-path)
               ("Content-Length" "0"))
             (return-this-closure keepalive-closure))
           (let ((awp-file-path ,(http-root-translate handler 'http-path))
                 (cache-path (format nil "~a/~a~a" (get-aw-worker-cache-or-error) ,(car (xconf-get handler :hosts)) http-path)))
             (let ((awp-compile-result (awp-compile-error-wrapper awp-file-path (aw_stat_get_mtime stat) cache-path)))
               (when (stringp awp-compile-result)
                 (aw-log () "failed .awp request: (~a) ~a" awp-file-path awp-compile-result)
                 (err-and-linger 500 "Anti Webpage didn't compile. See syslog.")))
             (setq $u-awp-real-path (format nil "~a~a" cache-path (if (= 1 (length $u-path-info))
                                                                    "/index.html" $u-path-info)))
             (when (eq http-method 'post)
               (let ((post-len (parse-integer $h-content-length)))
                 (if (zerop post-len)
                   (err-and-linger 400 "Empty POST to AWP file"))
                 (if (> post-len AW_MAX_MSG_LENGTH)
                   (err-and-linger 413 "POST Content-Length too large for AWP file"))
                 (cffi:with-foreign-slots ((ready limit sep) c conn)
                   (return-from http-user-dispatch-macro
                     (read-fixed-length-message-from-conn-and-store-in-shared-input-buffer post-len
                       (block http-user-dispatch-macro
                         (let ((post-handlers (awp-page-post-handlers (gethash awp-file-path awp-loaded-pages))))
                           (dolist (ph post-handlers)
                             (let ((res (funcall ph $u-path-info
                                                 :cookie $h-cookie
                                                 :referer $h-referer
                                                 :args shared-input-buffer)))
                               (when (stringp res)
                                 (send-raw res)
                                 (keepalive)))))
                         (err-and-linger 405 "Unhandled POST to AWP file")))))))
             (when (eq http-method 'get)
               (let ((get-handlers (awp-page-get-handlers (gethash awp-file-path awp-loaded-pages))))
                 (dolist (gh get-handlers)
                   (let ((res (funcall gh $u-path-info
                                       :ip $ip
                                       :cookie $h-cookie
                                       :referer $h-referer
                                       :args http-args
                                       :x-real-ip ,(if (xconf-get handler :accept-x-real-ip-from)
                                                     '$u-used-x-real-ip)
                              )))
                     (when (stringp res)
                       (send-raw res)
                       (keepalive)))))))))))


(antiweb-module mod-cgi
  :cgi-phase
    (if (or (xconf-get handler :cgi) (xconf-get handler :naked-cgi))
      `(let (this-is-a-cgi naked-cgi)
         ,(if (xconf-get handler :cgi)
            `(when-match (,(list-of-file-extensions-to-regex-with-path-info (xconf-get handler :cgi)) http-path)
               (setq http-path $1)
               (setq $u-path-info $2)
               (setq this-is-a-cgi t)))
         ,(if (xconf-get handler :naked-cgi)
            `(when-match (,(list-of-file-extensions-to-regex-with-path-info (xconf-get handler :naked-cgi)) http-path)
               (setq http-path $1)
               (setq $u-path-info $2)
               (setq this-is-a-cgi t
                     naked-cgi t)))
         (when this-is-a-cgi
           (let* ((real-path ,(http-root-translate handler 'http-path))
                  (stat (cffi:with-foreign-string (pstr real-path)
                          (aw_stat_returning_a_static_struct pstr))))
             (if (cffi:null-pointer-p stat)
               (err-and-linger 404 "File doesn't exist"))
             (if (zerop (aw_stat_is_world_readable stat))
               (err-and-linger 403 "CGI script isn't world readable"))
             (if (zerop (aw_stat_is_world_executable stat))
               (err-and-linger 403 "CGI script isn't world executable"))
             (let ((single-process-cgis-only ,(if (xconf-get handler :cgi-no-forking) 1 0))
                   (maxfiles ,(or (xconf-get handler :cgi-maxfiles) 50)))
               (let ((cgi-conn
                       (cffi:with-foreign-string (pstr real-path)
                         (cffi:with-foreign-string (pistr $u-path-info)
                           (cffi:with-foreign-string (astr (or http-args ""))
                             (cffi:with-foreign-string (cstr (or $h-cookie ""))
                               (cffi:with-foreign-string (ctstr (or $h-content-type ""))
                                 (if (eq http-method 'post)
                                   (cffi:with-foreign-string (method "POST")
                                     (aw_build_cgi_conn c pstr pistr astr method cstr ctstr $u-content-length-num
                                                        100000 50000 ; tune this
                                                        single-process-cgis-only maxfiles
                                                        (if naked-cgi 1 0)))
                                   (cffi:with-foreign-string (method "GET")
                                     (aw_build_cgi_conn c pstr pistr astr method cstr ctstr 0
                                                          0 0
                                                          single-process-cgis-only maxfiles
                                                          (if naked-cgi 1 0)))))))))))
                 (add-to-conn-table cgi-conn
                   'cgi-proxy-sink-should-never-become-ready)
                 (return-this-closure
                   (fsm cgi-source
                     (cffi:with-foreign-slots ((conntype) c conn)
                       (setf conntype AW_CONNTYPE_ZOMBIE))
                     (cffi:with-foreign-slots ((conntype) cgi-conn conn)
                       (setf conntype AW_CONNTYPE_ZOMBIE))
                     'proxy-sink-finished-sending-cgi-data)))))))))



(antiweb-module mod-jsmin
  :jsmin-phase
    (if (xconf-get handler :jsmin)
      `(when (and (null $u-awp-real-path)
                  (#~m/[.]js$/ http-path))
         (let* ((real-path ,(http-root-translate handler 'http-path))
                (stat (cffi:with-foreign-string (pstr real-path)
                        (aw_stat_returning_a_static_struct pstr))))
           (unless (cffi:null-pointer-p stat)
             (ignore-errors ; invalid javascript file (ie unclosed comment), clients will be sent original
               (let* ((orig-mtime (aw_stat_get_mtime stat))
                      (cache-path (format nil "~a/~a~a.min.js"
                                          (get-aw-worker-cache-or-error)
                                          ,(car (xconf-get handler :hosts))
                                          http-path))
                      (stat (cffi:with-foreign-string (pstr cache-path)
                              (aw_stat_returning_a_static_struct pstr))))
                 (when (or (cffi:null-pointer-p stat) (> orig-mtime (aw_stat_get_mtime stat)))
                   (cffi:with-foreign-string (cstr cache-path)
                     (aw_mkdir_dash_p cstr)
                     (jsmin:jsmin-file real-path cache-path)
                     (cffi:with-foreign-string (gstr (format nil "~a.gz" cache-path))
                       (aw_gzip_file cstr gstr))))
                 (setq $u-awp-real-path cache-path))))))))



(antiweb-module mod-cssmin
  :cssmin-phase
    (if (xconf-get handler :cssmin)
      `(when (and (null $u-awp-real-path)
                  (#~m/[.]css$/ http-path))
         (let* ((real-path ,(http-root-translate handler 'http-path))
                (stat (cffi:with-foreign-string (pstr real-path)
                        (aw_stat_returning_a_static_struct pstr))))
           (unless (cffi:null-pointer-p stat)
             (ignore-errors ; invalid javascript file (ie unclosed comment), clients will be sent original
               (let* ((orig-mtime (aw_stat_get_mtime stat))
                      (cache-path (format nil "~a/~a~a.min.css"
                                          (get-aw-worker-cache-or-error)
                                          ,(car (xconf-get handler :hosts))
                                          http-path))
                      (stat (cffi:with-foreign-string (pstr cache-path)
                              (aw_stat_returning_a_static_struct pstr))))
                 (when (or (cffi:null-pointer-p stat) (> orig-mtime (aw_stat_get_mtime stat)))
                   (cffi:with-foreign-string (cstr cache-path)
                     (aw_mkdir_dash_p cstr)
                     (cssmin-file real-path cache-path)
                     (cffi:with-foreign-string (gstr (format nil "~a.gz" cache-path))
                       (aw_gzip_file cstr gstr))))
                 (setq $u-awp-real-path cache-path))))))))




(defun expand-into-form-that-looks-up-mime-type-of-real-path-and-stores-in-$u-mime-type (handler)
  `(progn
     (when-match (#~m/[.](\w+)$/ real-path)
       ,(let ((mimes (mapcar (lambda (m) (list (if (stringp (car m)) (car m) (string-downcase (symbol-name (car m))))
                                               (cadr m)))
                             (xconf-get-all handler :mime-type))))
          (if mimes
            `(setq $u-mime-type (lookup-mime-type $1 ',mimes))))
       (unless $u-mime-type
         (setq $u-mime-type (lookup-mime-type $1 aw-mime-types))))
     (unless $u-mime-type
       (setq $u-mime-type ,(if (xconf-get handler :default-mime-type)
                             (xconf-get handler :default-mime-type)
                             'aw-default-mime-type)))))

(antiweb-module mod-regular-file
  :regular-file-phase
    `(let* ((real-path (or $u-awp-real-path ,(http-root-translate handler 'http-path)))
            (stat (cffi:with-foreign-string (pstr real-path)
                    (aw_stat_returning_a_static_struct pstr))))
       (if (cffi:null-pointer-p stat)
         (err-and-linger 404 "File doesn't exist"))
       (if (eq http-method 'post)
         (err-and-linger 405 "You can't POST to a regular file"))
       (if (zerop (aw_stat_is_world_readable stat))
         (err-and-linger 403 "File isn't world readable")
         (progn
           ,(expand-into-form-that-looks-up-mime-type-of-real-path-and-stores-in-$u-mime-type handler)

           (if $u-mime-type
             (add-header-to-response "Content-Type: ~a" $u-mime-type))

           ,(if (xconf-get handler :etags)
              (let ((hash (xconf-get handler :etag-hash)))
                (if hash
                  `(setq $u-etag (format nil #""~a""#
                                         (aw-sha1 (format nil #"~a-~a-~a-~a"#
                                                              ,hash
                                                              (aw_stat_get_inode stat)
                                                              (aw_stat_get_file_size stat)
                                                              (aw_stat_get_mtime stat)))))
                  '(setq $u-etag (format nil #""~x-~x-~x""#
                                         (aw_stat_get_inode stat)
                                         (aw_stat_get_file_size stat)
                                         (aw_stat_get_mtime stat))))))

           ,(if (xconf-get handler :etags)
              '(when (equalp $u-etag $h-if-none-match)
                 (send-http-response-headers 304
                   ("Etag" "~a" $u-etag))
                 (return-this-closure keepalive-closure)))

           ,(if (xconf-get handler :download-resuming)
              '(if $h-range 
                 (when-match (#~m/bytes=(\d+)-/ $h-range)
                   (let ((offset (parse-integer $1))
                         (file-size (aw_stat_get_file_size stat)))
                     (when (< 0 offset file-size)
                       (send-http-response-headers 206
                         ("Content-Range" "~a-~a/~a" offset (1- file-size) file-size)
                         ("Content-Length" "~a" (- file-size offset)))
                       (cffi:with-foreign-string (fstr real-path)
                         (aw_send_file_to_http_conn c fstr stat offset))
                     (return-this-closure keepalive-closure))))))

           ,(if (xconf-get handler :gzip)
              `(if (or
                     (and $u-awp-real-path
                          (#~m/gzip/ $h-accept-encoding))
                     (and (,(list-of-file-extensions-to-regex (xconf-get handler :gzip)) http-path)
                          (#~m/gzip/ $h-accept-encoding)
                          (<= ,(or (car (xconf-get handler :gzip-size-range)) 256)
                              (aw_stat_get_file_size stat)
                              ,(or (cadr (xconf-get handler :gzip-size-range)) 200000))))
                 (let* ((orig-mtime (aw_stat_get_mtime stat))
                        (cache-path (if $u-awp-real-path
                                      (format nil "~a.gz" $u-awp-real-path)
                                      (format nil "~a/~a~a.gz" (get-aw-worker-cache-or-error)
                                                               ,(car (xconf-get handler :hosts))
                                                               http-path))))
                   (cffi:with-foreign-string (pstr cache-path)
                     (let ((stat (aw_stat_returning_a_static_struct pstr)))
                       (if (and $u-awp-real-path (cffi:null-pointer-p stat))
                         (fatal "Couldn't find gziped awp file ~a" $u-awp-real-path))
                       (if (or (cffi:null-pointer-p stat)
                               (> orig-mtime (aw_stat_get_mtime stat)))
                         (cffi:with-foreign-string (fstr real-path)
                           (aw_mkdir_dash_p pstr)
                           (aw_gzip_file fstr pstr)
                           (setq stat (aw_stat_returning_a_static_struct pstr))))
                       (send-http-response-headers 200
                         ("Content-Length" "~a" (aw_stat_get_file_size stat))
                         ("Content-Encoding" "gzip")
                         ,@(if (xconf-get handler :etags)
                             '(("Etag" "~a" $u-etag))))
                       (aw_send_file_to_http_conn c pstr stat 0)
                       (return-this-closure keepalive-closure))))))

           (send-http-response-headers 200
             ("Content-Length" "~a" (aw_stat_get_file_size stat))
             ,@(if (xconf-get handler :etags)
                 '(("Etag" "~a" $u-etag))))
           (cffi:with-foreign-string (fstr real-path)
             (aw_send_file_to_http_conn c fstr stat 0))
           (return-this-closure keepalive-closure)))))





(defvar aw-fast-1x1gif-data
  #.(coerce
      (mapcar (lambda (e) (if (numberp e) (code-char e) e))
              (read-from-string
                  ; lisp
(#~s/,//          ;  /\          THE GOLDEN TRIANGLE
(#~s/0x/#x/       ; /  \                         \
(#~s/'/#\/        ;.----. perl                    \
(#~s/',//         ;  ||                            \
(#~s|/[*].*?\n||  ;  ||    THE HOLY TRINITY       / \
                  ;  C                           /   \

;; The following 43-byte 1x1 gif is from nginx:
;; /*
;;  * Copyright (C) Igor Sysoev
;;  */

#"(
    'G', 'I', 'F', '8', '9', 'a',  /* header                                 */

                                   /* logical screen descriptor              */
    0x01, 0x00,                    /* logical screen width                   */
    0x01, 0x00,                    /* logical screen height                  */
    0x80,                          /* global 1-bit color table               */
    0x01,                          /* background color #1                    */
    0x00,                          /* no aspect ratio                        */

                                   /* global color table                     */
    0x00, 0x00, 0x00,              /* #0: black                              */
    0xff, 0xff, 0xff,              /* #1: white                              */

                                   /* graphic control extension              */
    0x21,                          /* extension introducer                   */
    0xf9,                          /* graphic control label                  */
    0x04,                          /* block size                             */
    0x01,                          /* transparent color is given,            */
                                   /*     no disposal specified,             */
                                   /*     user input is not expected         */
    0x00, 0x00,                    /* delay time                             */
    0x01,                          /* transparent color #1                   */
    0x00,                          /* block terminator                       */

                                   /* image descriptor                       */
    0x2c,                          /* image separator                        */
    0x00, 0x00,                    /* image left position                    */
    0x00, 0x00,                    /* image top position                     */
    0x01, 0x00,                    /* image width                            */
    0x01, 0x00,                    /* image height                           */
    0x00,                          /* no local color table, no interlaced    */

                                   /* table based image data                 */
    0x02,                          /* LZW minimum code size,                 */
                                   /*     must be at least 2-bit             */
    0x02,                          /* block size                             */
    0x4c, 0x01,                    /* compressed bytes 01_001_100, 0000000_1 */
                                   /* 100: clear code                        */
                                   /* 001: 1                                 */
                                   /* 101: end of information code           */
    0x00,                          /* block terminator                       */

    0x3B                           /* trailer                                */
)"#))))))) 'string))



(antiweb-module mod-fast-files
  :fast-1x1gif-phase
    (let ((v (xconf-get handler :fast-1x1gif)))
      (if v
        `(when (string= http-path ,v)
           (send-http-response-headers 200
             ("Content-Type" "image/gif")
             ("Content-Length" "~a" (length aw-fast-1x1gif-data)))
           (write-to-conn-from-string c aw-fast-1x1gif-data)
           (return-this-closure keepalive-closure))))

  :fast-files-phase
    (let ((files (xconf-get handler :fast-files)))
      (if files
        `(labels
           ((send-fast-file (f)
             (let* ((real-path ,(http-root-translate handler 'f))
                    (val (gethash real-path aw-fast-files-table)))
               (if val
                 (if (stringp val)
                   (progn
                     (write-to-conn-from-string c val)
                     (return-this-closure keepalive-closure))
                   (err-and-keepalive 404 "File doesn't exist"))
                 (let ((file-data
                         (ignore-errors
                           (with-open-file (in real-path :direction :input :element-type '(unsigned-byte 8))
                             (let ((data-u8 (make-array (file-length in) :element-type '(unsigned-byte 8) :fill-pointer t))
                                   (data-char (make-string (file-length in))))
                               (setf (fill-pointer data-u8) (read-sequence data-u8 in))
                               (loop for i from 0 below (file-length in) do
                                 (setf (aref data-char i) (code-char (aref data-u8 i))))
                               data-char)))))
                   (when file-data
                     ,(expand-into-form-that-looks-up-mime-type-of-real-path-and-stores-in-$u-mime-type handler)
                     (setf file-data (format nil
                                             "HTTP/1.1 200 OK~aAntiweb/~a~aContent-Type: ~a~aContent-Length: ~a~a~a~a~a"
                                                 crlf AW_VERSION crlf $u-mime-type crlf
                                                 (length file-data) crlf
                                                 ,(apply #'concatenate 'string
                                                    (mapcar (lambda (h) (format nil "~a~a" h crlf))
                                                            (xconf-get-all handler :fast-files-header)))
                                                 crlf file-data)))
                   (setf (gethash real-path aw-fast-files-table) (or file-data 404))
                   (send-fast-file f))))))
           (cond
             ,@(mapcar #`((string= ,a1 http-path) (send-fast-file http-path)) files)))))

)
