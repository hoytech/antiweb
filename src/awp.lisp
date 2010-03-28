;; Antiweb (C) Doug Hoyte

(defvar awp-reserved-options
  '(:doctype :title :css
    :pre-html
    :layout :layout-width
    :html
    :post-html
    :js-extern :js-main))

(defvar awp-loaded-pages (make-hash-table :test #'equal))

(defvar awp-glue-modules nil)

(defstruct awp-page
  mtime glues post-handlers get-handlers failed-compile)


(defmacro defglue (name args &rest body)
  `(progn
     (push (list ',name
                 (compile nil '(lambda ,args ,@body)))
           awp-glue-modules)))




;; Based on Cal Henderson's CSS minification algorithm here:
;; http://www.thinkvitamin.com/features/webapps/serving-javascript-fast
(defun cssmin (inp)
  (setq inp (#~s!\/\*(.*?)\*\/!! inp)) ; remove comments
  (setq inp (#~s!\s+! ! inp))          ; collapse space
  (setq inp (#~s!\} !}
! inp))                                ; add line breaks
  (setq inp (#~s!\n$!! inp))           ; remove last break
  (setq inp (#~s! \{ ! {! inp))        ; trim inside brackets
  (setq inp (#~s!; \}!}! inp))         ; trim inside brackets
  inp)

(defun cssmin-file (infile outfile)
  (with-open-file (in infile :direction :input)
    (let ((css (make-string (file-length in))))
      (read-sequence css in)
      (setq css (cssmin css))
      (with-open-file (out outfile :direction :output :if-exists :supersede :if-does-not-exist :create)
        (format out #"~a"# css)))))



;; returns t on success, otherwise throws error
(defun awp-compile (awp-file curr-mtime output-dir)
  (let ((rec (gethash awp-file awp-loaded-pages)))
    (if (and rec
             (awp-page-failed-compile rec)
             (= curr-mtime (awp-page-mtime rec)))
      (error "there was an error last time we compiled this AWP and mtime hasn't changed: ~a"
             (awp-page-failed-compile rec)))
    (when (or (not rec)
              (/= curr-mtime (awp-page-mtime rec)))
     (handler-bind ((error (lambda (condition)
                             (setf (gethash awp-file awp-loaded-pages)
                                   (make-awp-page :mtime curr-mtime
                                                  :failed-compile (format nil "~a" condition))))))
      (let ((conf (load-conf-from-file awp-file))
            (awp-glue-modules awp-glue-modules))
        (cffi:with-foreign-string (fstr (format nil "~a/" output-dir))
          (aw_mkdir_dash_p fstr))
        (with-open-file (o (workaround-cl-pathnames
                             (format nil "~a/aw_load.lisp" output-dir)) :direction :output :if-exists :supersede)
          (let ((*print-circle* t))
            (let ((elems (conf-get-all conf 'defglue)))
              (dolist (e elems)
                (format o #"~S~%"# e)))
            (let ((elems (conf-get-all conf 'progn)))
              (dolist (e elems)
                (format o #"~S~%"# e)))))
        (load (compile-file (workaround-cl-pathnames (format nil "~a/aw_load.lisp" output-dir))))
        (let ((pages (conf-get-all conf 'page)))
          (dolist (p pages)
            (when (stringp (cadr p))
              (let ((filename (format nil "~a/~a" output-dir (cadr p))))
                (awp-compile-page pages p filename)
                (cffi:with-foreign-string (src filename)
                  (cffi:with-foreign-string (dst (format nil "~a.gz" filename))
                    (aw_gzip_file src dst)))))))
        (let ((post-handlers (mapcar #'awp-compile-handler-form (conf-get-all conf 'post-handler)))
              (get-handlers (mapcar #'awp-compile-handler-form (conf-get-all conf 'get-handler))))
          (setf (gethash awp-file awp-loaded-pages)
                (make-awp-page :mtime curr-mtime
                               :glues awp-glue-modules
                               :get-handlers get-handlers
                               :post-handlers post-handlers))))))
    t))



;; (post-handler "/blah" (cookie) ...)
;; (get-handler #~m|^/blah/(.*)| () ... $1 ...)
(defun awp-compile-handler-form (form)
  (destructuring-bind (h-symbol path args &rest body) form
    (declare (ignore h-symbol))
    (compile nil
      (let ((g!http-path (gensym "http-path")))
        `(lambda (,g!http-path &key ,@args &allow-other-keys)
           (block nil
             ,(if (stringp path)
                `(when (string-equal ,path ,g!http-path)
                   ,@body)
                `(when-match (,path ,g!http-path)
                   ,@body))))))))


(defun awp-compile-page (pages p filename)
  (with-open-file (o (workaround-cl-pathnames filename) :direction :output :if-exists :supersede)
    (let ((page-with-inherits (awp-process-inherits pages p nil)))

      (let ((doctype (xconf-get page-with-inherits :doctype)))
        (if doctype
          (format o "~a~%" doctype)))

      (format o #"<html><head>"#)

      (let ((title (xconf-get page-with-inherits :title)))
        (if title
          (format o #"<title>~a</title>"# title)))

      (let ((css (reverse (xconf-get-all page-with-inherits :css))))
        (when css
          (format o #"<style type="text/css">"#)
          (format o #"~a"# (cssmin (format nil #"~{~a~%~}"# css)))
          (format o #"</style>"#)))

      (format o #"</head><body>"#)

      (let ((html (reverse (xconf-get-all page-with-inherits :pre-html))))
        (format o #"~{~a~}"# (mapcar #'awp-super-glue html)))

      (let ((layout (xconf-get page-with-inherits :layout)))
        (when layout
          (format o #"<div align=center id="page">"#)
          (format o "~a" (awp-compile-layout
                           layout
                           page-with-inherits
                           (xconf-get page-with-inherits :layout-width)))
          (format o #"</div>"#)))

      (let ((html (xconf-get page-with-inherits :html)))
        (when html
          (loop do
            (if-match (#~m/^(.*){{{([\w-]+)}}}(.*)$/s html)
              (setq html (format nil "~a~a~a"
                                 $1
                                 (or (xconf-get page-with-inherits (keyword-intern $2)) "")
                                 $3))
              (return)))
          (format o #"~a"# html)))

      (let ((html (xconf-get-all page-with-inherits :post-html))) ; NOT reversed
        (format o #"~{~a~}"# (mapcar #'awp-super-glue html)))

      (format o #"</body>"#)

      (let ((js (reverse (xconf-get-all page-with-inherits :js-extern))))
        (if js
          (format o #"~{<script src="~a"></script>~}"# js)))

      (let ((js (reverse (xconf-get-all page-with-inherits :js-main))))
        (when js
          (format o "<script>")
          (format o "~a" (jsmin:jsmin (format nil #"~{~a~%~}"# js)))
          (format o "</script>")))

      (format o #"</html>~%"#))))

(defun awp-process-inherits (pages p seen)
  (apply #'append p
    (mapcar (lambda (e)
              (if (member e seen :test #'equal)
                (error "LOOP: ~a inherits ~{~a~^ inherits ~} inherits ~a"
                       (car seen) (reverse (cdr seen)) (car seen)))
              (awp-process-inherits
                pages
                (find e (conf-get-all pages 'page) :key #'cadr :test #'equal)
                (cons e seen)))
              (xconf-get-all p :inherit))))

(defun awp-super-glue (v)
  (cond ((stringp v)
           (let ((scan (#~m/#[(]/ v))) ;-)
             (if scan
               (multiple-value-bind (val skip) (read-from-string v t nil :start (1+ scan) :preserve-whitespace t)
                 (concatenate 'string
                   (subseq v 0 scan)
                   (awp-super-glue val)
                   (awp-super-glue (subseq v skip))))
               v)))
        ((consp v)
           (apply (cadr (find (car v) awp-glue-modules :key #'car))
                  (cdr v)))
        (t
           (format nil "~a" v))))

(defun html-escape (s)
  (setq s (#~s/[&]/&amp;/ s))
  (setq s (#~s/[<]/&lt;/ s))
  (setq s (#~s/[>]/&gt;/ s))
  s)







(defun awp-compile-layout (layout page &optional layout-width)
  (setq layout (#~s/[\r\t]+/ / layout))
  (setq layout (#~s/\s+$/
/ layout))
  (let ((lines (cl-ppcre:split #"\n"# layout))
        (pixels-per-char 1000000))
    (dolist (l lines)
      (if (and layout-width
               (#~m/\|/ l)
               (< #1=(/ layout-width (length l)) pixels-per-char))
        (setq pixels-per-char #1#)))
    (with-output-to-string (s)
      (dolist (l lines)
        (if (#~m/\|/ l)
          (let ((segs (cl-ppcre:split #"\|"# l)))
            (format s #"<table cellpadding=0 cellspacing=0 border=0><tr>"#)
            (dolist (seg segs)
              (unless (zerop (length seg))
                (if-match (#~m/^\s*([\w-_]+)@?([lrtbh]{1,3}|)\s*$/ seg)
                  (let ((valign "middle") (align "center") hidden)
                    (unless $2 (setq $2 ""))
                    (if (#~m/l/i $2) (setq align "left"))
                    (if (#~m/r/i $2) (setq align "right"))
                    (if (#~m/t/i $2) (setq valign "top"))
                    (if (#~m/b/i $2) (setq valign "bottom"))
                    (if (#~m/h/i $2) (setq hidden t))
                    (format s #"<td align=~a valign=~a~a>&nbsp;<div id="~a"~a>~a</div></td>"#
                              align valign
                              (if layout-width
                                (format nil #" width=~a"# (floor (* (1+ (length seg)) pixels-per-char)))
                                "")
                              $1
                              (if hidden #" style="display:none""# "")
                              (awp-compile-layout-element
                                $1 page (if layout-width (floor (* (1+ (length seg)) pixels-per-char))))))
                  (format s #"<td~a>&nbsp;<div></div></td>"#
                            (if layout-width
                              (format nil #" width=~a"# (floor (* (1+ (length seg)) pixels-per-char)))
                              "")))))
            (format s #"</tr></table>"#)))))))

(defun keyword-intern (str)
  (intern (string-upcase str) #+cmu *keyword-package* #+clisp system::*keyword-package* #+ccl ccl::*keyword-package*))

(defun awp-compile-layout-element (element page &optional element-width)
  (let ((layout (xconf-get page (keyword-intern (format nil "~a-layout" element)))))
    (if layout
      (awp-compile-layout layout page element-width)
      (awp-super-glue (xconf-get page (keyword-intern element))))))
