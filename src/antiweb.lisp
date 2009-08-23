;; Antiweb (C) Doug Hoyte


(defmacro fsm (name &rest body)
  (unless (symbolp name) (error "fsm needs symbol for name"))
  `(let (,name)
     (declare (ignorable ,name))
     (setq ,name (lambda () ,@body))))


(defmacro! aw-log ((&key (log-file "syslog") (prefix "")) fmt &rest args)
  `(cffi:with-foreign-string (,g!fstr-log-file ,log-file)
     (cffi:with-foreign-string (,g!fstr-prefix ,prefix)
       (cffi:with-foreign-string (,g!fstr-msg (format nil ,fmt ,@args))
         (aw_log ,g!fstr-log-file ,g!fstr-prefix ,g!fstr-msg)))))


(defun fatal (fmt &rest args)
  (cffi:with-foreign-string (fstr (apply #'format nil fmt args))
    (aw_fatal fstr)))


(cffi:defcfun ("getpid" aw-getpid) :int)
(cffi:defcfun ("getuid" aw-getuid) :int)
(cffi:defcfun ("getgid" aw-getgid) :int)
(cffi:defcfun ("fork" aw-fork) :int)
(cffi:defcfun ("execl" aw-execl) :int (path :string) (arg :string) &rest)

(defun timespec-to-seconds (ts)
  (cond
    ((integerp ts) ts)
    ((listp ts)
      (* (car ts)
         (case (cadr ts)
           ((s) 1)
           ((m) 60)
           ((h) 3600)
           ((d) 86400)
           (t (error "bad timespec unit")))))
    (t (error "bad timespec"))))

(defun sizespec-to-bytes (ss)
  (cond
    ((integerp ss) ss)
    ((listp ss)
      (* (car ss)
         (case (cadr ss)
           ((b) 1)
           ((k) 1000)
           ((m) 1000000)
           (t (error "bad sizespec unit")))))
    (t (error "bad sizespec"))))






(defstruct rollback
  conf dispatch time comment)

(defun push-rollback (&optional (comment "no comment"))
  (push (make-rollback :conf aw-worker-conf
                       :dispatch (symbol-function 'http-user-dispatch)
                       :time (get-internal-real-time)
                       :comment comment)
        aw-rollbacks)
  (format nil #"pushed ~a"# (describe-rollback (car aw-rollbacks))))

(defun pop-rollback ()
  (if aw-rollbacks
    (pop aw-rollbacks)
    (error "no rollback to pop")))

(defun describe-rollback (rb)
  (format nil #"rollback [~a] created ~a ago"# (rollback-comment rb) (calculate-uptime (rollback-time rb))))

(defun rollback (&optional condition)
  (unless aw-rollbacks (error "no rollbacks available"))
  (let ((rb (pop-rollback)))
    (setq aw-worker-conf (rollback-conf rb))
    (setf (symbol-function 'http-user-dispatch) (rollback-dispatch rb))
    (aw-log () #"ROLLBACK: ~a (~a)"# (describe-rollback rb) (or condition "manual rollback"))
    (format nil #"installed ~a (~a)"# (describe-rollback rb) (or condition "manual rollback"))))

(defun pop-all-rollbacks ()
  (prog1 aw-rollbacks (setq aw-rollbacks nil)))

(defun flush-fast-files ()
  (setf aw-fast-files-table (make-hash-table :test #'equal))
  'ok)






(defun aw-room ()
  (format nil #"---ANTIWEB MEMORY STATS---~%~a~%~%conns and ioblocks:~%~a~%---END OF ANTIWEB MEMORY STATS---"#
    (#~s/^\s+// (#~s/\s+$//
      (with-output-to-string (*standard-output*)
        #+cmu (room nil)
        #+clisp (room)
        #+ccl (room t)
      )))
    (aw-room-antiweb-memory)))

(defun aw-room-antiweb-memory ()
  (with-output-to-string (o)
    (let ((total 0))
      (let* ((n num_conns_in_use) (b (* n (bytes_in_a_conn))))
        (incf total b)
        (format o #"  Allocated conns:     ~a, ~a bytes~%"# n b))
      (multiple-value-bind (n used-bytes) (aw-room-count-allocated-ioblocks)
        (let* ((b (* n (bytes_in_a_ioblock))) (overhead (format nil "~1$" (if (zerop used-bytes)
                                                                            0
                                                                            (* 100 (- 1 (/ used-bytes b)))))))
          (incf total b)
          (format o #"  Allocated ioblocks:  ~a, ~a bytes, ~a used, ~a% overhead~%"# n b used-bytes overhead)))
      (let* ((n (aw-room-count-conns free_conns)) (b (* n (bytes_in_a_conn))))
        (incf total b)
        (format o #"  Free conns:          ~a, ~a bytes~%"# n b))
      (let* ((n (aw-room-count-ioblocks free_ioblocks)) (b (* n (bytes_in_a_ioblock))))
        (incf total b)
        (format o #"  Free ioblocks:       ~a, ~a bytes~%"# n b))
      (format o   #"  Total:               ~a bytes + malloc overhead"# total))))

(defun aw-room-count-ioblocks (b)
  (let ((n 0))
    (loop until (cffi:null-pointer-p b) do
      (cffi:with-foreign-slots ((next) b ioblock)
        (incf n)
        (setq b next)))
    n))

(defun aw-room-count-conns (c)
  (let ((n 0))
    (loop until (cffi:null-pointer-p c) do
      (cffi:with-foreign-slots ((next) c conn)
        (incf n)
        (setq c next)))
    n))

(defun aw-room-count-allocated-ioblocks ()
  (let ((c conns_in_use) (n 0) (used-bytes 0))
    (loop until (cffi:null-pointer-p c) do
      (cffi:with-foreign-slots ((next in out inlen outlen) c conn)
        (incf n (aw-room-count-ioblocks in))
        (incf n (aw-room-count-ioblocks out))
        (incf used-bytes inlen)
        (incf used-bytes outlen)
        (setq c next)))
    (values n used-bytes)))



(defun aw-lookup-user-name-with-getpwnam (name)
  (cond
    ((and (integerp name) (<= 1 name 65534))
       name)
    ((stringp name)
       (cffi:with-foreign-string (fstr name)
         (aw_lookup_user_name_with_getpwnam fstr)))
    (t
       (error "bad value for name: ~a" name))))

(defun aw-chmod (path mode)
  (cffi:with-foreign-string (fstr path)
    (aw_chmod fstr mode)))

(defun aw-mkdir-dash-p (path)
  (cffi:with-foreign-string (fstr path)
    (aw_mkdir_dash_p fstr)))

(defun aw-daemonise-drop-terminal ()
  #+cmu (progn (close *terminal-io*)
               (setf *terminal-io* (open "/dev/null" :direction :io :if-exists :supersede)))
  (aw_daemonise_drop_terminal))

;; FIXME: would be nice to do this without eval for efficiency's sake
(defun aw-fork-and-exec (path &rest args)
  (let ((val (aw-fork)) out)
    (if (= val -1)
      (fatal "aw-fork-and-exec: unable to fork"))
    (when (zerop val)
      (push 'aw-execl out)
      (push path out)
      (push path out)
      (loop for a in args do
        (push :string out)
        (push a out))
      (push :pointer out)
      (push (cffi:null-pointer) out)
      (eval (nreverse out))
      (quit)))) ; should never get here





(defun aw-n-bit-session-id (bits)
  (let ((*print-base* 36))
    (string-downcase (format nil "~a" (isaac:rand-bits aw-isaac-ctx bits)))))

(defun aw-sha1 (input &optional (iters 1))
  (cffi:with-foreign-string (fstr input)
    (cffi:with-foreign-pointer (fstr2 20)
      (aw_sha1 fstr (length input) fstr2 iters)
      (format nil "~{~2,'0x~}"
                  (loop for i from 0 below 20 collect (cffi:mem-aref fstr2 :uchar i))))))

(let ((password-iterations 1000))
  (defun aw-hash-password (clear-text)
    (let ((salt (isaac:rand32 aw-isaac-ctx))
          (*print-base* 10))
      (format nil "~a,~a" salt (aw-sha1 (format nil "~a,~a" salt clear-text) password-iterations))))

  (defun aw-compare-password (clear-text hashed-password)
    (let ((*print-base* 10))
      (if-match (#~m/^(\d+),(\w+)$/ hashed-password)
        (string= (aw-sha1 (format nil "~a,~a" $1 clear-text) password-iterations) $2)
        (error "bad value for hashed-password")))))




(define-condition bdb-deadlock (condition) ())

(defvar aw-bdb-open-dbs (make-hash-table :test #'equal))

(defvar aw-bdb-transaction-stack nil)

(defun aw-bdb-version ()
  (cffi:convert-from-foreign (aw_bdb_version) :string))

(defun aw-bdb-init-environment (db-dir)
  (cffi:with-foreign-string (fstr db-dir)
    (aw_bdb_init_environment fstr)))

(defun aw-bdb-open (db-name)
  (or
    (gethash db-name aw-bdb-open-dbs)
    (progn
      (unless (#~m/^[\w-_.]+$/ db-name) (error "bad character in DB name"))
      (cffi:with-foreign-string (fstr db-name)
        (setf (gethash db-name aw-bdb-open-dbs) (aw_bdb_open fstr))
        (gethash db-name aw-bdb-open-dbs)))))

(defun aw-bdb-lowlevel-put (db-name key data)
  (let ((db-ptr (aw-bdb-open db-name)))
    (cffi:with-foreign-string (key-fstr key)
      (cffi:with-foreign-string (data-fstr data)
        (let ((ret (aw_bdb_put db-ptr
                               (or (car aw-bdb-transaction-stack)
                                   (error "aw-bdb-lowlevel-put outside transaction"))
                               key-fstr (length key)
                               data-fstr (length data))))
          (if (= ret 2)
            (error 'bdb-deadlock)))))))

(defun aw-bdb-lowlevel-get-copy-to-lisp-string (ptr-ptr len-ptr)
  (let* ((data-len (cffi:mem-ref len-ptr :int))
         (data-ptr (cffi:mem-ref ptr-ptr :pointer))
         (data-str (make-string data-len)))
    (fast-progn
      (loop for i from 0 below data-len do
        (setf (aref data-str i) (code-char (cffi:mem-aref data-ptr :uchar i)))))
    data-str))

(defun aw-bdb-lowlevel-get (db-name key db-rmw)
  (let ((db-ptr (aw-bdb-open db-name)))
    (cffi:with-foreign-string (key-fstr key)
      (cffi:with-foreign-object (data-ptr-ptr :pointer 1)
        (cffi:with-foreign-object (data-len-ptr :int 1)
          (let ((ret (aw_bdb_get db-ptr
                                 (or (car aw-bdb-transaction-stack) (error "aw-bdb-lowlevel-get outside transaction"))
                                 db-rmw
                                 key-fstr (length key)
                                 data-ptr-ptr data-len-ptr)))
            (ecase ret
              ((0) (aw-bdb-lowlevel-get-copy-to-lisp-string data-ptr-ptr data-len-ptr))
              ((1) nil)
              ((2) (error 'bdb-deadlock)))))))))

(defun aw-bdb-key-segments-to-key (key-segs)
  (with-output-to-string (o)
    (loop for s in key-segs do
      (let ((key (etypecase s
                   (string s)
                   (integer (format nil "~a" s)))))
        (if (find (code-char 0) key)
          (error "BDB key segments can't contain nul characters"))
        (format o "~a~a" 
          key (code-char 0))))))

(defun bdb-get (db-name &rest key-segs)
  (let* ((key (aw-bdb-key-segments-to-key key-segs))
         (str (aw-bdb-lowlevel-get db-name key 0)))
    (if (null str)
      (values nil nil)
      (values (read-from-string str) t))))

;; not setfable
(defun aw-bdb-get-with-write-lock (db-name &rest key-segs)
  (let* ((key (aw-bdb-key-segments-to-key key-segs))
         (str (aw-bdb-lowlevel-get db-name key 1)))
    (if (null str)
      (values nil nil)
      (values (read-from-string str) t))))

(defsetf bdb-get (db-name &rest key-segs) (val)
  (let ((g!val (gensym "val")))
    `(let ((,g!val (format nil "~S" ,val)))
       (aw-bdb-lowlevel-put ,db-name (aw-bdb-key-segments-to-key (list ,@key-segs)) ,g!val)
       ,val)))

;; Returns t if the item was in the DB, nil otherwise (same as remhash)
(defun bdb-del (db-name &rest key-segs)
  (let ((db-ptr (aw-bdb-open db-name))
        (key (aw-bdb-key-segments-to-key key-segs)))
    (cffi:with-foreign-string (key-fstr key)
      (let ((ret (aw_bdb_del db-ptr
                             (or (car aw-bdb-transaction-stack) (error "bdb-del outside transaction"))
                             key-fstr (length key))))
        (ecase ret
          ((0) t)
          ((1) nil)
          ((2) (error 'bdb-deadlock)))))))


;; should return a value or else transaction will be aborted
;; forms inside with-bdb-transaction should have no side effects other than writing to the DB
(defmacro! with-bdb-transaction (&rest body)
  `(let (,g!ret ,g!abort-to-parent-transaction)
     (tagbody
       ,g!retry-transaction
       (if ,g!abort-to-parent-transaction
         (error 'bdb-deadlock))
       (let (,g!do-commit
             (aw-bdb-transaction-stack (cons (aw_bdb_begin_transaction
                                               (or (car aw-bdb-transaction-stack) (cffi:null-pointer)))
                                             aw-bdb-transaction-stack)))
         (handler-bind ((bdb-deadlock (lambda (c)
                                        (declare (ignore c))
                                        (when (cdr aw-bdb-transaction-stack)
                                          (setq ,g!abort-to-parent-transaction t))
                                        (go ,g!retry-transaction))))
           (unwind-protect
             (progn
               (setq ,g!ret (progn ,@body))
               (setq ,g!do-commit t))
             (if ,g!do-commit
               (aw_bdb_commit_transaction (car aw-bdb-transaction-stack))
               (aw_bdb_abort_transaction (car aw-bdb-transaction-stack)))))))
     ,g!ret))




(defun bdb-get-serialised-version-of-key (key)
  (let ((segs (cl-ppcre:split #"\0"# key)))
    (mapcar (lambda (s)
              (if (#~m/^\d+$/ s)
                (parse-integer s)
                s))
            segs)))

(defun bdb-get-all (db-name &key key-filter just-keys)
  (let* (accumulator
         (db-ptr (aw-bdb-open db-name))
         (cursor (aw_bdb_open_cursor db-ptr (or (car aw-bdb-transaction-stack) (error "bdb-get-all outside transaction")))))
    (unwind-protect
      (cffi:with-foreign-object (key-ptr-ptr :pointer 1)
        (cffi:with-foreign-object (key-len-ptr :int 1)
          (cffi:with-foreign-object (data-ptr-ptr :pointer 1)
            (cffi:with-foreign-object (data-len-ptr :int 1)
              (loop do
                (let ((ret (aw_bdb_cursor_next cursor key-ptr-ptr key-len-ptr data-ptr-ptr data-len-ptr)))
                  (ecase ret
                    ((1) (return))
                    ((2) (error 'bdb-deadlock))
                    ((0) (let ((key (aw-bdb-lowlevel-get-copy-to-lisp-string key-ptr-ptr key-len-ptr)))
                           (if (or (null key-filter) (funcall key-filter key))
                             (let ((data (if (not just-keys)
                                           (aw-bdb-lowlevel-get-copy-to-lisp-string data-ptr-ptr data-len-ptr))))
                               (push (if just-keys
                                       (bdb-get-serialised-version-of-key key)
                                       (cons (bdb-get-serialised-version-of-key key) (if data (read-from-string data))))
                                     accumulator))))))))))))
      (aw_bdb_close_cursor cursor))
    (nreverse accumulator)))



#|
;; IMPORTANT NOTES about with-bdb-checkout:
;; * Use :write-out t for changes to your checked out item to be saved to BDB
;; * accessor may be evaluated more than once--don't use forms with side-effects

(with-bdb-checkout (u ("users.db" "doug"))
  ...)

(with-bdb-checkout (u ("users.db" "doug") :write-out t)
  ...
  (setf (user-firstname u) "Douglas")
  (setf (user-lastname u) "Hoyte")
  ...)
|#

;; FIXME: write-out done in an unwind-protect? This would allow write-outs to happen even if leaving
;; the checkout form non-locally... Not sure if this is desirable. For now just don't do this.
(defmacro! with-bdb-checkout ((sym accessor &key write-out) &rest body)
  `(let (,g!ret)
     (let ((,sym (,(if write-out 'aw-bdb-get-with-write-lock 'bdb-get) ,@accessor)))
        (setq ,g!ret (progn ,@body))
        ,(if write-out
           `(setf (bdb-get ,@accessor) ,sym)))
     ,g!ret))



;; Binds anaphor key
(defmacro! bdb-iterate ((sym accessor &key write-out) &rest body)
  `(let ((,g!keys (bdb-get-all ,(car accessor) :just-keys t)))
     (loop for key in ,g!keys do
       (block ,g!block
         (when (= ,(length (cdr accessor)) (length key))
           (let ,(remove-if-not #'symbolp (cdr accessor))
             ,@(loop for i from 0
                     for a in (cdr accessor)
                     collect (if (symbolp a)
                               `(setq ,a (nth ,i key))
                               `(unless (equal ,a (nth ,i key))
                                  (return-from ,g!block nil))))
             (with-bdb-checkout (,sym ,accessor :write-out ,write-out)
               ,@body)))))))


(defmacro! bdb-collect (&rest accessor)
  `(let (,g!results)
     (bdb-iterate (,g!sym (,@accessor))
       (push (cons key ,g!sym) ,g!results))
     (nreverse ,g!results)))




;; lenient-struct is so we can read in and print out structs even without a corresponding defstruct
(defclass lenient-struct ()
  ((contents :accessor lenient-struct-contents
             :initform nil
             :initarg :contents)))

(defmethod print-object ((l lenient-struct) (s stream))
  (format s "#S~S" (lenient-struct-contents l)))

(defmacro! with-lenient-struct-reader (&rest body)
  `(let ((*readtable* (copy-readtable *readtable*)))
     (let ((,g!read-handler (lambda (stream sub-char numarg)
                              (declare (ignore sub-char numarg))
                              (unless (char= #\( (read-char stream)) ;)
                                (error "bad lenient-struct form")) ;(
                              (make-instance 'lenient-struct :contents (read-delimited-list #\) stream t)))))
       (set-dispatch-macro-character #\# #\s ,g!read-handler)
       (set-dispatch-macro-character #\# #\S ,g!read-handler)
       ,@body)))









;; Don't use these buffers to store anything between closure invocations. They *will* be overwritten.
(defvar shared-input-buffer
  (make-array AW_MAX_MSG_LENGTH :element-type 'base-char :fill-pointer t))
(defvar shared-output-buffer
  (make-array AW_MAX_MSG_LENGTH :element-type 'base-char :fill-pointer t))


;; Any data from str must be UTF-8 decoded before performing character processing
;; Str is populated with bytes in range 0..255
(defun read-from-conn-into-string (c str n)
  (declare (type fixnum n) (type string str))
  (if (> n (array-dimension str 0))
    (error "str string too small to hold ~a" n))
  (setf (fill-pointer str) n)
  (cffi:with-foreign-slots ((in) c conn)
    (let ((p in) (i 0))
      (declare (type fixnum i))
      #f
      (loop while (> n 0) do
        (cffi:with-foreign-slots ((offset len data next) p ioblock)
          (loop for j from 0 below (min len n) do
            (setf (aref str i) (code-char (cffi:mem-aref data :uchar (+ offset (the fixnum j)))))
            (incf i))
          (decf n len)
          (setq p next))))))


;; Must be UTF-8 encoded before calling this function
;; Assumes every character's code is in range 0..255 (and logands over ones that aren't)
(defun write-to-conn-from-string (c str)
  (declare (type string str))
  (let ((i 0) (end (safe-progn (length str))))
    (declare (type fixnum i) (type fixnum end))
    #f
    (loop while (< i end) do
      (prealloc_ioblock)
      (let ((p free_ioblocks) (j 0))
        (declare (type fixnum j))
        (cffi:with-foreign-slots ((offset len data next) p ioblock)
          (loop while (and (< i end) (< j AW_IOBLOCK_SIZE)) do
            (setf (cffi:mem-aref data :uchar j)
                  (the (unsigned-byte 8) (logand (char-code (aref str i)) #xFF)))
            (incf i)
            (incf j))
          (setf len j)
          (setf offset 0)
          (safe-progn (setf free_ioblocks next))
          (cffi:with-foreign-slots ((out outp outlen) c conn)
            (safe-progn (incf outlen j)) ; (signed-byte 64)
            (if (cffi:null-pointer-p out)
              (setf out p  outp p  next (cffi:null-pointer))
              (setf (cffi:foreign-slot-value outp 'ioblock 'next) p
                    outp p
                    next (cffi:null-pointer))))))))
  (aw_event_update c))

(defun copy-lisp-string-to-c-string-no-bounds-check-yes-really (lstr cptr)
  (declare (type string lstr))
  #f
  (let ((len (safe-progn (length lstr))))
    (declare (type fixnum len))
    (loop for i from 0 below len do
      (setf (cffi:mem-aref cptr :uchar (the fixnum i))
          (the (unsigned-byte 8) (logand (char-code (aref lstr i)) #xFF))))
    (setf (cffi:mem-aref cptr :uchar len) (the (unsigned-byte 8) 0))))




(defun add-to-conn-table (conn handler-closure)
  (if (gethash (cffi:pointer-address conn) conn-table)
    (fatal "add-to-conn-table: tried to add already existing connection"))
  (setf (gethash (cffi:pointer-address conn) conn-table) handler-closure))




(defun event-loop ()
(let (*read-eval*)
  (loop do
    (let ((c (aw_get_event)))
      (cffi:with-foreign-slots ((conntype outlen sd ready) c conn)
        (if (and (= conntype AW_CONNTYPE_ZOMBIE)
                 (or (= outlen 0) (= sd -1)))
          (progn
            (unless (remhash (cffi:pointer-address c) conn-table)
              (fatal "event-loop tried to remove non-existing zombie"))

            (when (gethash (cffi:pointer-address c) worker-conn-table)
              (remhash (cffi:pointer-address c) worker-conn-table)
              (remhash (cffi:pointer-address c) locked-worker-table)
              (loop for k being the hash-keys in host-to-conn-dispatch-table using (hash-value v) do
                (if (= (cffi:pointer-address c) v)
                  (remhash k host-to-conn-dispatch-table))))

            (when (gethash (cffi:pointer-address c) inet-conn-table)
              (remhash (cffi:pointer-address c) inet-conn-table))

            (aw_unalloc_conn c))
          (progn
            (loop while (and (> ready 0)
                             (not (= conntype AW_CONNTYPE_ZOMBIE))
                             (not (= conntype AW_CONNTYPE_HTTP_LINGER))) do
              (setf (gethash (cffi:pointer-address c) conn-table)
                (let ((f (gethash (cffi:pointer-address c) conn-table)))
                  (unless (functionp f)
                    (fatal "event-loop: ~a is not a function (conntype ~a)" f conntype))
                  (funcall f))))
            (aw_event_update c))))))))







(eval-when (:execute :load-toplevel :compile-toplevel)
  (defun code-alpha-lookup (c)
    (case c
      ((200) "OK")
      ((206) "Partial Content")
      ((301) "Moved Permanently")
      ((302) "Found")
      ((304) "Not Modified")
      ((307) "Temporary Redirect")
      ((400) "Bad Request")
      ((403) "Forbidden")
      ((404) "Not Found")
      ((405) "Method Not Allowed")
      ((411) "Length Required")
      ((413) "Request Entity Too Large")
      ((500) "Internal Server Error")
      (t (error "Unknown code: ~a" c)))))

;; Expands into a constant string
(defmacro expand-to-http-err-msg (code code-alpha desc &key close)
  (let ((body (format nil "<html><body><title>~a ~a</title><h1>~a ~a</h1><h2>~a</h2><br><hr>Antiweb ~a</body></html>"
                          code code-alpha code code-alpha desc AW_VERSION)))
    (format nil "HTTP/1.1 ~a ~a~aServer: Antiweb/~a~a~aContent-Type: text/html; charset=utf-8~aContent-Length: ~a~a~a~a"
                code code-alpha crlf AW_VERSION crlf
                (if close (format nil "Connection: close~a" crlf) "")
                crlf (length body) crlf crlf body)))

(defmacro send-err-to-conn (c code code-alpha desc &rest keys)
  `(write-to-conn-from-string ,c
     (expand-to-http-err-msg ,code ,code-alpha ,desc ,@keys)))

(defun linger-this-http-conn (c)
  (remove_from_artificial_ready_conns c)
  (cffi:with-foreign-slots ((conntype ready) c conn)
    (setf conntype AW_CONNTYPE_HTTP_LINGER)
    (setf ready 0)))

(defmacro! send-http-err-and-linger (o!c code desc)
  `(progn
     (linger-this-http-conn ,g!c)
     (send-err-to-conn ,g!c ,code ,(code-alpha-lookup code) ,desc :close t)))

(defmacro! send-http-err-and-keepalive (o!c code desc)
  `(progn
     (send-err-to-conn ,g!c ,code ,(code-alpha-lookup code) ,desc)))




(defun hub-start-inet-listener (bind-addr port &optional existing-conn)
  (let ((c (or existing-conn
               (cffi:with-foreign-string (fstr bind-addr)
                 (aw_listen_inet fstr port)))))
    (setf (gethash (cffi:pointer-address c) inet-conn-table) (list bind-addr port))
    (add-to-conn-table c
      (fsm hub-inet-main
        (hub-accept-http-connection c)
        hub-inet-main))))


(defun hub-close-inet-listener (bind-addr port)
  (let (done (q (list bind-addr port)))
    (loop for k being the hash-keys in inet-conn-table using (hash-value v) do
      (when (equalp q v)
        (setf done t)
        (let ((c (cffi:make-pointer k)))
          (cffi:with-foreign-slots ((conntype) c conn)
            (aw-log () "closed inet listener ~a ~a" bind-addr port)
            (setf conntype AW_CONNTYPE_ZOMBIE)
            (aw_touch_conn c 0)
            (remhash k inet-conn-table)))))
    (if done 'ok 'listener-not-found)))




(declaim (notinline hub-rewrite-host))

;; can override in hub.conf
(defun hub-rewrite-host (http-headers)
  (declare (ignore http-headers))
  nil)

(defun install-hub-rewrite-host (lambda-form)
  (setf (symbol-function 'hub-rewrite-host) (compile nil lambda-form)))

;; Shortest host ::1   FIXME technically should allow [::1]:80
;; Hub only looks at host, doesn't care about path.
;; All changes must be made in worker handler code too.
(defun hub-accept-http-connection (c)
  (let ((c (aw_accept_conn c 0 AW_CONNTYPE_HTTP)))
    (unless (cffi:null-pointer-p c)
      (incf hub-stats-total-conns)
      (add-to-conn-table c
        (fsm hub-accept-http-main
          (cffi:with-foreign-slots ((ready) c conn)
            (read-from-conn-into-string c shared-input-buffer ready)
            (if-match (#~m=^(?:GET|POST|HEAD) /.*?\r\n[Hh][Oo][Ss][Tt]: ([\w-.:]{3,100})\r\n=s shared-input-buffer)
              (progn
                (setq $1 (or (funcall 'hub-rewrite-host shared-input-buffer) $1))
                (let ((dest-conn (gethash $1 host-to-conn-dispatch-table)))
                  (if dest-conn
                    (progn
                      (incf hub-stats-dispatched-conns)
                      (aw_send_conn c (cffi:make-pointer dest-conn) (cffi:null-pointer) 0)
                      'conn-sent)
                    (progn
                      (send-http-err-and-linger c 404 "Virtual host not registered")
                      'host-not-found))))
              (progn
                (send-http-err-and-linger c 400 "Valid HTTP/1.1 required")
                'bad-request))))))))







(defun hub-start-unix-listener (path)
  (unless (zerop (aw-getuid)) (error "only root can listen on unix sockets"))
  (let ((c (cffi:with-foreign-string (fstr path)
             (aw_listen_unix fstr))))
    (add-to-conn-table c
      (fsm hub-unix-listener
        (hub-accept-unix-connection c)
        hub-unix-listener))
        (aw-chmod path #b111000000)))

(defun lookup-conn-pointer-from-worker-name (w)
  (loop for k being the hash-keys in worker-conn-table using (hash-value v) do
    (if (eq w v)
      (return-from lookup-conn-pointer-from-worker-name k)))
  nil)

(defun unlock-worker (name)
  (remhash (lookup-conn-pointer-from-worker-name name) locked-worker-table))

(defun lock-worker (name)
  (setf (gethash (lookup-conn-pointer-from-worker-name name) locked-worker-table) t))



(defmacro with-aw-repl-bindings (&rest body)
  `(let (tp* tp** tp*** tp+ tp++ tp+++)
     ,@body))


;; Must be surrounded by (cffi:with-foreign-slots ((ready limit sep) c conn) ... )
(defmacro! read-fixed-length-message-from-conn-and-store-in-shared-input-buffer (o!len &rest body)
  `(let ((,g!orig-limit limit)
         (,g!orig-sep sep))
     (setf limit ,g!len)
     (setf sep (cffi:null-pointer))
     (aw_update_conn_ready_status c)
     (fsm ,g!reading-fixed-message
       (read-from-conn-into-string c shared-input-buffer ready)
       (aw_drop_n_input_bytes c ready)
       (setf limit ,g!orig-limit)
       (setf sep ,g!orig-sep)
       (aw_update_conn_ready_status c)
       ,@body)))



;; Must be surrounded by (cffi:with-foreign-slots ((ready limit sep) c conn) ... )
(defmacro handle-eval-returning-closure-or-nil-if-eval-msg-not-in-shared-input-buffer (closure)
  `(when-match (#~m/^eval (\d+)\n$/ shared-input-buffer)
     (read-fixed-length-message-from-conn-and-store-in-shared-input-buffer (parse-integer $1)
       (let (form-to-eval eval-result)
         (let ((* tp*) (** tp**) (*** tp***) (+ tp+) (++ tp++) (+++ tp+++))
           (setq form-to-eval (read-from-string shared-input-buffer))
           (setq eval-result
             (block eval-result-block
               (handler-bind ((error (lambda (condition)
                                       (return-from eval-result-block
                                                    (list '#:err (#~s/[\r\n\t]/ / (format nil "ERROR ~a" condition)))))))
                 (return-from eval-result-block (eval form-to-eval)))))
           (setq tp*** tp**  tp** tp*  tp* eval-result)
           (setq tp+++ tp++  tp++ tp+  tp+ form-to-eval))
         (let ((result-as-str (format nil "~S" eval-result)))
           (write-to-conn-from-string c
             (format nil "eval-result ~a~%~a" (length result-as-str) result-as-str))))
       ,closure)))




(defun hub-accept-unix-connection (tpc)
  (let ((c (aw_accept_unix_conn tpc)))
   (with-aw-repl-bindings
    (add-to-conn-table c
      (fsm hub-unix-handler
        (cffi:with-foreign-slots ((ready limit sep conntype) c conn)
          (read-from-conn-into-string c shared-input-buffer ready)
          (aw_drop_n_input_bytes c ready)
          (or
            (when-match (#~m/^(check-|)worker ([\w._-]+)\n$/ shared-input-buffer) ;; no turning back for workers
              (let ((worker-name (intern (string-upcase $2))))
                (if (or (null worker-name) (eq worker-name 'hub) (lookup-conn-pointer-from-worker-name worker-name))
                  (progn
                    (if (zerop (length $1))
                      (aw-log () "worker ~a already registered" $2))
                    (setf conntype AW_CONNTYPE_ZOMBIE)
                    (aw_touch_conn c 0)
                    'worker-already-regged)
                  (progn
                    (setf (gethash (cffi:pointer-address c) worker-conn-table) (read-from-string $2))
                    hub-unix-handler))))
            (when (gethash (cffi:pointer-address c) worker-conn-table) ;; worker cmds
              (or
                (when-match (#~m/^lock\n$/ shared-input-buffer)
                  (setf (gethash (cffi:pointer-address c) locked-worker-table) t)
                  hub-unix-handler)
                (when-match (#~m/^register-host ([\w.:_-]+)\n$/ shared-input-buffer)
                  (if (gethash (cffi:pointer-address c) locked-worker-table)
                    (progn
                      (aw-log () "locked worker ~a tried to register-host ~a"
                                  (gethash (cffi:pointer-address c) worker-conn-table) $1)
                      (setf conntype AW_CONNTYPE_ZOMBIE)
                      (aw_touch_conn c 0)
                      'connection-was-locked)
                    (if (gethash $1 host-to-conn-dispatch-table)
                      (progn
                        (aw-log () "worker ~a tried to register-host ~a but this is already registered"
                                    (gethash (cffi:pointer-address c) worker-conn-table) $1)
                        (setf conntype AW_CONNTYPE_ZOMBIE)
                        (aw_touch_conn c 0)
                        'already-registered)
                      (progn
                        (setf (gethash $1 host-to-conn-dispatch-table) (cffi:pointer-address c))
                        hub-unix-handler))))
                (when-match (#~m/^unregister-host ([\w.:_-]+)\n$/ shared-input-buffer)
                  (if (eql (gethash $1 host-to-conn-dispatch-table) (cffi:pointer-address c))
                    (remhash $1 host-to-conn-dispatch-table))
                  hub-unix-handler)
                (when-match (#~m/^log ([\w-_.]+) (\d+)\n$/ shared-input-buffer)
                  (read-fixed-length-message-from-conn-and-store-in-shared-input-buffer (parse-integer $2)
                    (let ((worker-name (symbol-name
                                         (gethash (cffi:pointer-address c) worker-conn-table))))
                      (aw-log (:log-file $1 :prefix (format nil "~a " worker-name)) "~a" shared-input-buffer))
                    hub-unix-handler))))
            (when (not (gethash (cffi:pointer-address c) worker-conn-table)) ;; cmds NOT available to workers
              (or
                (handle-eval-returning-closure-or-nil-if-eval-msg-not-in-shared-input-buffer hub-unix-handler)
                (when-match (#~m/^logger\n$/ shared-input-buffer)
                  (unless (cffi:null-pointer-p logger_conn)
                    (fatal "second logger process tried to connect"))
                  (setf logger_conn c)
                  (aw-log () "logger process connected to hub")
                  (fsm logger-main
                    (fatal "got a message from logger process")
                    logger-main))
                (when-match (#~m/^transfer ([\w-_.]+)\n$/ shared-input-buffer)
                  (let ((p (lookup-conn-pointer-from-worker-name (read-from-string $1))))
                    (if (null p)
                      (progn
                        (aw-log () "tried to transfer to unknown worker: ~a" $1)
                        (setf conntype AW_CONNTYPE_ZOMBIE)
                        (aw_touch_conn c 0)
                        'no-such-worker-to-transfer-to)
                      (progn
                        (aw_send_conn c (cffi:make-pointer p) (cffi:null-pointer) 0)
                        'sent-unix-conn-to-worker))))
                (when-match (#~m/^add-listener ([\w:.]+) (\d+)\n$/ shared-input-buffer)
                  (hub-start-inet-listener $1 (parse-integer $2) (aw_accept_conn c 0 AW_CONNTYPE_INETLISTENER))
                  (write-to-conn-from-string c (format nil "ok~%"))
                  (aw-log () "adding inet listener ~a ~a" $1 $2)
                  hub-unix-handler)))
            (progn
              (aw-log () "bad connection to hub")
              (setf conntype AW_CONNTYPE_ZOMBIE)
              (aw_touch_conn c 0)
              'bad-cmd-from-connection-to-hub-socket))))))))



(defvar aw-open-log-files (make-hash-table :test 'equal))

(defun reopen-log-files ()
  (if (and (not (cffi:null-pointer-p hub_conn))
           (not (cffi:null-pointer-p logger_conn)))
    (error "reopen-log-files: worker can't reopen log files"))
  (if (not (cffi:null-pointer-p logger_conn)) ; hub
    (write-to-conn-from-string logger_conn
      (format nil "reopen-log-files~%"))
    (progn ; logger
      (loop for v being the hash-values in aw-open-log-files do
        (aw_close_log_file v))
      (setf aw-open-log-files (make-hash-table :test 'equal)))))

(defun get-log-file (logfile)
  (let ((f (gethash logfile aw-open-log-files)))
    (if f
      (return-from get-log-file f))
    (cffi:with-foreign-string (fstr (format nil "/~a" logfile))
      (setf f (aw_open_log_file fstr))
      (let ((stat (aw_lstat_returning_a_static_struct fstr))
            (logger-uid (aw-lookup-user-name-with-getpwnam (conf-get aw-hub-conf 'logger-uid))))
        (unless (cffi:null-pointer-p stat)
          (unless (zerop (aw_stat_is_sym_link stat))
            (fatal "~a/aw_log/~a can't be a symlink" aw-hub-dir logfile))
          (if (zerop (aw_stat_is_reg_file stat))
            (fatal "~a/aw_log/~a must be a file" aw-hub-dir logfile))
          (unless (= logger-uid (aw_stat_get_uid stat))
            (fatal "~a/aw_log/~a not owned by hub user" aw-hub-dir logfile))
          (unless (= logger-uid (aw_stat_get_gid stat))
            (fatal "~a/aw_log/~a in different group from hub" aw-hub-dir logfile))))
      (setf (gethash logfile aw-open-log-files) f)
      (aw-chmod logfile #b110000000)
      f)))


(defun logger-setup-hub-conn ()
  (let ((c (cffi:with-foreign-string (fstr (format nil "~a/hub.socket" aw-hub-dir))
             (aw_conn_unix fstr))))
    (setf hub_conn c)
    (write-to-conn-from-string c
      (format nil "logger~%"))
    (add-to-conn-table c
      (fsm logger-main
        (cffi:with-foreign-slots ((ready limit sep conntype) c conn)
          (read-from-conn-into-string c shared-input-buffer ready)
          (aw_drop_n_input_bytes c ready)
          (or
            (when-match (#~m/^reopen-log-files\n$/ shared-input-buffer)
              (reopen-log-files)
              logger-main)
            (when-match (#~m/^log ([\w-_.]+) (\d+)\n$/ shared-input-buffer)
              (read-fixed-length-message-from-conn-and-store-in-shared-input-buffer (parse-integer $2)
                (cffi:with-foreign-string (fstr shared-input-buffer)
                  (aw_write_log_message (get-log-file $1) fstr))
                logger-main))
            (fatal "bad message from hub")))))))



(defun worker-unix-connect (checking)
  (let ((c (cffi:with-foreign-string (fstr (format nil "~a/hub.socket" aw-hub-dir))
             (aw_conn_unix fstr))))
    (setf hub_conn c)
    (setf logger_conn c)
    (write-to-conn-from-string c
      (format nil "~aworker ~a~%~{register-host ~a~%~}lock~%" 
        (if checking "check-" "")
        (conf-get aw-worker-conf 'worker)
        (flatten
          (mapcar (lambda (h) (xconf-get h :hosts))
                  (conf-get-all aw-worker-conf 'handler)))))
    (aw-log () "worker connected")
    (add-to-conn-table c
      (fsm worker-unix
        (cffi:with-foreign-slots ((ready sep currsep limit) c conn)
          (read-from-conn-into-string c shared-input-buffer ready)
          (aw_drop_n_input_bytes c ready)
          (or
            (if-match (#~m/^http (\d+) ([\d:.]{3,46})\n$/ shared-input-buffer)
              (let ((len (parse-integer $1)))
                (let ((c (aw_accept_conn c len AW_CONNTYPE_HTTP)))
                  (cffi:with-foreign-slots ((ip) c conn)
                    (copy-lisp-string-to-c-string-no-bounds-check-yes-really $2 ip)) ; ip has INET6_ADDRSTRLEN+1=47
                  (add-to-conn-table c
                    (worker-handle-http c)))
                worker-unix))
            (if-match (#~m/^supervise (\d+)\n$/ shared-input-buffer)
              (let ((len (parse-integer $1)))
                (let ((c (aw_accept_conn c len AW_CONNTYPE_UNIX)))
                  (add-to-conn-table c
                    (worker-handle-supervise c)))
                worker-unix))
            (cffi:with-foreign-slots ((conntype) c conn)
              (aw_touch_conn c 0) ; FIXME fatal? bad command from hub means something really bad has happened
              (setf conntype AW_CONNTYPE_ZOMBIE)
              'bad-cmd-from-hub)))))))



(defun worker-handle-supervise (c)
  (with-aw-repl-bindings
    (fsm worker-supervise
      (cffi:with-foreign-slots ((ready limit sep conntype) c conn)
        (read-from-conn-into-string c shared-input-buffer ready)
        (aw_drop_n_input_bytes c ready)
        (or
          (handle-eval-returning-closure-or-nil-if-eval-msg-not-in-shared-input-buffer worker-supervise)
          (progn
            (aw_touch_conn c 0)
            (setf conntype AW_CONNTYPE_ZOMBIE)
            'bad-cmd-in-worker-supervise))))))


(defun worker-handle-http (c)
  (incf worker-stats-total-conns)
  (fsm worker-http
    (cffi:with-foreign-slots ((ready) c conn)
      (read-from-conn-into-string c shared-input-buffer ready)
      (aw_drop_n_input_bytes c ready)
      (let (fatal-condition)
        (or
          (ignore-errors
            (handler-bind ((error (lambda (condition)
                                    (if aw-rollbacks
                                      (progn
                                        (rollback condition)
                                        (send-http-err-and-linger c 500 "http-user-dispatch rollback. see syslog"))
                                      (setq fatal-condition condition)))))
              (incf worker-stats-total-requests)
              (http-user-dispatch worker-http c shared-input-buffer)))
          (if fatal-condition
            (fatal "error in http user dispatch: ~a" fatal-condition)
            'rolled-back-http-user-dispatch))))))





(defun add-this-antiweb-module (m)
  (let ((existing (member (cadr m) all-antiweb-modules :key #'cadr)))
    (if existing
      (setf (car existing) m)
      (setf all-antiweb-modules (nconc all-antiweb-modules (list m))))))

(defmacro antiweb-module (name &rest code)
  `(add-this-antiweb-module '(antiweb-module ,name ,@code)))

(defun antiweb-module-phase (handler phase)
  `(progn
     ,@(mapcar
         (lambda (e)
           (pandoric-eval (handler) e))
         (remove-if #'null (mapcar (lambda (m) (xconf-get m phase)) all-antiweb-modules)))))

(defun $u-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 3)
       (string= (symbol-name s)
                "$U-"
                :start1 0
                :end1 3)))

(defmacro with-all-$u-symbols-in-our-sub-lexical-scope-bound-to-nil (&rest body)
  (let ((syms (remove-duplicates
                (remove-if-not #'$u-symbol-p (flatten body)))))
    `(let ,syms
       (declare (ignorable ,@syms))
       ,@body)))

(defun $h-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 3)
       (string= (symbol-name s)
                "$H-"
                :start1 0
                :end1 3)))

(defun $h-symbol-to-http-header-name (sym)
  (subseq (symbol-name sym) 3))

(defun $h-symbol-to-regexp (sym) ; s-expr regexps? no, it's a feature. supports $h-referr?er etc.
  `(lambda (s)
     (cl-ppcre:scan
       ,(format nil #"(?i)\r\n~a: ([^\x00-\x1F]+)\r\n"# ($h-symbol-to-http-header-name sym))
       s)))

(defmacro with-all-$h-symbols-in-our-sub-lexical-scope-bound-to-corresponding-http-headers-from-http-buf (&rest body)
  (let ((syms (remove-duplicates
                (remove-if-not #'$h-symbol-p (flatten body)))))
    `(let ,syms
       (declare (ignorable ,@syms))
       ,@(mapcar #`(if-match (,($h-symbol-to-regexp a1) http-buf)
                     (setq ,a1 $1))
                 syms)
       ,@body)))






;; The following 2 functions (url-decode and url-encode) were adapted
;; from Hunchentoot by Edi Weitz (http://weitz.de/hunchentoot/)

;;; Copyright (c) 2004-2008, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; Doesn't decode from UTF-8. Returns nil if url encoding was invalid
(defun url-decode (string)
(ignore-errors ; in case of "%NE"
  (let ((vector (make-array (length string)
                            :element-type 'character
                            :fill-pointer 0)))
    (loop with percent-p and buff
          for char of-type character across string
          for i from 0
          when buff do
            (vector-push (code-char (parse-integer string
                                        :start (1- i)
                                        :end (1+ i)
                                        :radix 16))
                         vector)
            (setq buff nil)
          else when percent-p
                 do (setq buff t
                          percent-p nil)
          else when (char= char #\%)
                 do (setq percent-p t)
          else do (vector-push (case char
                                 ((#\+) #\Space)
                                  (otherwise char))
                         vector))
    vector)))

;; Assumes string is UTF-8 encoded
(defun url-encode (string)
  (with-output-to-string (s)
    (loop for c across string
          for index from 0
          do (cond ((char= c #\space)
                     (write-char #\+ s))
                   ((or (char<= #\0 c #\9)
                        (char<= #\a c #\z)
                        (char<= #\A c #\Z)
                        (find c "?/&-_.'()" :test #'char=))
                     (write-char c s))
                   (t (format s "%~2,'0x" (char-code c)))))))





(defun valid-utf-8-p (str)
  (cl-ppcre:scan ; regexp adapted from: http://www.w3.org/International/questions/qa-forms-utf-8
    #.(#~s/\s// (#~s/#.*?\n//
#"\A(?:[\x09\x0A\x0D\x20-\x7E]            # ASCII
     | [\xC2-\xDF][\x80-\xBF]             # non-overlong 2-byte
     |  \xE0[\xA0-\xBF][\x80-\xBF]        # excluding overlongs
     | [\xE1-\xEC\xEE\xEF][\x80-\xBF]{2}  # straight 3-byte
     |  \xED[\x80-\x9F][\x80-\xBF]        # excluding surrogates
     |  \xF0[\x90-\xBF][\x80-\xBF]{2}     # planes 1-3
     | [\xF1-\xF3][\x80-\xBF]{3}          # planes 4-15
     |  \xF4[\x80-\x8F][\x80-\xBF]{2}     # plane 16
    )*\z"#))
    str))





(declaim (notinline http-user-dispatch))

;; Must call compile-http-user-dispatch before dispatching.
;; http-buf can be a shared buffer, so will be mutilated between closure invocations.
(defun http-user-dispatch #1=(keepalive-closure c http-buf)
  (declare (ignore . #1#))
  (error "http-user-dispatch not compiled"))

;; Must load aw-worker-conf before calling
(defun compile-http-user-dispatch ()
  (setf (symbol-function 'http-user-dispatch)
    (compile nil `(lambda (keepalive-closure c http-buf)
                    ,(http-user-dispatch-macro-macrolet-wrapper)))))

(defun http-user-dispatch-macro-macrolet-wrapper ()
  `(macrolet ((err-and-linger (code desc)
                `(progn
                   (send-http-err-and-linger c ,code ,desc)
                   (return-from http-user-dispatch-macro ,desc)))
              (err-and-keepalive (code desc)
                `(progn
                   (send-http-err-and-keepalive c ,code ,desc)
                   (if $u-keepalive
                     (return-from http-user-dispatch-macro keepalive-closure)
                     (progn
                       (linger-this-http-conn c)
                       (return-from http-user-dispatch-macro 'client-does-not-support-keepalive)))))
              (return-this-closure (closure)
                `(return-from http-user-dispatch-macro
                   (if $u-keepalive
                     ,closure
                     (progn (linger-this-http-conn c)
                            'client-does-not-support-keepalive))))
              (add-header-to-response (fmt &rest args)
                `(push (format nil ,fmt ,@args) extra-http-headers-to-add-to-response))
              (keepalive ()
                '(return-this-closure keepalive-closure))
              (send-raw (str)
                `(write-to-conn-from-string c ,str))
              (redir-close (code fmt &rest args)
                `(progn
                   (setq $u-keepalive nil)
                   (redir ,code ,fmt ,@args)))
              (redir (code fmt &rest args)
                (unless (and (integerp code) (or (= code 301) (= code 302) (= code 307)))
                  (error "bad redirect code: ~a" code))
                `(progn
                   (send-http-response-headers ,code
                     ("Location" "~a" (format nil ,fmt ,@args))
                     ("Content-Length" "0"))
                   (return-this-closure keepalive-closure)))
              (send-html-response-and-keepalive (&rest elements)
                (let ((g!body (gensym "body")))
                  `(let ((,g!body (concatenate 'string ,@elements)))
                     (send-http-response-headers 200
                       ("Content-Type" "text/html; charset=utf-8")
                       ("Content-Length" "~a" (length ,g!body)))
                       (send-raw ,g!body)
                       (keepalive)))))
     ,(http-user-dispatch-macro)))

(defun http-user-dispatch-macro ()
  `(block http-user-dispatch-macro
     (if-match (#~m=^(GET|POST|HEAD) (/[^\s?]*)[?]?([^\s?]*?) HTTP/(0[.]9|1[.]0|1[.]1).*?\r\n[Hh][Oo][Ss][Tt]: ([\w-.:]{3,100})\r\n=s http-buf)
       (let ((http-method (intern $1)) (http-path-encoded $2) (http-args $3) (http-ver $4) (http-host (string-downcase $5)))
         (when (eq http-method 'head)
           (setq http-method 'get)
           (setq http-ver "1.0"))
         (let ((http-path (url-decode http-path-encoded)))
           (if (or (null http-path)
                   (#~m=(?:/[.][^/]|/aw[_/]|[\x00-\x1F])=i http-path) ; Allows all non-control characters. UTF-8 friendly
                   (not (valid-utf-8-p http-path)))
             (err-and-linger 400 "URL path contains invalid characters"))
             ,@(mapcar #'http-user-dispatch-macro-request-handler (conf-get-all aw-worker-conf 'handler))
           (err-and-linger 404 "Virtual host not registered"))))
     (err-and-linger 400 "Valid HTTP/1.1 required")))

(defun http-hosts-to-regexp (handler)
  (let ((hosts (xconf-get handler :hosts)))
    (unless hosts
      (error "no hosts found in handler")) ; FIXME: allow this? hub just wouldn't route any conns to handler
    (unless (every #~m/^[\w-:.]+$/ hosts)
      (error "bad character in host")) ; will fuck up regexp
    `(lambda (s)
       (cl-ppcre:scan
         ,(format nil #"(?i)^(?:~{~a~^|~})$"# (mapcar #~s/[.]/[.]/ hosts))
         s))))

(defun list-of-file-extensions-to-regex (l)
  `(lambda (s)
     (cl-ppcre:scan
       ,(format nil #"(?i)^(/.*?[.](?:~{~a~^|~}))$"# l)
       s)))

(defun list-of-file-extensions-to-regex-with-path-info (l)
  `(lambda (s)
     (cl-ppcre:scan
       ,(format nil #"(?i)^(/.*?[.](?:~{~a~^|~}))(/.*|)$"# l)
       s)))

(defun http-root-translate (handler path)
  (let ((root (xconf-get handler :root))
        (simple (xconf-get handler :simple-vhost-root)))
    (unless (or root simple) (error "either a :root or a :simple-vhost-root is required"))
    (if (and root simple) (error "can't specify a :root and a :simple-vhost-root in same handler"))
    (if root
      `(format nil ,(format nil "~a~~a" root) ,path)
      `(format nil ,(format nil "~a/~~a~~a" simple) http-host ,path))))


(defun get-ip-address-from-conn (c)
  (cffi:with-foreign-slots ((ip) c conn)
    (cffi:translate-from-foreign ip :string)))

(defun worker-axslog (method host path args ip referer user-agent)
  (aw-log (:log-file "axslog") #"~a ~a ~a~a~a~a "~a" "~a""#
                                          ip method host path
                                          (if (and args (plusp (length args))) "?" "")
                                          (or args "")
                                          (#~s/"/?/ (or referer ""))
                                          (#~s/"/?/ (or user-agent ""))))

(defun http-user-dispatch-macro-request-handler (handler)
  `(when (or (,(http-hosts-to-regexp handler) http-host)
             ,(xconf-get handler :handle-all-hosts))
     (with-all-$h-symbols-in-our-sub-lexical-scope-bound-to-corresponding-http-headers-from-http-buf
       (with-all-$u-symbols-in-our-sub-lexical-scope-bound-to-nil
         (setq $u-keepalive (string= "1.1" http-ver))
         (let (extra-http-headers-to-add-to-response
               ($ip (get-ip-address-from-conn c)))

           ,(let ((val (xconf-get handler :accept-x-real-ip-from)))
              (if val
                `(when (and $h-x-real-ip
                            (string= $ip ,val)
                            (#~m/^[\d:.]{3,46}$/ $h-x-real-ip))
                   (setq $u-used-x-real-ip t)
                   (setq $ip $h-x-real-ip))))

           (worker-axslog http-method http-host http-path-encoded http-args $ip $h-referer $h-user-agent)

           (if (eq http-method 'post)
             (if (and $h-content-length (#~m/^\d{1,20}$/ $h-content-length))
               (progn (setq $u-content-length-num (parse-integer $h-content-length))
                      (if (> $u-content-length-num AW_MAX_CGI_POST_LEN)
                        (err-and-linger 413 "Content-Length is too long")))
               (err-and-linger 411 "POST method requires Content-Length")))

           ,(antiweb-module-phase handler :rewrite-phase)
           ,(antiweb-module-phase handler :fast-1x1gif-phase)
           ,(antiweb-module-phase handler :fast-files-phase)
           ,(antiweb-module-phase handler :awp-phase)
           ,(antiweb-module-phase handler :directory-handling-phase)
           ,(antiweb-module-phase handler :cgi-phase)
           ,(antiweb-module-phase handler :jsmin-phase)
           ,(antiweb-module-phase handler :cssmin-phase)
           ,(antiweb-module-phase handler :regular-file-phase)

           #+nil (error "fell through dispatch") ; unreachable unless regular-file-phase changes, thx python
          )))))



(defmacro! send-http-response-headers (code &rest headers)
  `(progn
     (setf (fill-pointer shared-output-buffer) 0)
     (with-output-to-string (,g!o shared-output-buffer)
       (fformat ,g!o
         ,(format nil "HTTP/1.1 ~a ~a~aServer: Antiweb/~a~a"
                      code (code-alpha-lookup code) crlf AW_VERSION crlf))
       (unless $u-keepalive
         (fformat ,g!o "Connection: close~a" crlf))
       ,@(mapcar
           #`(progn (fformat ,g!o "~a: " ,(car a1))
                    (fformat ,g!o ,@(cdr a1))
                    (fformat ,g!o "~a" crlf))
           headers)
       (if extra-http-headers-to-add-to-response
         (loop for i in extra-http-headers-to-add-to-response do
           (fformat ,g!o "~a~a" i crlf)))
       (fformat ,g!o "~a" crlf))
     (write-to-conn-from-string c shared-output-buffer)))


(defmacro! build-http-response (code headers &optional body)
  (unless (integerp code) (error "build-http-response code must be an integer"))
  `(let ((,g!body ,body))
     (declare (ignorable ,g!body))
     (with-output-to-string (,g!o)
       (fformat ,g!o
                ,(format nil "HTTP/1.1 ~a ~a~aServer: Antiweb/~a~a"
                         code (code-alpha-lookup code) crlf AW_VERSION crlf))
       (fformat ,g!o "Content-Length: ~a~a" (if (null ,g!body) 0 (length ,g!body)) crlf)
       ,@(mapcar
           #`(progn (fformat ,g!o "~a: " ,(car a1))
                    (fformat ,g!o ,@(cdr a1))
                    (fformat ,g!o "~a" crlf))
           headers)
       (format ,g!o "~a" crlf)
       ,(if body
          `(format ,g!o "~a" ,g!body)))))


(defmacro! www-form-bind (o!form fields &rest body)
  `(let (,@fields
         (,g!chunks (mapcar (lambda (chunk)
                              (cl-ppcre:split "=" chunk))
                            (cl-ppcre:split "&" ,g!form))))
     (dolist (,g!c ,g!chunks)
       (when (= 2 (length ,g!c))
         ,@(mapcar #`(if (string= (car ,g!c) ,(string-downcase (symbol-name a1)))
                       (setq ,a1 (url-decode (cadr ,g!c))))
                   fields)))
     ,@body))



(defconstant names-of-the-months
  #(nil "January" "February" "March" "April" "May" "June" "July" "August"
    "September" "October" "November" "December"))

(defun format-date (date)
  (multiple-value-bind (second minute hour date month year) (decode-universal-time date)
    (format nil "~2,'0d:~2,'0d.~2,'0d ~a ~a ~a" hour minute second (aref names-of-the-months month) date year)))

(defun calculate-uptime (time)
  (let ((seconds (floor (/ (- (get-internal-real-time) time) internal-time-units-per-second))))
    (let ((s (mod seconds 60))
          (m (mod (floor seconds 60) 60))
          (h (mod (floor seconds (* 60 60)) 24))
          (d (floor seconds (* 60 60 24))))
      (format nil "~ad ~ah ~am ~as" d h m s))))








(defun do-aw-init (do-fork)
  (setq aw-isaac-ctx (isaac:init-kernel-seed))
  (setq aw-start-time (get-internal-real-time))
  (ignore-errors (load-libantiweb)) ; errors ignored in case it's already loaded
  (if do-fork (aw_daemonise_fork))  ; other reason? will find out now
  (aw_init))







(defun run-hub (hub-dir &optional nodaemon)
  ;; Privileged
  (do-aw-init (not nodaemon))
  (handler-bind ((error (lambda (condition)
                          (fatal "run-hub: startup: ~a" condition))))
    (setf aw-hub-dir hub-dir)
    (let (*read-eval*)
      (setf aw-hub-conf (load-conf-from-file (format nil "~a/hub.conf" aw-hub-dir))))
    (let ((hub-uid (aw-lookup-user-name-with-getpwnam (conf-get aw-hub-conf 'hub-uid)))
          (logger-uid (aw-lookup-user-name-with-getpwnam (conf-get aw-hub-conf 'logger-uid)))
          (max-fds (conf-get aw-hub-conf 'max-fds))
          (install-hub-rewrite-host (conf-get aw-hub-conf 'install-hub-rewrite-host)))

      (if (or (not (integerp hub-uid)) (zerop hub-uid))
        (error "hub can't run as root"))
      (if (equal hub-uid logger-uid)
        (error "hub and logger processes must run under different users"))

      (cffi:with-foreign-string (pstr (format nil "~a/empty" aw-hub-dir))
        (let ((stat (aw_stat_returning_a_static_struct pstr)))
          (if (cffi:null-pointer-p stat)
            (error "empty dir doesn't exist"))
          (if (zerop (aw_stat_is_dir stat))
            (error "empty dir must be a directory"))
          (if (= hub-uid (aw_stat_get_uid stat))
            (error "empty directory is owned by hub-uid (~a)" hub-uid))
          (if (= logger-uid (aw_stat_get_gid stat))
            (error "empty directory in same group as hub-uid (~a)" hub-uid))))

      (aw-chmod (format nil "~a/empty" aw-hub-dir) #b111101101)

      (unless nodaemon (aw-daemonise-drop-terminal))

      (dolist (i (conf-get-all aw-hub-conf 'listen))
        (hub-start-inet-listener (cadr i) (caddr i)))
      (hub-start-unix-listener (format nil "~a/hub.socket" aw-hub-dir))
      (if max-fds
        (aw_set_nofile max-fds))
      (if install-hub-rewrite-host
        (install-hub-rewrite-host install-hub-rewrite-host))
      (cffi:with-foreign-string (pstr (format nil "~a/empty" aw-hub-dir))
        (aw_chroot pstr))
      (aw_dropto_uid_gid hub-uid)
      (aw_set_nproc 1)))

  ;; Unprivileged
  (handler-bind ((error (lambda (condition)
                          (fatal "run-hub: running: ~a" condition))))
    (format t "Hub started. Entering event-loop...~%")
    (event-loop)))




(defun run-logger (hub-dir &optional nodaemon)
  ;; Privileged
  (do-aw-init (not nodaemon))
  (handler-bind ((error (lambda (condition)
                          (fatal "run-logger: startup: ~a" condition))))
    (setf aw-hub-dir hub-dir)
    (let (*read-eval*)
      (setf aw-hub-conf (load-conf-from-file (format nil "~a/hub.conf" aw-hub-dir))))
    (let ((logger-uid (aw-lookup-user-name-with-getpwnam (conf-get aw-hub-conf 'logger-uid)))
          (hub-uid (aw-lookup-user-name-with-getpwnam (conf-get aw-hub-conf 'hub-uid))))

      (if (or (not (integerp logger-uid)) (zerop logger-uid))
        (error "logger can't run as root"))
      (if (equal hub-uid logger-uid)
        (error "hub and logger processes must run under different users"))

      (cffi:with-foreign-string (pstr (format nil "~a/aw_log" aw-hub-dir))
        (let ((stat (aw_stat_returning_a_static_struct pstr)))
          (if (cffi:null-pointer-p stat)
            (error "aw_log dir doesn't exist"))
          (if (zerop (aw_stat_is_dir stat))
            (error "aw_log dir must be a directory"))
          (unless (= logger-uid (aw_stat_get_uid stat))
            (error "aw_log directory isn't owned by logger-uid (~a)" logger-uid))
          (unless (= logger-uid (aw_stat_get_gid stat))
            (error "aw_log directory in different group from logger-uid (~a)" logger-uid))))

      (aw-chmod (format nil "~a/aw_log" aw-hub-dir) #b111000000)

      (unless nodaemon (aw-daemonise-drop-terminal))

      (logger-setup-hub-conn)
      (cffi:with-foreign-string (pstr (format nil "~a/aw_log" aw-hub-dir))
        (aw_chroot pstr))
      (aw_dropto_uid_gid logger-uid)
      (aw_set_nproc 1)))

  ;; Unprivileged
  (handler-bind ((error (lambda (condition)
                          (fatal "logger process: ~a" condition))))
    (format t "Logger started. Entering event-loop...~%")
    (event-loop)))



(defun install-worker-conf ()
  (let ((tp-cache (conf-get aw-worker-conf 'cache)))
    (if tp-cache
      (unless (and (stringp tp-cache) (#~m|^/| tp-cache))
        (error "Bad value for cache")))
    (setq aw-worker-cache tp-cache))
  (let ((ka-forms (conf-get-all aw-worker-conf 'keepalive)))
    (if ka-forms
      (let ((ka (timespec-to-seconds (cdar ka-forms))))
        (if (<= 0 ka 3600)
          (setf keepalive_time_in_seconds ka)
          (error "bad value for keepalive")))))
  (compile-http-user-dispatch)

  (let ((c conns_in_use))
    (loop until (cffi:null-pointer-p c) do
      (cffi:with-foreign-slots ((conntype ready next) c conn)
        (when (= conntype AW_CONNTYPE_TIMER)
          (setq conntype AW_CONNTYPE_ZOMBIE  ready 0)
          (aw_touch_conn c 0))
        (setq c next))))

  (loop for ee in (conf-get-all aw-worker-conf 'eval-every) do
    (let ((c (aw_start_timer (timespec-to-seconds (cdr ee)))))
      (add-to-conn-table c
        (funcall (compile nil `(lambda (c)
                                 (fsm timer-fsm
                                   ,@(cdddr ee)
                                   (cffi:with-foreign-slots ((ready) c conn)
                                     (setq ready 0))
                                   (aw_touch_conn c ,(timespec-to-seconds (cdr ee)))
                                   timer-fsm)))
                 c)))))


(defun reload-worker-conf (tp-worker-conf)
  (flush-fast-files)
  (push-rollback "original")
  (handler-bind ((error (lambda (condition)
                          (pop-rollback)
                          (aw-log () "problem reloading worker conf: ~a. Ignoring reload request" condition)
                          (return-from reload-worker-conf nil))))
    (unless (equal (conf-get aw-worker-conf 'worker) (conf-get tp-worker-conf 'worker)) (error "worker name changed"))
    (unless (equal (conf-get aw-worker-conf 'hub-dir) (conf-get tp-worker-conf 'hub-dir)) (error "hub directory changed"))
    (unless (equal (conf-get aw-worker-conf 'chroot) (conf-get tp-worker-conf 'chroot)) (error "chroot changed"))
    (unless (equal (conf-get aw-worker-conf 'bdb-dir) (conf-get tp-worker-conf 'bdb-dir)) (error "bdb-dir changed"))
    (unless (equal (conf-get aw-worker-conf 'max-fds) (conf-get tp-worker-conf 'max-fds)) (error "max-fds changed"))
    (unless (equal (conf-get aw-worker-conf 'hub-uid) (conf-get tp-worker-conf 'hub-uid)) (error "hub-uid changed"))
    (unless (equal (conf-get aw-worker-conf 'logger-uid) (conf-get tp-worker-conf 'logger-uid)) (error "logger-uid changed"))
    (let ((aw-worker-conf tp-worker-conf))
      (install-worker-conf)) ; do this first so it checks hosts are valid before we register/unregister them with the hub
    (let* ((orig-hosts
             (flatten
               (mapcar (lambda (h) (xconf-get h :hosts))
                       (conf-get-all aw-worker-conf 'handler))))
           (new-hosts
             (flatten
               (mapcar (lambda (h) (xconf-get h :hosts))
                       (conf-get-all tp-worker-conf 'handler))))
           (to-unreg (set-difference orig-hosts new-hosts :test #'equalp))
           (to-reg (set-difference new-hosts orig-hosts :test #'equalp))
           (update-str (format nil "~{unregister-host ~a~%~}~{register-host ~a~%~}lock~%" to-unreg to-reg)))
      (write-to-conn-from-string hub_conn update-str)))
  (if (> (length aw-rollbacks) 1)
    (pop-rollback)) ; when reloading, only keep rollback if there are no previous rollbacks
  (setf aw-worker-conf tp-worker-conf)
  (aw-log () "reloaded worker conf")
  'ok)


(defun run-worker (filename &key nodaemon checking)
  (sleep 1/10) ; give hub a chance to start if launched simultaneously
  ;; Privileged
  (do-aw-init (not nodaemon))
  (handler-bind ((error (lambda (condition)
                          (fatal "run-worker: startup: ~a" condition))))
    (let (*read-eval*)
      (setf aw-worker-conf (load-conf-from-file filename)))
    (let ((name (conf-get aw-worker-conf 'worker)))
      (if (not (symbolp name))
        (error "worker name must be a symbol"))
      (if (or (null name) (eq name 'hub))
        (error "bad name for worker: ~a" name)))
    (setf aw-hub-dir (conf-get aw-worker-conf 'hub-dir))
    (worker-unix-connect checking)
    (let ((uid (aw-lookup-user-name-with-getpwnam (conf-get aw-worker-conf 'uid)))
          (max-fds (conf-get aw-worker-conf 'max-fds))
          (chroot (conf-get aw-worker-conf 'chroot))
          (bdb-dir (conf-get aw-worker-conf 'bdb-dir)))
      (if (zerop uid)
        (error "worker can't run as root"))
      (if max-fds
        (aw_set_nofile max-fds))
      (unless nodaemon (aw-daemonise-drop-terminal))
      (if chroot
        (cffi:with-foreign-string (pstr chroot)
          (aw_chroot pstr)))
      (aw_dropto_uid_gid uid)
      (if bdb-dir
        (aw-bdb-init-environment bdb-dir))))

  ;; Unprivileged
  (handler-bind ((error (lambda (condition)
                          (fatal "install-worker-conf: ~a" condition))))
    (install-worker-conf))

  (handler-bind ((error (lambda (condition)
                          (fatal "run-worker: running: ~a" condition))))
    (format t "Worker started. Entering event-loop...~%")
    (event-loop)))




(defun supervise-attach (transfer &optional cmd-str one-shot pre-transfer-cmd)
  (let ((c (cffi:with-foreign-string (fstr (format nil "~a/hub.socket" aw-hub-dir))
             (aw_conn_unix fstr)))
        (cmd-str-eval (if cmd-str (format nil "eval ~a~%~a" (length cmd-str) cmd-str) "")))
    (aw_dropto_uid_gid (aw-lookup-user-name-with-getpwnam
                         (if transfer
                           (conf-get aw-worker-conf 'uid)
                           (conf-get aw-hub-conf 'hub-uid))))
    (write-to-conn-from-string c
      (if transfer
        (if pre-transfer-cmd
          (format nil "eval ~a~%~atransfer ~a~%~aeval 7~%(stats)" (length pre-transfer-cmd) pre-transfer-cmd
                                                                  transfer cmd-str-eval)
          (format nil "transfer ~a~%~aeval 7~%(stats)" transfer cmd-str-eval))
        (format nil "~aeval 7~%(stats)" cmd-str-eval)))
    (add-to-conn-table c
      (fsm attached
        (cffi:with-foreign-slots ((ready sep currsep limit) c conn)
          (read-from-conn-into-string c shared-input-buffer ready)
          (aw_drop_n_input_bytes c ready)
          (or
            (if-match (#~m/^eval-result (\d+)\n$/ shared-input-buffer)
              (read-fixed-length-message-from-conn-and-store-in-shared-input-buffer (parse-integer $1)
                (when one-shot
                  (format t "~%~a~%~%" shared-input-buffer)
                  (quit))
                (format t "~%~a~%~%[Antiweb: Attached to ~a~a] *~%"
                            shared-input-buffer
                            (if transfer "worker " "HUB")
                            (or transfer ""))
                (let* ((v (with-lenient-struct-reader (read t)))
                       (s (let ((*print-circle* t))
                            (format nil "~S" v))))
                  (if (equal v '(quit)) (quit))
                  (write-to-conn-from-string c
                    (format nil "eval ~a~%~a" (length s) s)))
                (aw_update_conn_ready_status c)
                attached))
            (fatal "Unknown command from attached process")))))
    (event-loop)))


(defun run-supervise-hub (dir &optional cmd-str one-shot)
  (do-aw-init nil)
  (unwind-protect
    (handler-bind ((error (lambda (condition)
                            (fatal "run-supervise-hub: ~a" condition))))
      (setf aw-hub-dir dir)
      (let (*read-eval*)
        (setf aw-hub-conf (load-conf-from-file (format nil "~a/hub.conf" aw-hub-dir))))
      (supervise-attach nil cmd-str one-shot))
    (quit)))


(defun run-supervise-worker (filename &optional cmd-str one-shot)
  (do-aw-init nil)
  (unwind-protect
    (handler-bind ((error (lambda (condition)
                            (fatal "run-supervise-worker: ~a" condition))))
      (let (*read-eval*)
        (setf aw-worker-conf (load-conf-from-file filename)))
      (setf aw-hub-dir (conf-get aw-worker-conf 'hub-dir))
      (supervise-attach (conf-get aw-worker-conf 'worker) cmd-str one-shot))
    (quit)))

(defun run-reload-worker-conf (filename)
  (do-aw-init nil)
  (unwind-protect
    (handler-bind ((error (lambda (condition)
                            (fatal "run-reload-worker-conf: ~a" condition))))
      (let (*read-eval*)
        (setf aw-worker-conf (load-conf-from-file filename)))
      (setf aw-hub-dir (conf-get aw-worker-conf 'hub-dir))
      (supervise-attach (conf-get aw-worker-conf 'worker)
                        (let ((*print-circle* t))
                          (format nil "(reload-worker-conf '~S)" aw-worker-conf))
                        t
                        (format nil #"(unlock-worker '~a)"# (conf-get aw-worker-conf 'worker))))
    (quit)))
 

(defun run-add-listener (hubdir bind-addr port)
  (do-aw-init nil)
  (unwind-protect
    (handler-bind ((error (lambda (condition)
                            (fatal "run-add-listener: ~a" condition))))
      (setf aw-hub-dir hubdir)
      (let ((iconn (cffi:with-foreign-string (fstr bind-addr)
                     (aw_listen_inet fstr port)))
            (uconn (cffi:with-foreign-string (fstr (format nil "~a/hub.socket" aw-hub-dir))
                     (aw_conn_unix fstr))))
        (cffi:with-foreign-string (fstr bind-addr)
          (aw_send_conn iconn uconn fstr port))
        (add-to-conn-table iconn
          'this-inet-listener-has-been-transfered-to-the-hub)
        (add-to-conn-table uconn
          (fsm sending
            (cffi:with-foreign-slots ((ready) uconn conn)
              (read-from-conn-into-string uconn shared-input-buffer ready)
              (aw_drop_n_input_bytes uconn ready)
              (when-match (#~m/^ok\n$/ shared-input-buffer)
                (format t "~%OK~2%")
                (quit))
              (fatal "run-add-listener: unexpected message")))))
      (event-loop))
    (quit)))




(defun stats ()
  (with-output-to-string (o)
    (when (and (boundp 'aw-hub-conf) aw-hub-conf)
      (format o #"--------ANTIWEB HUB STATS---------~%"#)
      (format o #"Antiweb ~a on ~a ~a~%"# AW_VERSION (lisp-implementation-type) (lisp-implementation-version))
      (format o #"Hub dir: ~a~%"# aw-hub-dir)
      (format o #"Hub uptime: ~a~%"# (calculate-uptime aw-start-time))
      (format o #"PID: ~a  UID: ~a  GID: ~a~%"# (aw-getpid) (aw-getuid) (aw-getgid))
      (format o #"Total Connections: ~a   Dispatched: ~a (~a%)~%"#
                hub-stats-total-conns hub-stats-dispatched-conns (if (zerop hub-stats-total-conns)
                                                                   "N/A"
                                                                   (format nil "~1$"
                                                                     (* 100 (/ hub-stats-dispatched-conns
                                                                               hub-stats-total-conns)))))
      (format o #"~a"# (hub-connection-count-break-down))
      (format o #"Listening on ~a/hub.socket and:~%"# aw-hub-dir)
      (loop for v being the hash-values in inet-conn-table do
        (format o #"  ~a ~a~%"# (car v) (cadr v)))
      (format o #"Workers:~%"#)
      (loop for p being the hash-keys in worker-conn-table using (hash-value w) do
        (format o "  ~a (~a)~%" w (if (gethash (lookup-conn-pointer-from-worker-name w) locked-worker-table)
                                      "locked" "UNLOCKED"))
        (loop for k being the hash-keys in host-to-conn-dispatch-table using (hash-value v) do
          (if (= v p) (format o "    ~a~%" k )))
        (format o "~%"))
      (format o #"-----END OF ANTIWEB HUB STATS-----"#))
    (when (and (boundp 'aw-worker-conf) aw-worker-conf)
      (format o #"--------ANTIWEB WORKER STATS---------~%"#)
      (format o #"Antiweb ~a on ~a ~a~%"# AW_VERSION (lisp-implementation-type) (lisp-implementation-version))
      (format o #"Worker name: ~a  uptime: ~a~%"# (conf-get aw-worker-conf 'worker) (calculate-uptime aw-start-time))
      (format o #"PID: ~a  UID: ~a  GID: ~a  (~a)~%"# (aw-getpid) (aw-getuid) (aw-getgid)
                (if (conf-get aw-worker-conf 'chroot)
                  (format nil #"chrooted to ~a"# (conf-get aw-worker-conf 'chroot))
                  (format nil #"not chrooted"#)))
      (format o #"BerkeleyDB: ~a~%"# (if aw-use-bdb (aw-bdb-version) "Not compiled"))
      (if (conf-get aw-worker-conf 'bdb-dir)
        (format o #"  Directory: ~a~%"# (conf-get aw-worker-conf 'bdb-dir)))
      (let ((dbs (loop for k being the hash-keys in aw-bdb-open-dbs collect k)))
        (if dbs
          (format o #"  DBs: ~{~a~^ ~}~%"# dbs)))
      (format o #"Cache directory: ~a~%"# (or (if (boundp 'aw-worker-cache) aw-worker-cache)  "NONE"))
      (format o #"Keepalive Time: ~a seconds~%"# keepalive_time_in_seconds)
      (format o #"Total Connections: ~a  HTTP requests: ~a  Avg reqs/conn: ~a~%"#
                worker-stats-total-conns worker-stats-total-requests (if (zerop worker-stats-total-conns)
                                                                       "N/A"
                                                                       (format nil "~1$"
                                                                         (/ worker-stats-total-requests
                                                                            worker-stats-total-conns))))
      (format o #"~a"# (worker-connection-count-break-down))
      (format o #"Rollbacks:~%"#)
      (if aw-rollbacks
        (dolist (rb aw-rollbacks)
          (format o #"  Rollback '~a' pushed ~a ago~%"#
                    (rollback-comment rb) (calculate-uptime (rollback-time rb))))
        (format o #"  None~%"#))
      (format o #"Host -> HTML root mappings:~%"#)
      (loop for handler in (conf-get-all aw-worker-conf 'handler) do
        (loop for vhost in (xconf-get handler :hosts) do
          (format o #"  ~a -> ~a~%"# vhost (eval `(let ((http-host ,vhost))
                                                    (declare (ignorable http-host))
                                                    ,(http-root-translate handler ""))))))
      (let ((num-files 0) (num-bytes 0) (num-negs 0))
        (loop for v being the hash-values in aw-fast-files-table do
          (if (stringp v)
            (progn
              (incf num-files)
              (incf num-bytes (length v)))
            (incf num-negs)))
        (format o #"Fast-files memory cache:~%  ~a files, ~a bytes (incl HTTP headers), ~a fast 404s~%"#
                  num-files num-bytes num-negs))
      (format o #"-----END OF ANTIWEB WORKER STATS-----"#))))


(defun hub-connection-count-break-down ()
  (with-output-to-string (o)
    (let ((c conns_in_use)
          (num-zombie 0)
          (num-http 0)
          (num-http-linger 0)
          (num-unix 0)
          (num-unixlisteners 0)
          (num-inetlisteners 0)
          (num-fds 0))
      (loop until (cffi:null-pointer-p c) do
        (cffi:with-foreign-slots ((conntype sd fd next) c conn)
        (if (/= sd -1) (incf num-fds))
        (if (/= fd -1) (incf num-fds))
        (cond
          ((= conntype AW_CONNTYPE_ZOMBIE) (incf num-zombie))
          ((= conntype AW_CONNTYPE_HTTP) (incf num-http))
          ((= conntype AW_CONNTYPE_HTTP_LINGER) (incf num-http-linger))
          ((= conntype AW_CONNTYPE_UNIX) (incf num-unix))
          ((= conntype AW_CONNTYPE_UNIXLISTENER) (incf num-unixlisteners))
          ((= conntype AW_CONNTYPE_INETLISTENER) (incf num-inetlisteners)))
        (setq c next)))
    (format o #"File descriptor usage (estimate): ~a/~a~%"# (+ 7 num-fds) (aw_get_nofile)) ; FIXME
    (format o #"Current Connections: ~a~%"# num_conns_in_use)
    (format o #"  Un-dispatched HTTP: ~a  Unix Connections: ~a~%"# num-http num-unix)
    (format o #"  Lingering HTTP: ~a  Zombies: ~a~%"# num-http-linger num-zombie)
    (format o #"  Unix Listeners: ~a  Inet Listeners: ~a~%"# num-unixlisteners num-inetlisteners))))


(defun worker-connection-count-break-down ()
  (with-output-to-string (o)
    (let ((c conns_in_use)
          (num-zombie 0)
          (num-http 0)
          (num-http-sending-file 0)
          (num-http-linger 0)
          (num-proxy-source 0) (num-proxy-sink 0) (num-proxy-idle 0)
          (num-timer 0)
          (num-unix 0)
          (num-fds 0))
      (loop until (cffi:null-pointer-p c) do
        (cffi:with-foreign-slots ((conntype sd fd next) c conn)
        (if (/= sd -1) (incf num-fds))
        (if (/= fd -1) (incf num-fds))
        (cond
          ((= conntype AW_CONNTYPE_ZOMBIE) (incf num-zombie))
          ((= conntype AW_CONNTYPE_HTTP) (incf num-http))
          ((= conntype AW_CONNTYPE_HTTP_SEND_FILE) (incf num-http-sending-file))
          ((= conntype AW_CONNTYPE_HTTP_LINGER) (incf num-http-linger))
          ((= conntype AW_CONNTYPE_PROXY_SOURCE) (incf num-proxy-source))
          ((= conntype AW_CONNTYPE_PROXY_SINK) (incf num-proxy-sink))
          ((= conntype AW_CONNTYPE_PROXY_IDLE) (incf num-proxy-idle))
          ((= conntype AW_CONNTYPE_UNIX) (incf num-unix))
          ((= conntype AW_CONNTYPE_TIMER) (incf num-timer)))
        (setq c next)))
    (format o #"File descriptor usage (estimate): ~a/~a~%"# (+ 4 num-fds) (aw_get_nofile)) ; FIXME
    (format o #"Current Connections: ~a~%"# num_conns_in_use)
    (format o #"  Keepalives: ~a  Sending files to: ~a~%"# num-http num-http-sending-file)
    (format o #"  Proxy: Sources: ~a  Sinks: ~a  Idle: ~a~%"# num-proxy-source num-proxy-sink num-proxy-idle)
    (format o #"  Timers: ~a  Hub: 1  Unix Connections: ~a~%"# num-timer (1- num-unix))
    (format o #"  Lingering: ~a  Zombies: ~a~%"# num-http-linger num-zombie))))

(defun conns-in-use-debug ()
  (with-output-to-string (o)
    (let ((c conns_in_use))
      (format o "~a conns:~%" num_conns_in_use)
      (loop until (cffi:null-pointer-p c) do
        (cffi:with-foreign-slots ((conntype expiry outlen sd next) c conn)
          (format o "  conntype=~a  expiry=~a  outlen=~a  sd=~a~%" conntype expiry outlen sd)
          (setq c next))))))
