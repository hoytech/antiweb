;; Antiweb (C) Doug Hoyte


;; In CL, there is no portable way to create or read a file with an
;; asterisk in its name. The following workaround works for CMU
;; (did I get all the chars?) but there is no way to do this in
;; CLISP so it will blow up if you have pathnames with * or ?.
;; VERY LUCKY that CLISP just passes : through OK (needed for vhosts like localhost:8080)
;; http://osdir.com/ml/lisp.clisp.general/2003-04/msg00101.html

(defun workaround-cl-pathnames (filename)
  #+(or cmu sbcl) (setq filename (#~s/([\\:*?])/\\\1/ filename))
  filename)


#|
In AW, a "conf" is a list of all the forms read from a file. The following two lines in a file

(worker blah)
(uid 1234)

would be loaded as this conf:

((WORKER BLAH)
 (UID 1234))

* The key-value pairs are accessed with conf-get and conf-get-all
  ie: (conf-get my-conf 'worker) => "blah"
|#

(defun load-conf-from-file (filename)
  (load-conf-from-file-aux filename nil))

(defun load-conf-from-file-aux (filename already-inherited)
  (let* ((conf (with-open-file (f (workaround-cl-pathnames filename) :direction :input)
                 (loop for i = (read f nil nil) while i collect i)))
         (final-conf conf))
    (loop for x in conf do
      (unless (and (consp x) (symbolp (car x)))
        (error "invalid xconf: ~a" x))
      (when (eq (car x) 'inherit)
        (if (member (cadr x) already-inherited :test #'equal)
          (error "already inherited ~a" (cadr x)))
        (setq final-conf (append final-conf
                                 (load-conf-from-file-aux (cadr x) (cons (cadr x) already-inherited))))))
    final-conf))

(defun conf-get-all (conf key)
  (remove-if-not (lambda (e) (and (consp e) (eq key (car e)))) conf))

(defun conf-get (conf key)
  (cadar (conf-get-all conf key)))



#|
Some elements in a conf are called "extended conf elements" or "xconfs". They are of the format:

(name-of-xconf [arguments]
  :key1 (blah blah)
  :key2 "sup?")

The number and meaning of [arguments] depends on the type of element (often there are none)
The :key key-value pairs are similar to destructuring keyword arguments except they may contain duplicates
and they are more robust (forgiving) in the event of accidental element additions/omissions.

* You must use conf-get-all to get xconfs because conf-get thows out info.
 (xconf-get-all '(:a 1 :b 123 :a 2 :c 324) :a) => (1 2)
 (xconf-get-all '(:a 1 :b 123 :a 2 :c 324) '(:a :b)) => ((:A 1) (:B 123) (:A 2))

 (xconf-get '(:a 1 :b 123 :a 2 :c 324) :a) => 1
|#


(defun xconf-get (xconf key)
  (let ((v (member key xconf)))
    (if (and v (not (cdr v)))
      t
      (cadr v))))

(defun xconf-get-all (xconf key)
  (unless (listp key) (setq key (list key)))
  (if xconf
    (let ((q (find (car xconf) key)))
      (if q
        (cons (if (> (length key) 1)
                (list (car xconf) (cadr xconf))
                (cadr xconf))
              (xconf-get-all (cddr xconf) key))
        (xconf-get-all (cdr xconf) key)))))
