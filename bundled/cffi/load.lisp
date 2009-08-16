;; Antiweb (C) Doug Hoyte
;; CFFI loader modeled after load.lisp from CL-PPCRE

(in-package :cl-user)

(let ((cffi-base-directory
        (make-pathname :name nil :type nil :version nil
                       :defaults (parse-namestring *load-truename*)))
      must-compile)
  (with-compilation-unit ()
    (dolist (file '("utils"
                    "features"
                    #+sbcl "cffi-sbcl"
                    #+cmu "cffi-cmucl"
                    #+clisp "cffi-clisp"
                    #+ccl "cffi-openmcl"
                    "package"
                    "libraries"
                    "early-types"
                    "types"
                    "enum"
                    "strings"
                    "functions"
                    "foreign-vars"))
      (let ((pathname (make-pathname :name file :type "lisp" :version nil
                                     :defaults cffi-base-directory)))
        (let ((compiled-pathname (compile-file-pathname pathname)))
          (unless (and (not must-compile)
                       (probe-file compiled-pathname)
                       (< (file-write-date pathname)
                          (file-write-date compiled-pathname)))
            (setq must-compile t)
            (compile-file pathname))
          (setq pathname compiled-pathname))
        (load pathname)))))
