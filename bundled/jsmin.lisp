;;; Re-packaged for Antiweb
;;;   :overwrite changed to :supersede in jsmin-file

;;; Copyright (c) 2007, Ury Marshak
;;; This is a port of original C code by Douglas Crockford to
;;; Common Lisp. There was no attempt to make the code more
;;; "lispy", it is just a rather faithful translation. This code
;;; may be used under the same conditions as the C original, which
;;; has the following copyright notice:
;;; 
;;; /* jsmin.c
;;;    2007-01-08
;;;
;;; Copyright (c) 2002 Douglas Crockford  (www.crockford.com)
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;;; this software and associated documentation files (the "Software"), to deal in
;;; the Software without restriction, including without limitation the rights to
;;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is furnished to do
;;; so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; The Software shall be used for Good, not Evil.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.
;;; */
;;;


(defpackage #:jsmin
   (:use :cl)
   (:export #:jsmin
            #:jsmin-file))

(in-package #:jsmin)



(defun is-alphanum (c)
  "isAlphanum -- return true if the character is a letter, digit, underscore,
        dollar sign, or non-ASCII character"
  (and c
       (or (and (char>= c #\a) (char<= c #\z))
           (and (char>= c #\0) (char<= c #\9))
           (and (char>= c #\A) (char<= c #\Z))
           (char= c #\_)
           (char= c #\$)
           (char= c #\\)
           (> (char-code c) 126))))



(defun %jsmin (in out)
  (let (the-a the-b the-lookahead (pos 0))
    (labels ((get-c ()
               ;; return the next character from stdin. Watch out for lookahead. If
               ;; the character is a control character, translate it to a space or
               ;; linefeed.
               (let ((c the-lookahead))
                 (setf the-lookahead nil)
                 (unless c
                   (setf c (read-char in nil nil))
                   (incf pos))
                 (cond
                   ((or (null c)
                        (char= c #\Newline)
                        (char>= c #\Space)) c)
                   ((char= c (code-char 13)) #\Newline)
                   (t #\Space))))
           
             (peek ()
               ;; get the next character without getting it
               (setf the-lookahead (get-c)))
           
             (next ()
               ;; get the next character, excluding comments. peek()
               ;; is used to see if a '/' is followed by a '/' or '*'.
               (let ((c (get-c)))
                 (if (and c
                          (char= c #\/))
                     (case (peek)
                       (#\/
                        (loop for cc = (get-c)
                           while (and cc
                                      (char> cc #\Newline))
                             finally (return cc)))
                       (#\*
                        (get-c)
                        (loop for cc = (get-c)
                           unless cc
                           do (error "JSMIN: Unterminated comment.")
                           when (and (char= cc #\*)
                                     (char= (peek) #\/))
                           do (progn (get-c) (return #\Space))))
                       (otherwise
                        c))
                     c)))


             (action (d)
               ;; action -- do something! What you do is determined by the argument:
               ;;         1   Output A. Copy B to A. Get the next B.
               ;;         2   Copy B to A. Get the next B. (Delete A).
               ;;         3   Get the next B. (Delete B).
               ;;    action treats a string as a single character. Wow!               
               ;;    action recognizes a regular expression if it is
               ;;    preceded by ( or , or =.

               
               (when (= d 1)
                 (write-char the-a out))
               (when (<= d 2)
                 (setf the-a the-b)
                 (when (and the-a
                            (or (char= the-a #\')
                                (char= the-a #\")))
                   (loop
                      (progn
                        (write-char the-a out)
                        (setf the-a (get-c))
                        (when (and the-a (char= the-a the-b))
                          (return))
                        (when (or (null the-a)
                                  (char<= the-a #\Newline))
                          (error "JSMIN unterminated string literal: ~C at position ~A" the-b pos))
                        (when (char= the-a #\\)
                          (write-char the-a out)
                          (setf the-a (get-c)))))))
               (when (<= d 3)
                 (setf the-b (next))
                 (when (and the-b
                            (char= the-b #\/)
                            (position the-a "(,=:[!&|?"))
                   (write-char the-a out)
                   (write-char the-b out)
                   (loop
                      (progn
                        (setf the-a (get-c))
                        (when (and the-a
                                   (char= the-a #\/))
                          (return))
                        (when (and the-a
                                   (char= the-a #\\))
                          (write-char the-a out)
                          (setf the-a (get-c)))
                        (when (or (null the-a)
                                  (char<= the-a #\Newline))
                          (error "JSMIN: unterminated Regular Expression literal."))
                        (write-char the-a out)))
                   (setf the-b (next))))))
      ;; jsmin -- Copy the input to the output, deleting the characters
      ;;   which are insignificant to JavaScript. Comments will be
      ;;   removed. Tabs will be replaced with spaces. Carriage returns will
      ;;   be replaced with linefeeds.  Most spaces and linefeeds will be
      ;;   removed.
      (setf the-a #\Newline)
      (action 3)
      (loop while the-a
         do (case the-a
             (#\Space
              (if (is-alphanum the-b)
                  (action 1)
                  (action 2)))
             (#\Newline
              (case the-b
                ((#\{ #\[ #\( #\+ #\-)
                 (action 1))
                (#\Space
                 (action 3))
                (otherwise
                 (if (is-alphanum the-b)
                     (action 1)
                     (action 2)))))
             (otherwise
              (case the-b
                (#\Space
                 (if (is-alphanum the-a)
                     (action 1)
                     (action 3)))
                (#\Newline
                 (case the-a
                   ((#\} #\] #\) #\+ #\- #\" #\')
                    (action 1))
                   (otherwise
                    (if (is-alphanum the-a)
                        (action 1)
                        (action 3)))))
                (otherwise
                 (action 1))))))
      )))


(defun jsmin (js)
  (with-output-to-string (out)
    (with-input-from-string (in js)
      (%jsmin in out))))

(defun jsmin-file (infile outfile)
  (with-open-file (in infile :direction :input)
    (with-open-file (out outfile :direction :output :if-exists :supersede :if-does-not-exist :create)
      (%jsmin in out))))
