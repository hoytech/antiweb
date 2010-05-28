;; This file is part of yason, a Common Lisp JSON parser/encoder
;;
;; Copyright (c) 2008 Hans Hübner
;; All rights reserved.
;;
;; Please see the file LICENSE in the distribution.

(in-package :yason)

(defvar *json-output*)

(defmacro with-standard-output-to ((stream) &body body)
  `(let ((*standard-output* ,stream))
     ,@body))

(defgeneric encode (object &optional stream)

  (:documentation "Encode OBJECT to STREAM in JSON format.  May be
  specialized by applications to perform specific rendering.  STREAM
  defaults to *STANDARD-OUTPUT*.")

  (:method ((object string) &optional (stream *standard-output*))
    (with-standard-output-to (stream)
      (princ #\")
      (loop
         for char across object
         do (case char
              ((#\\ #\" #\/)
               (princ #\\) (princ char))
              (#\Backspace
               (princ #\\) (princ #\b))
              (#\Page
               (princ #\\) (princ #\f))
              (#\Newline
               (princ #\\) (princ #\n))
              (#\Return
               (princ #\\) (princ #\r))
              (#\Tab
               (princ #\\) (princ #\t))
              (t
               (princ char))))
      (princ #\"))
    object)

  (:method ((object rational) &optional (stream *standard-output*))
    (encode (float object) stream)
    object)

  (:method ((object integer) &optional (stream *standard-output*))
    (princ object stream))

  (:method ((object hash-table) &optional (stream *standard-output*))
    (with-standard-output-to (stream)
      (princ #\{)
      (let (printed)
        (maphash (lambda (key value)
                   (if printed
                       (princ #\,)
                       (setf printed t))
                   (encode key stream)
                   (princ #\:)
                   (encode value stream))
                 object))
      (princ #\}))
    object)

  (:method ((object vector) &optional (stream *standard-output*))
    (with-standard-output-to (stream)
      (princ #\[)
      (let (printed)
        (loop
           for value across object
           do
             (when printed
               (princ #\,))
             (setf printed t)
             (encode value stream)))
      (princ #\]))
    object)

  (:method ((object list) &optional (stream *standard-output*))
    (with-standard-output-to (stream)
      (princ #\[)
      (let (printed)
        (dolist (value object)
          (if printed
              (princ #\,)
              (setf printed t))
          (encode value stream)))
      (princ #\]))
    object)

  (:method ((object (eql 'true)) &optional (stream *standard-output*))
    (princ "true" stream)
    object)

  (:method ((object (eql 'false)) &optional (stream *standard-output*))
    (princ "false" stream)
    object)

  (:method ((object (eql 'null)) &optional (stream *standard-output*))
    (princ "null" stream)
    object)

  (:method ((object (eql t)) &optional (stream *standard-output*))
    (princ "true" stream)
    object)

  (:method ((object (eql nil)) &optional (stream *standard-output*))
    (princ "null" stream)
    object))

(defclass json-output-stream ()
  ((output-stream :reader output-stream
                  :initarg :output-stream)
   (stack :accessor stack
          :initform nil))
  (:documentation "Objects of this class capture the state of a JSON stream encoder."))

(defun next-aggregate-element ()
  (if (car (stack *json-output*))
      (princ (car (stack *json-output*)) (output-stream *json-output*))
      (setf (car (stack *json-output*)) #\,)))

(defmacro with-output ((stream) &body body)
  "Set up a JSON streaming encoder context on STREAM, then evaluate BODY."
  `(let ((*json-output* (make-instance 'json-output-stream :output-stream ,stream)))
     ,@body))

(defmacro with-output-to-string* (() &body body)
  "Set up a JSON streaming encoder context, then evaluate BODY.
Return a string with the generated JSON output."
  `(with-output-to-string (s)
     (with-output (s)
       ,@body)))

(define-condition no-json-output-context (error)
  ()
  (:report "No JSON output context is active")
  (:documentation "This condition is signalled when one of the stream
  encoding function is used outside the dynamic context of a
  WITH-OUTPUT or WITH-OUTPUT-TO-STRING* body."))

(defmacro with-aggregate ((begin-char end-char) &body body)
  `(progn
     (unless (boundp '*json-output*)
       (error 'no-json-output-context))
     (when (stack *json-output*)
       (next-aggregate-element))
     (princ ,begin-char (output-stream *json-output*))
     (push nil (stack *json-output*))
     (prog1
         (progn ,@body)
       (pop (stack *json-output*))
       (princ ,end-char (output-stream *json-output*)))))

(defmacro with-array (() &body body)
  "Open a JSON array, then run BODY.  Inside the body,
ENCODE-ARRAY-ELEMENT must be called to encode elements to the opened
array.  Must be called within an existing JSON encoder context, see
WITH-OUTPUT and WITH-OUTPUT-TO-STRING*."
  `(with-aggregate (#\[ #\]) ,@body))

(defmacro with-object (() &body body)
  "Open a JSON object, then run BODY.  Inside the body,
ENCODE-OBJECT-ELEMENT or WITH-OBJECT-ELEMENT must be called to encode
elements to the object.  Must be called within an existing JSON
encoder context, see WITH-OUTPUT and WITH-OUTPUT-TO-STRING*."
  `(with-aggregate (#\{ #\}) ,@body))

(defun encode-array-element (object)
  "Encode OBJECT as next array element to the last JSON array opened
with WITH-ARRAY in the dynamic context.  OBJECT is encoded using the
ENCODE generic function, so it must be of a type for which an ENCODE
method is defined."
  (next-aggregate-element)
  (encode object (output-stream *json-output*)))

(defun encode-object-element (key value)
  "Encode KEY and VALUE as object element to the last JSON object
opened with WITH-OBJECT in the dynamic context.  KEY and VALUE are
encoded using the ENCODE generic function, so they both must be of a
type for which an ENCODE method is defined."
  (next-aggregate-element)
  (encode key (output-stream *json-output*))
  (princ #\: (output-stream *json-output*))
  (encode value (output-stream *json-output*))
  value)

(defmacro with-object-element ((key) &body body)
  "Open a new encoding context to encode a JSON object element.  KEY
  is the key of the element.  The value will be whatever BODY
  serializes to the current JSON output context using one of the
  stream encoding functions.  This can be used to stream out nested
  object structures."
  `(progn
     (next-aggregate-element)
     (encode ,key (output-stream *json-output*))
     (setf (car (stack *json-output*)) #\:)
     (unwind-protect
          (progn ,@body)
       (setf (car (stack *json-output*)) #\,))))

