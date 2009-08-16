;; Antiweb (C) Doug Hoyte

;; Default AWP glue

(defglue link (url text)
  (format nil #"<a href="~a">~a</a>"# url text))

(defglue p (&rest ps)
  (format nil #"~{<p>~a</p>~}"# (mapcar #'awp-super-glue ps)))

(defglue pre (&rest ps)
  (format nil #"~{<pre>~a</pre>~}"# (mapcar #'html-escape ps)))

(defglue ul (&rest ps)
  (format nil #"<ul>~{<li>~a~}</ul>"# (mapcar #'awp-super-glue ps)))

(defglue ol (&rest ps)
  (format nil #"<ol>~{<li>~a~}</ol>"# (mapcar #'awp-super-glue ps)))

(defglue fieldset (legend &rest ps)
  (format nil #"<fieldset><legend>~a</legend>~a</fieldset>"# (awp-super-glue legend) (awp-super-glue (cons 'p ps))))

(defglue center (&rest ps)
  (format nil #"<center>~a</center>"# (awp-super-glue (cons 'p ps))))
