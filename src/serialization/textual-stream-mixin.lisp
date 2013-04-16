;;;; textual-stream-mixin.lisp --- Mixin for textual mechanisms based on streams.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rosetta.serialization)

(defclass textual-stream-mixin (textual-mixin)
  ()
  (:documentation
   "This class is intended to be mixed into mechanism classes that
implement textual mechanisms based on streams."))

(defmethod pack ((mechanism   textual-stream-mixin)
		 (source      t)
		 (destination (eql 'string))
		 &rest args
		 &key)
  "The value of DESTINATION indicates that a string should be created
as destination."
  (let ((result (with-output-to-string (stream)
		  (apply #'pack mechanism source stream args))))
    (values (length result) result)))

(defmethod unpack ((mechanism   textual-stream-mixin)
		   (source      pathname)
		   (destination t)
		   &rest args
		   &key)
  "Open a stream for SOURCE and unpack the contents into DESTINATION."
  (with-input-from-file (stream source)
    (apply #'unpack mechanism stream destination args)))
