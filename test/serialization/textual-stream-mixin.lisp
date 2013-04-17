;;;; textual-stream-mixin.lisp --- Unit tests for the textual-stream-mixin class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rosetta.serialization.test)

;;; Mock mechanism class

(defmethod find-mechanism-class ((spec (eql :mock-for-textual-stream-mixin)))
  (find-class 'mechanism-mock-for-textual-stream-mixin))

(defclass mechanism-mock-for-textual-stream-mixin (textual-stream-mixin)
  ())

(defmethod pack ((mechanism   mechanism-mock-for-textual-stream-mixin)
                 (source      t)
                 (destination stream)
                 &key)
  (format destination "~S" source)
  (values nil destination))

(defmethod unpack ((mechanism   mechanism-mock-for-textual-stream-mixin)
                   (source      stream)
                   (destination t)
                   &key)
  (values (read source) nil))

;;; Test suite

(deftestsuite textual-stream-mixin-root (serialization-root)
  ((simple-mechanism (make-instance 'mechanism-mock-for-textual-stream-mixin)))
  (:documentation
   "Root test suite for the `textual-stream-mixin' class."))

(define-basic-textual-mechanism-tests textual-stream-mixin)
