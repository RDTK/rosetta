;;;; data-holder-mixin.lisp --- Unit tests for the data-holder-mixin class.x
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.serialization.test)

;;; Mock classes

(defclass mock-class-for-data-holder-mixin ()
  ())

(defmethod find-mechanism-class ((spec (eql :mock-for-data-holder-mixin)))
  (find-class 'mechanism-mock-for-data-holder-mixin))

(defclass mechanism-mock-for-data-holder-mixin (data-holder-mixin)
  ())

(defmethod unpack ((mechanism   mechanism-mock-for-data-holder-mixin)
                   (source      string)
                   (destination mock-class-for-data-holder-mixin)
                   &key)
  destination)

;;; Test suite

(deftestsuite data-holder-mixin-root (serialization-root)
  ((simple-mechanism (make-instance 'mechanism-mock-for-data-holder-mixin)))
  (:documentation
   "Root test suite for the `data-holder-mixin' class."))

(addtest (data-holder-mixin-root
          :documentation
          "Test case for the methods on `unpack' provided by the
`data-holder-mixin' class.")
  unpack

  ;; Test finding the class by.
  (ensure (typep (unpack simple-mechanism
                         "foo"
                         'mock-class-for-data-holder-mixin)
                 'mock-class-for-data-holder-mixin))

  ;; Test creating an instance to unpack into.
  (ensure (typep (unpack simple-mechanism
                         "foo"
                         (find-class 'mock-class-for-data-holder-mixin))
                 'mock-class-for-data-holder-mixin)))
