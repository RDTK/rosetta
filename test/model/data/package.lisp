;;;; package.lisp --- Package definition for unit tests of the model.data module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rosetta.model.data.test
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate
   #:more-conditions
   #:lift

   #:rosetta
   #:rosetta.model
   #:rosetta.model.data

   #:rosetta.test
   #:rosetta.model.test)

  (:shadowing-import-from #:rosetta.model.data
   #:root)

  ;; Some simple types
  (:export
   #:+package/root+
   #:+package/simple+

   #:+singleton/uint32+
   #:+singleton/float64+

   #:+enum/uint8/empty+
   #:+enum/uint8/simple+
   #:+enum/uint8/one+
   #:+enum/uint32/simple+
   #:+enum/int32/simple+

   #:+struct/simple+
   #:+struct/empty+
   #:+struct/recursive+
   #:+struct/nested+
   #:+struct/packaged+)

  (:documentation
   "This package contains unit tests for the model.data module."))

(cl:in-package #:rosetta.model.data.test)

(deftestsuite model-data-root (model-root)
  ()
  (:documentation
   "Root unit test suite for the model.data module."))

;;; `mock-type/validate-value'

(defclass mock-type/validate-value ()
  ())

(defmethod validate-value ((type  mock-type/validate-value)
                           (value t)
                           &key &allow-other-keys)
  (error "~@<Mock value validation error.~@:>"))

;;; Simple data types

(defparameter +package/root+
  (make-instance 'package1 :name ""))

(defparameter +package/simple+
  (let ((package (make-instance 'package1 :name "package")))
    (setf (lookup +package/root+ :structure "package") package)
    package))

(defparameter +singleton/uint32+
  (make-instance 'singleton :type +uint32+ :value 1)
  "An uint32 singleton.")

(defparameter +singleton/float64+
  (make-instance 'singleton :type +float64+ :value 1.0d0)
  "A float64 singleton.")

(defparameter +enum/uint8/empty+
  (make-instance
   'enum :name   "simple/empty8"
         :type   +uint8+
         :values '()
         :documentation
         "A empty uint8 enum."))

(defparameter +enum/uint8/simple+
  (make-instance
   'enum :name   "simple/uint8"
         :type   +uint8+
         :values '(:a 1 :b 2)
         :documentation
         "A simple uint8 enum with two values."))

(defparameter +enum/uint8/one+
  (make-instance
   'enum :name   "one/uint8"
         :type   +uint8+
         :values '(:a 1)
         :documentation
         "A simple uint8 enum with a single value."))

(defparameter +enum/uint32/simple+
  (make-instance
   'enum :name   "simple/uint32"
         :type   +uint32+
         :values '(:a 1 :b 2)
         :documentation
         "A simple uint32 enum with two values."))

(defparameter +enum/int32/simple+
  (make-instance
   'enum :name   "simple/int32"
         :type   +int32+
         :values '(:a 1 :b 2)
         :documentation
         "A simple int32 enum with two values."))

(defparameter +struct/simple+
  (make-instance
   'base-structure
   :name   "simple"
   :fields `("a" (,+utf-8-string+))
   :documentation
   "A simple structure with a single field."))

(defparameter +struct/empty+
  (make-instance
   'base-structure
   :name   "empty"
   :fields '()
   :documentation
   "An empty structure."))

(defparameter +struct/recursive+
  (let ((struct
          (make-instance
           'base-structure
           :name   "recursive"
           :fields `("a" (,+uint16+) "b" (,+utf-8-string+))
           :documentation
           "A simple recursive structure.")))
    (setf (lookup struct :field "c")
          (make-instance 'base-field
                         :name "c"
                         :type (make-instance 'base-array
                                              :index-type   +int32+
                                              :element-type struct)))
    struct))

(defparameter +struct/nested+
  (let* ((inner (make-instance 'base-structure
                               :name   "inner"
                               :fields `("a" (,+uint16+))
                               :documentation
                               "Inner structure."))
         (outer (make-instance 'base-structure
                               :name   "outer"
                               :nested `(,inner)
                               :fields `("a" (,+uint16+))
                               :documentation
                               "Outer structure.")))
    outer))

(defparameter +struct/packaged+
  (let ((struct (make-instance
                 'base-structure
                 :name   "packaged"
                 :fields `("a" (,+uint16+) "b" (,+utf-8-string+))
                 :documentation
                 "A packaged structure.")))
    (setf (lookup +package/simple+ :structure "packaged") struct)
    struct))
