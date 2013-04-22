;;;; package.lisp --- Package definition for unit tests of the model.serialization module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rosetta.model.serialization.test
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:lift

   #:rosetta.model.data
   #:rosetta.model.serialization

   #:rosetta.model.test
   #:rosetta.model.data.test)

  (:shadowing-import-from #:rosetta.model.serialization
   #:type1)

  (:export
   #:model-serialization-root)

  (:documentation
   "This package contains unit tests for the model.serialization
module."))

(cl:in-package #:rosetta.model.serialization.test)

(deftestsuite model-serialization-root (model-root)
  ()
  (:documentation
   "Root unit test suite for the model.serialization module."))

(defclass mock-mechanism/validate-type ()
  ((valid? :initarg  :valid?
           :reader   mechanism-valid?
           :initform t)))

(defmethod validate-type ((mechanism mock-mechanism/validate-type)
                          (type      t)
                          &key &allow-other-keys)
  (when (not (mechanism-valid? mechanism))
    (error "~@<Mock type validation error.~@:>"))

  (or (fundamental? type)
      (every (curry #'validate-type mechanism)
             (direct-dependencies type))))
