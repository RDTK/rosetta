;;;; package.lisp --- Package definition for unit tests of the model.serialization module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage :rosetta.model.serialization.test
  (:use
   :cl
   :let-plus
   :lift

   :rosetta.model.serialization

   :rosetta.model.test)

  (:export
   :model-serialization-root)

  (:documentation
   "This package contains unit tests for the model.serialization
module."))

(cl:in-package :rosetta.model.serialization.test)

(deftestsuite model-serialization-root (model-root)
  ()
  (:documentation
   "Root unit test suite for the model.serialization module."))

(defclass mock-mechanism/validate-type ()
  ())

(defmethod validate-type ((mechanism mock-mechanism/validate-type)
			  (type      t)
			  &key &allow-other-keys)
  (error "~@<Mock type validation error.~@:>"))
