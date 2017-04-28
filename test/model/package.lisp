;;;; package.lisp --- Package definition for unit tests of the model module.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rosetta.model.test
  (:use
   #:cl
   #:lift

   #:rosetta.model

   #:rosetta.test)

  (:shadowing-import-from #:rosetta.model
   #:root)

  (:export
   #:model-root)

  (:documentation
   "This package contains unit tests for the model module."))

(cl:in-package #:rosetta.model.test)

(deftestsuite model-root (rosetta.test:root)
  ()
  (:documentation
   "Root unit test suite for the model module."))
