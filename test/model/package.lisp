;;;; package.lisp --- Package definition for unit tests of the model module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage :rosetta.model.test
  (:use
   :cl
   :lift

   :rosetta.model

   :rosetta.test)

  (:export
   :model-root)

  (:documentation
   "This package contains unit tests for the model module."))

(cl:in-package :rosetta.model.test)

(deftestsuite model-root (root)
  ()
  (:documentation
   "Root unit test suite for the model module."))
