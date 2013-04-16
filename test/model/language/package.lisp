;;;; package.lisp --- Package definition for unit tests of the model.language module.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage :rosetta.model.language.test
  (:use
   :cl
   :alexandria
   :let-plus
   :lift

   :rosetta.model.language

   :rosetta.model.test)

  (:export
   :rosetta.model.language-root)

  (:documentation
   "This package contains unit tests for the model.language module."))

(cl:in-package #:rosetta.model.language.test)

(deftestsuite rosetta.model.language-root (model-root)
  ()
  (:documentation
   "Root unit test suite for the model.language module."))
