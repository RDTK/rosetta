;;;; package.lisp --- Package definition for unit tests of the serialization module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rosetta.serialization.test
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:flexi-streams
   #:lift

   #:rosetta.serialization

   #:rosetta.model.serialization

   #:rosetta.test)

  (:documentation
   "This package contains unit tests for the serialization module"))

(cl:in-package #:rosetta.serialization.test)

(deftestsuite serialization-root (root)
  ()
  (:documentation
   "Root unit test suite for the serialization module."))
