;;;; package.lisp --- Package definition for unit tests of the rosetta system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rosetta.test
  (:use
   #:cl
   #:alexandria
   #:lift

   #:rosetta)

  (:export
   #:root)

  (:documentation
   "This package contains unit tests for the rosetta system"))

(cl:in-package #:rosetta.test)

(deftestsuite root ()
  ()
  (:documentation
   "Root unit test suite for the rosetta system."))
