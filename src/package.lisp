;;;; package.lisp --- Package definition for rosetta module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rosetta
  (:use
   #:cl
   #:alexandria
   #:iterate
   #:let-plus)

  ;; print-items mechanism and mixin class
  (:export
   #:print-items

   #:print-items-mixin)

  ;; Utilities
  (:export
   #:camel-case-boundary?
   #:underscore-boundary?
   #:normalize-name)

  (:documentation
   "This package contains generic facilities used within the rosetta
system."))
