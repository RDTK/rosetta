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

  ;; Utilities
  (:export
   #:digit-boundary?
   #:camel-case-boundary?
   #:separator-boundary?

   #:normalize-name)

  (:documentation
   "This package contains generic facilities used within the rosetta
system."))
