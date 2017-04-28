;;;; package.lisp --- Package definition for the model module.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rosetta.model
  (:nicknames
   #:rs.m)

  (:use
   #:cl
   #:alexandria
   #:iterate
   #:let-plus)

  ;; Types
  (:export
   #:name-component

   #:name/absolute
   #:name-expression/absolute

   #:name/relative
   #:name-expression/relative

   #:name
   #:name-expression)

  ;; Name protocol
  (:export
   #:kind

   #:name
   #:qname
   #:qname/kind)

  ;; Utility functions
  (:export
   #:print-qname
   #:print-name-expression)

  (:documentation
   "General model-related functions and classes."))
