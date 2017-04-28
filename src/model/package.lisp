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
   #:let-plus
   #:more-conditions)

  ;; Types
  (:export
   #:name-component

   #:name/absolute
   #:name-expression/absolute

   #:name/relative
   #:name-expression/relative

   #:name
   #:name-expression)

  ;; Conditions
  (:export
   #:container-condition
   #:container-condition-container

   #:child-condition
   #:child-condition-key

   #:child-error

   #:chainable-child-error ; condition and function

   #:simple-child-error)   ; condition and function

  ;; Name protocol
  (:export
   #:kind

   #:name
   #:qname
   #:qname/kind)

  ;; Parent protocol
  (:export
   #:parent
   #:ancestors
   #:root)

  ;; Builder protocol
  (:export
   #:find-node
   #:make-node
   #:add-child)

  ;; Utility functions
  (:export
   #:print-qname
   #:print-name-expression)

  (:documentation
   "General model-related functions and classes."))
