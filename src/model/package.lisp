;;;; package.lisp --- Package definition for the model module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage :rosetta.model
  (:nicknames
   :rs.m)

  (:use
   :cl
   :alexandria
   :let-plus)

  ;; Types
  (:export
   :name-component

   :name/absolute
   :name-expression/absolute

   :name/relative
   :name-expression/relative

   :name
   :name-expression)

  ;; Name protocol
  (:export
   :kind

   :name
   :qname)

  ;; Utility functions
  (:export
   :print-qname)

  (:documentation
   "This package contains general model-related functions and
classes."))
