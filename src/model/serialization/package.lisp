;;;; package.lisp --- Package definition for model.serialization module.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rosetta.model.serialization
  (:nicknames
   #:rs.m.s)

  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions

   #:rosetta.model.data)

  (:shadow
   #:name
   #:type1)

  ;; Types
  (:export
   #:endian
   #:endian-designator

   #:resolve-endian
   #:opposite-platform-endian)

  ;; Conditions
  (:export
   #:type-invalid-for-mechanism

   #:type1
   #:mechanism)

  ;; Mechanism protocol
  (:export
   #:name

   #:wire-type
   #:offset-type
   #:length-type

   #:endian-for

   #:validate-type)

  ;; Mechanism service
  (:export
   #:mechanism)

  ;; Mixins
  (:export
   #:wire-type-mixin
   #:offset-type-mixin
   #:length-type-mixin

   #:constant-endian-mixin)

  (:documentation
   "This package contains model elements which represent
    serializations.

    Each modeled serialization mechanism specializes the generic
    functions:

    * `name'                            [generic function]

    * `wire-type'                       [generic function]
    * `offset-type'                     [generic function]
    * `length-type'                     [generic function]

    * `validate-type'                   [generic function]"))
