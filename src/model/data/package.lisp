;;;; package.lisp --- Package definition for model.data module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rosetta.model.data
  (:nicknames
   #:rs.m.d)

  (:use
   #:cl
   #:alexandria
   #:iterate
   #:let-plus
   #:more-conditions

   #:rosetta
   #:rosetta.model)

  ;; Conditions
  (:export
   #:data-type-error
   #:data-type-error-type

   #:value-invalid-for-type ; condition and function
   #:data-type-error-value

   #:child-error
   #:data-type-error-key

   #:chainable-child-error  ; condition and function

   #:simple-child-error     ; condition and function

   #:no-such-child          ; condition and function

   #:duplicate-child-key)   ; condition and function

  ;; Restarts
  (:export
   #:upgrade)

  ;; Documentation protocol
  (:export
   #:documentation1)

  ;; Composition protocol
  (:export
   #:contents
   #:contents/plist

   #:lookup
   #:query

   #:parent
   #:ancestors
   #:root

   #:composite?)

  ;; Storage protocol
  (:export
   #:fixed-size?)

  ;; Typed protocol
  (:export
   #:type1)

  ;; Value validation protocol
  (:export
   #:validate-value)

  ;; Dependency protocol
  (:export
   :direct-dependencies
   :dependencies)

  ;; Fundamental type protocol
  (:export
   #:fundamental?
   #:category
   #:width
   #:signed?
   #:encoding)

  ;; Field protocol
  (:export
   #:optional?)

  ;; Array protocol
  (:export
   #:element-type
   #:index-type)

  ;; Singleton protocol
  (:export
   #:value)

  ;; Mapping protocol
  (:export
   #:data-holder
   #:wire-schema)

  ;; Type mixin classes
  (:export
   #:documentation-mixin

   #:named-mixin

   #:fundamental-type-mixin

   #:fixed-width-mixin
   #:variable-width-mixin

   #:composite-mixin

   #:typed-mixin

   #:field-mixin
   #:structure-mixin

   #:array-mixin

   #:toplevel-mixin)

  ;; `mapping' class
  (:export
   #:mapping)

  ;; Data type classes
  (:export
   #:type-bool

   #:type-integer*
   #:type-uint8 #:type-uint16 #:type-uint32  #:type-uint64
   #:type-int8  #:type-int16  #:type-int32   #:type-int64

   #:type-float*
   #:type-float32 #:type-float64

   #:type-string*
   #:type-ascii-string #:type-utf-8-string

   #:type-octet-vector

   #:enum-value
   #:enum

   #:base-field
   #:base-structure

   #:base-array

   #:singleton

   #:package1)

  ;; Builtin data type objects
  (:export
   #:+bool+

   #:+uint8+ #:+uint16+ #:+uint32+ #:+uint64+
   #:+int8+  #:+int16+  #:+int32+  #:+int64+

   #:+float32+ #:+float64+

   #:+ascii-string+ #:+utf-8-string+

   #:+octet-vector+)

  ; Forward reference protocol and class
  (:export
   #:upgrade!

   #:forward-reference)

  ;; Builder protocol
  (:export
   #:find-node
   #:make-node
   #:add-child)

  ;; Builder class family
  (:export
   #:no-such-builder-class
   #:find-builder-class
   #:builder-classes)

  (:documentation
   "This package contains protocols and classes which can be used to
define data-type classes for use with the rosetta backend. In
Addition, some classes for common data-types are included."))
