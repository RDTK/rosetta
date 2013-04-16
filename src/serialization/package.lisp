;;;; package.lisp --- Package definition for the serialization module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rosetta.serialization
  (:nicknames
   #:rs.s)

  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate

   #:nibbles

   #:rosetta.model.serialization)

  ;; Conditions
  (:export
   #:serialization-error
   #:serialization-error-mechanism
   #:serialization-error-source
   #:serialization-error-destination

   #:pack-error

   #:unpack-error)

  ;; Serialization and deserialization protocol
  (:export
   #:packed-size

   #:pack #:unpack
   #:pack*)

  ;; Partial deserialization protocol
  (:export
   #:location
   #:extract)

  ;; `textual-mixin' mixin class
  (:export
   #:textual-mixin)

  ;; `textual-stream-mixin' mixin class
  (:export
   #:textual-stream-mixin)

  ;; `binary-mixin' mixin class
  (:export
   #:binary-mixin)

  ;; `data-holder' mixin class
  (:export
   #:data-holder-mixin)

  (:documentation
   "This package contains protocols and functionality related to
serialization and deserialization using different serialization
mechanisms."))
