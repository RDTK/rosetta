;;;; macros.lisp --- Package definition for the backend module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage :rosetta.backend
  (:nicknames
   :rs.b)

  (:use
   :cl
   :alexandria
   :split-sequence
   :let-plus
   :iterate
   :more-conditions

   :rosetta
   :rosetta.model
   :rosetta.model.data
   :rosetta.model.language
   :rosetta.model.serialization

   :rosetta.serialization)

  (:shadow
   :generate)

  (:shadowing-import-from :rosetta.model
   :name)

  (:shadowing-import-from :rosetta.model.data
   :type1)

  ;; Conditions
  (:export
   :conversion-condition
   :conversion-condition-from
   :conversion-condition-to

   :conversion-error        :conversion-warning
   :simple-conversion-error :simple-conversion-warning

   :cannot-narrow

   :loss-of-precision

   :context-condition
   :context-condition-context

   :missing-environment-entry
   :missing-environment-entry-name

   :emit-condition
   :emit-condition-node
   :emit-condition-target
   :emit-condition-language

   :emit-error

   :emit-warning)

  ;; Variables
  (:export
   :*context*)

  ;; generate/emit protocol
  (:export
   :generate
   :emit/context
   :emit)

  ;; Context protocol
  (:export
   :context-stack

   :context-node
   :context-target
   :context-language

   :context-environment/alist
   :context-get)

  ;; Target protocol
  (:export
   :make-target-like)

  ;; Target class family
  (:export
   :no-such-target-class
   :find-target-class
   :target-classes)

  (:export
   :code-generating-target-mixin)

  ;; General-purpose target classes
  (:export
   :target-class

   :target-reference

   :target-instantiate
   :target-initargs

   :target-value->code
   :target-code->value

   :target-convert)

  ;; Serialization-related target classes
  (:export
   :mechanism-target-mixin
   :mechanism

   :target-packed-size :target-packed-size/method
   :target-pack        :target-pack/method
   :target-unpack      :target-unpack/method

   :target-location    :target-location/method
   :target-extract     :target-extract/method)

  ;; Macros
  (:export
   :define-target
   :define-target/method
   :define-mechanism-target
   :define-mechanism-target/method
   :define-mechanism-targets

   :with-updated-context
   :with-emit-symbols
   :parent
   :grandparent
   :ancestors
   :recur

   :optmization-case

   ;; let-plus
   :&env :&env-r/o
   :&context)

  (:documentation
   "This package contains backend-related protocols and infrastructure
of the rosetta compiler.

* `generate'                        [generic function]

There is a family of targets which can be manipulated using:

* `no-such-target-class'            [condition]
* `find-target-class'               [generic function]
* `target-classes'                  [function]

The names of some important targets are:

* :class
* :reference

* :packed-size :packed-size/method
* :pack        :pack/method
* :unpack      :unpack/method

See

  (documentation SYMBOL 'rs.b:target)"))
