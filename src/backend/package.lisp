;;; macros.lisp --- Package definition for the backend module.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

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

   :rosetta.model.data

   :rosetta.model.language)

  ;; Conditions
  (:export)

  ;; Generic emitter
  (:export
   :emit)

  ;; Context
  (:export
   :*context*

   :context
   :context-target
   :context-language
   :context-stack
   :context-package
   :context-get

   :context-get)

  ;; Targets
  (:export
   :no-such-target
   :find-target-class
   :target-classes)

  ;; Languages
  (:export
   :no-such-language
   :find-language-class
   :language-classes)

  (:export
   :code-generating-target-mixin)

  ;; Serialization-related target classes
  (:export
   :serializtion-mixin
   :target-mechanism

   :target-packed-size :target-pack :target-unpack

   :target-location
   :target-extract)

  ;; Macros
  (:export
   :define-target
   :define-mechanism-target
   :define-mechanism-target/method
   :define-mechanism-targets

   :with-emit-symbols
   :parent
   :grandparent
   :ancestors
   :recur

   ;; let-plus
   :&env :&env-r/o)

  (:documentation
   "This package contains backend-related protocols and infrastructure
of the rosetta compiler."))
