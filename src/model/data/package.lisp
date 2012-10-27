;;; package.lisp --- Package definition for model.data module.
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

(cl:defpackage :rosetta.model.data
  (:nicknames
   :rs.m.d)

  (:use
   :cl
   :alexandria
   :iterate
   :let-plus
   :more-conditions

   :rosetta)

  ;; Conditions
  (:export
   :data-type-error
   :data-type-error-type

   :child-error
   :data-type-error-key

   :no-such-child

   :duplicate-child-key)

  ;; Name protocol
  (:export
   :kind

   :name
   :qname)

  ;; Documentation protocol
  (:export
   :documentation1)

  ;; Composition protocol
  (:export
   :contents
   :lookup
   :query

   :parent
   :ancestors
   :root

   :composite?)

  ;; Storage protocol
  (:export
   :fixed-size?)

  ;; Typed protocol
  (:export
   :type1)

  ;; Fundamental type protocol
  (:export
   :fundamental?
   :category
   :width
   :signed?
   :encoding)

  ;; Field protocol
  (:export
   :optional?)

  ;; Array protocol
  (:export
   :element-type
   :index-type)

  ;; Singleton protocol
  (:export
   :value)

  ;; Mapping protocol
  (:export
   :data-holder
   :wire-schema)

  ;; `named-mixin' mixin class
  (:export
   :named-mixin)

  ;; `fundamental-type-mixin' and `*-with-mixin' classes
  (:export
   :fundamental-type-mixin

   :fixed-width-mixin
   :variable-width-mixin)

  ;; `composite-mixin' mixin class
  (:export
   :composite-mixin)

  ;; `typed-mixin' mixin class
  (:export
   :typed-mixin)

  ;; `field-mixin' mixin class
  (:export
   :field-mixin)

  ;; `structure-mixin' mixin class
  (:export
   :structure-mixin)

  ;; `array-mixin' mixin class
  (:export
   :array-mixin)

  ;; `toplevel-mixin'
  (:export
   :toplevel-mixin)

  ;; `mapping' class
  (:export
   :mapping)

  ;; Data type class
  (:export
   :type-bool

   :type-uint8 :type-uint16 :type-uint32  :type-uint64
   :type-int8  :type-int16  :type-int32   :type-int64

   :type-float*
   :type-float32 :type-float64

   :type-string*
   :type-ascii-string
   :type-utf-8-string

   :type-octet-vector

   :enum-value
   :enum

   :base-field
   :base-structure

   :base-array

   :singleton)

  ; Forward reference protocol and class
  (:export
   :upgrade!

   :forward-reference)

  (:documentation
   "This package contains protocols and classes which can be used to
define data-type classes for use with the rosetta backend. In
Addition, some classes for common data-types are included."))
