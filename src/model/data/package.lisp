;;; package.lisp --- Package definition for model.data module.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the GNU Lesser General
;; Public License Version 3 (the ``LGPL''), or (at your option) any
;; later version.
;;
;; Software distributed under the License is distributed on an ``AS
;; IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the LGPL for the specific language governing rights
;; and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html or
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

(cl:defpackage :rosetta.model.data
  (:nicknames
   :rs.m.d)

  (:use
   :cl
   :alexandria
   :iterate
   :let-plus
   :more-conditions

   :rosetta
   :rosetta.model)

  ;; Conditions
  (:export
   :data-type-error
   :data-type-error-type

   :value-invalid-for-type ; condition and function
   :data-type-error-value

   :child-error
   :data-type-error-key

   :chainable-child-error  ; condition and function

   :simple-child-error     ; condition and function

   :no-such-child          ; condition and function

   :duplicate-child-key)   ; condition and function

  ;; Restarts
  (:export
   :upgrade)

  ;; Documentation protocol
  (:export
   :documentation1)

  ;; Composition protocol
  (:export
   :contents
   :contents/plist

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

  ;; Value validation protocol
  (:export
   :validate-value)

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

  ;; `documentation-mixin' mixin class
  (:export
   :documentation-mixin)

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

   :type-integer*
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

  (:export
   :package1)

  ; Forward reference protocol and class
  (:export
   :upgrade!

   :forward-reference)

  ;; Builder protocol
  (:export
   :find-node
   :make-node
   :add-child)

  ;; Builder class family
  (:export
   :no-such-builder-class
   :find-builder-class
   :builder-classes)

  (:documentation
   "This package contains protocols and classes which can be used to
define data-type classes for use with the rosetta backend. In
Addition, some classes for common data-types are included."))
