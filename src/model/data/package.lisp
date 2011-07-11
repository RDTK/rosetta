;;; package.lisp --- Package definition for model.data module.
;;
;; Copyright (C) 2011 Jan Moringen
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

(in-package :cl-user)

(defpackage :rosetta.model.data
  (:use
   :cl
   :alexandria

   :rosetta)

  ;; Data type protocol
  (:export
   :data-type-name
   :data-type-documentation)

  ;; Composite data type protocol
  (:export
   :data-type-composite?
   :data-type-parent
   :composite-children
   :composite-child)

  ;; Field protocol
  (:export
   :field-name
   :field-type
   :field-optional?)

  ;; `named-mixin' mixin class
  (:export
   :named-mixin)

  (:documentation
   "This package contains protocols and classes which can be used to
define data-type classes for use with the cl-rosetta backend. In
Addition, some classes for common data-types are included."))
