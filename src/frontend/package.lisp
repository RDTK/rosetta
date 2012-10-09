;;; package.lisp --- Package definition for frontend module.
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

(cl:defpackage :rosetta.frontend
  (:nicknames
   :rs.f)

  (:use
   :cl
   :alexandria
   :iterate
   :let-plus
   :more-conditions

   :rosetta.model.data)

  ;; Conditions
  (:export
   :location-condition
   :location

   :builder-condition
   :builder

   :parse-error1

   :parse-warning

   :processing-error

   :processing-warning

   :dependency-error
   :dependency-error-dependency

   :cannot-resolve-dependency
   :dependency-error-locations)

  ;; Location protocol
  (:export
   :source
   :source-content
   :bounds
   :line
   :column)

  ;; Location repository protocol
  (:export
   :location-of)

  ;; Location Utilities
  (:export
   :location-info

   :format-location
   :format-content)

  ;; Parse protocol
  (:export
   :parse)

  ;; Format class family
  (:export
   :no-such-format-class
   :find-format-class
   :format-classes)

  ;; `binary-format-mixin' mixin class
  (:export
   :binary-format-mixin)

  ;; `text-format-mixin' mixin class
  (:export
   :text-format-mixin)

  ;; Comment attaching protocol
  (:export
   :most-recent-comment
   :comment
   :comment?)

  ;; Dependency resolution protocol
  (:export
   :resolve)

  (:documentation
   "This package contains frontend-related protocols and
infrastructure of the rosetta compiler.

In particular the `parse' generic function and function and some
default methods are contained in this package."))
