;;; package.lisp --- Package definition for frontend module.
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

(defpackage :rosetta.frontend
  (:use
   :cl
   :alexandria
   :metabang-bind)

  (:export
   :parse)

  (:export
   :no-such-format-class
   :find-format-class
   :format-classes)

  (:documentation
   "This package contains frontend-related protocols and
infrastructure of the cl-rosetta compiler.

In particular the `parse' generic function and function and some
default methods are contained in this package."))
