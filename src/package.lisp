;;; package.lisp --- Package definition for util module.
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

(cl:defpackage :rosetta
  (:use
   :cl
   :let-plus)

  ;; Conditions
  (:export
   :missing-required-initarg
   :missing-required-initarg-class

   :missing-required-argument
   :missing-required-argument-parameter)

  ;; print-items mechanism and mixin class
  (:export
   :print-items

   :print-items-mixin)

  (:documentation
   "This package contains generic facilities like unspecific
conditions used within cl-rosetta."))
