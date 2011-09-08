;;; package.lisp --- Package definition for unit tests of the model.data module.
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

(cl:defpackage :rosetta.model.data.test
  (:use
   :cl
   :alexandria
   :lift

   :rosetta
   :rosetta.model.data

   :rosetta.test
   :rosetta.model.test)

  (:documentation
   "This package contains unit tests for the model.data module."))

(cl:in-package :rosetta.model.data.test)

(deftestsuite model-data-root (model-root)
  ()
  (:documentation
   "Root unit test suite for the model.data module."))
