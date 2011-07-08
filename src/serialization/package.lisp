;;; package.lisp --- Package definition for the serialization module.
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

(defpackage :rosetta.serialization
  (:nicknames
   :rs.s)

  (:use
   :cl
   :alexandria
   :metabang-bind
   :iterate)

  ;; Conditions
  (:export
   :serialization-error
   :serialization-error-mechanism
   :serialization-error-source
   :serialization-error-destination

   :pack-error

   :unpack-error)

  ;; Serialization and deserialization protocol
  (:export
   :packed-size

   :pack :unpack
   :pack*)

  ;; Partial deserialization protocol
  (:export
   :offset
   :extract)

  ;; Mechanism class family
  (:export
   :no-such-mechanism-class
   :find-mechanism-class
   :mechanism-classes)

  (:documentation
   "This package contains protocols and functionality related to
serialization and deserialization using different serialization
mechanisms."))
