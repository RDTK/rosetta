;;; data-holder-mixin.lisp --- Mixin class for mechanisms that use data-holders.
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

(in-package :rosetta.serialization)

(defclass data-holder-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into mechanism classes that
unpack into data-holder instances \(as opposed to schema-free
mechanisms which could, for example, return nested lists)."))

(defmethod unpack ((mechanism   data-holder-mixin)
		   (source      t)
		   (destination class)
		   &rest args &key)
  "Make an instance of the class DESTINATION and load SOURCE into the
instance."
  (apply #'unpack mechanism source (make-instance destination) args))

(defmethod unpack ((mechanism   data-holder-mixin)
		   (source      t)
		   (destination symbol)
		   &rest args &key)
  "Make an instance of the class designated by DESTINATION and load
SOURCE into the instance."
  (apply #'unpack mechanism source (find-class destination) args))
