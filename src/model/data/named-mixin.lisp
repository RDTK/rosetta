;;; named-mixin.lisp --- A mixin class for named data type classes.
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

(in-package :rosetta.model.data)

(defclass named-mixin ()
  ((name :initarg  :name
	 :type     string
	 :reader   data-type-name
	 :documentation
	 "Stores the name of the data type."))
  (:documentation
   "This class is intended to be mixed into data type classes
instances of which represent named data types."))

(defmethod print-items append ((object named-mixin))
  (list (list :name (data-type-name object) "~S")))
