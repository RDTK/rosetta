;;; type-singleton.lisp --- Singleton data type.
;;
;; Copyright (C) 2012 Jan Moringen
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

(cl:in-package :rosetta.model.data)

;;; TODO(jmoringe, 2012-05-03): mixin?
(defclass singleton (typed-mixin
		     print-items-mixin)
  ((value :initarg  :value
	  :reader   value
	  :documentation
	  "Stores the singleton value."))
  (:default-initargs
   :value (missing-required-initarg 'singleton :value))
  (:documentation
   "Instances of this type class represent types the extension of
which consist of singleton values."))

(defmethod shared-initialize :after ((instance   singleton)
                                     (slot-names t)
                                     &key)
  (let+ (((&accessors-r/o (type type1) value) instance)) ;;; TODO(jmoringe, 2012-05-04): temp
	(unless (typep value t #+later (->lisp-type type))
	  (error 'type-error
		 :datum         value
		 :expected-type type))))

(defmethod print-items append ((type singleton))
  (list (list :value (value type))))
