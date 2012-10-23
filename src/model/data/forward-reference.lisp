;;; forward-reference.lisp --- Representation of not-yet-defined things.
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

(defclass forward-reference (print-items-mixin)
  ((kind :initarg  :kind
	 :accessor kind
	 :documentation
	 "Stores a description of the kind of the object represented
by this forward reference.")
   (args :initarg  :args
	 :type     list
	 :accessor args
	 :documentation
	 "Stores the arguments that would have been passed to the
object represented by this forward reference."))
  (:default-initargs
   :kind (missing-required-initarg 'forward-reference :kind)
   :args (missing-required-initarg 'forward-reference :args))
  (:documentation
   "Instances of this class represent unresolved references to model
objects.

They store all information that was available when the resolution of
the reference failed. This information can later be used to resolve
the reference and eventually replace (via `change-class') the forward
reference object with the desired object."))

(defmethod name ((thing forward-reference))
  (format nil "F<~A~[ ~A~]>" (kind thing) (getf (args thing) :name)))

(defmethod qname ((thing forward-reference))
  (list :absolute (name thing)))

(defmethod upgrade! ((instance forward-reference)
		     (other    t))
  (let ((new-class (class-of other)))
    (apply #'change-class instance new-class nil)
    (iter (for slot in (closer-mop:class-slots new-class))
	  (let ((name (closer-mop:slot-definition-name slot)))
	    (setf (slot-value instance name)
		  (slot-value other name))))
    instance))

(defmethod print-items append ((object forward-reference))
  (list (list :kind (kind object))
	(list :name (getf (args object) :name) "~@[ ~A~]")))
