;;; type-enum.lisp --- Representation of basic enum types.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the GNU Lesser General
;; Public License Version 3 (the ``LGPL''), or (at your option) any
;; later version.
;;
;; Software distributed under the License is distributed on an ``AS
;; IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the LGPL for the specific language governing rights
;; and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html or
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

(cl:in-package :rosetta.model.data)


;;; `base-enum-value' class
;;

(defclass enum-value (named-mixin ;; named-component-mixin
		      singleton ;;; TODO(jmoringe, 2012-05-03): singleton-mixin
		      documentation-mixin
		      print-items-mixin)
  ()
  (:default-initargs
   :type (make-instance 'type-uint32))
  (:documentation
   "Instances of this class represent enumeration values consisting of
a name and an associated, usually numeric, value."))


;;; `base-enum' class
;;

(define-composite-mixin value
    :class-name values-mixin)

(defclass enum (named-mixin
		parented-mixin
		typed-mixin
		ordered-mixin
		values-mixin
		;; toplevel-mixin
		documentation-mixin
		print-items-mixin)
  ()
  (:documentation
   "Instances of this class represent enum data types, that is a named
collection of value objects each associated a name and a, usually
numeric, value.

One example of a class representing such values is `enum-value'."))

(defmethod shared-initialize :after ((instance   enum)
                                     (slot-names t)
                                     &key
				     values)
  (etypecase values
    ((cons (or string keyword))
     (iter (for (name value) on values :by #'cddr)
	   (setf (lookup instance :value name)
		 (make-instance 'enum-value
				:name  name
				:type  (type1 instance)
				:value value))))
    (list
     (iter (for value in values)
	   (setf (lookup instance :value (name value)) value)))))

(defmethod kind ((type enum))
  :enum)

(defmethod (setf lookup) :before ((new-value t)
				  (container enum)
				  (kind      (eql :value))
				  (key       string)
				  &key &allow-other-keys)
  ;; Make sure that we can obtain a numeric value from
  ;; NEW-VALUE. Otherwise, reject it.
  (unless (compute-applicable-methods #'value (list new-value))
    (simple-child-error container new-value
			"~@<Supplied value ~A for ~A does not ~
specialize the ~S method.~@:>"
			new-value container 'value))

  (let ((value (value new-value)))
    ;; Make sure we do not already have the numeric value associated
    ;; to NEW-VALUE.
    (when-let ((existing (find value (contents container :value)
			       :key #'value)))
      (duplicate-child-key container (list :value value) existing))

    ;; Finally make sure the numeric value associated to NEW-VALUE is
    ;; valid for the numeric type used by CONTAINER.
    (let+ (((&values valid? cause)
	    (validate-value (type1 container) value :if-invalid nil)))
      (unless valid?
	(chainable-child-error container key cause)))))

(defmethod validate-value ((type enum) (value t)
			   &key &allow-other-keys)
  (unless (find value (contents type :value)
		:key #'value)
    (let ((values (mapcar #'value (contents type :value))))
     (error "~@<~S is not one of the valid values of ~A. ~[There are ~
no valid values~;The only valid value is ~:;Valid values are ~]~{~S~^, ~
~}.~@:>"
	    value type (length values) values)))

  (when (next-method-p)
    (call-next-method)))
