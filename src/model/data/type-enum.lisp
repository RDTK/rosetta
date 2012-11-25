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

(defmethod kind ((type enum))
  :enum)

(defmethod (setf lookup) :before ((new-value t)
				  (container enum)
				  (kind      (eql :value))
				  (key      string)
				  &key &allow-other-keys)
  (unless (compute-applicable-methods #'value (list new-value))
    (error "~@<Supplied value ~A for ~A does not specialize the ~S method.~@:>"
	   new-value container 'value)))
