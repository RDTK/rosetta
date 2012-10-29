;;; protocol.lisp --- Protocol functions for serialization model elements.
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

(cl:in-package :rosetta.model.serialization)


;;; Mechanism protocol
;;

(defgeneric name (mechanism)
  (:documentation
   "Return the name of the serialization mechanism MECHANISM."))

(defgeneric wire-type (mechanism)
  (:documentation
   "Return the wire-type of the serialization mechanism MECHANISM. The
returned object models a type."))

(defgeneric offset-type (mechanism)
  (:documentation
   "Return the type used by the serialization mechanism MECHANISM to
represent offsets in binary buffers. The returned object models a
type."))

(defgeneric length-type (mechanism)
  (:documentation
   "Return the type used by the serialization mechanism MECHANISM to
represent lengths of arrays. The returned object models a type."))

(defgeneric validate-type (mechanism type
			   &key
			   if-invalid)
  (:documentation
   "Check whether the type represented by TYPE can be processed by the
serialization mechanism represented by MECHANISM. Return non-nil, if
TYPE can be processed by MECHANISM.

IF-INVALID controls the behavior in case TYPE cannot be processed by
MECHANISM. Valid values are nil or a function which can be called with
a condition object."))

(defmethod name ((mechanism t))
  (class-name (class-of mechanism)))

(defmethod validate-type :around ((mechanism t)
				  (type      t)
				  &key
				  (if-invalid #'error))
  (or (call-next-method)
      (etypecase if-invalid
	(null
	 nil)
	(function
	 (restart-case
	     (funcall if-invalid
		      (make-condition
		       'simple-error
		       :format-control   "~@<Type ~A is invalid for ~
mechanism ~A.~@:>"
		       :format-arguments (list type mechanism)))
	   (continue ()
	     :report (lambda (stream)
		       (format stream "~@<Ignore the incompatibility ~
and try to continue.~@:>"))
	     t))))))

(defmethod validate-type ((mechanism t)
			  (type      t)
			  &key &allow-other-keys)
  nil)

(defmethod validate-type ((mechanism t)
			  (type      composite-mixin)
			  &key &allow-other-keys)
  (every (curry #'validate-type mechanism)
	 (contents type t)))

(defmethod validate-type ((mechanism t)
			  (type      typed-mixin)
			  &key &allow-other-keys)
  (validate-type mechanism (type1 type)))


;;; Mechanisms
;;

(intern "MECHANISM") ;; for (documentation :MECHANISM 'rosetta.serialization:mechanism)

(dynamic-classes:define-findable-class-family mechanism
    "This family consists of serialization mechanism classes. Each
serialization mechanism class represents a serialization mechanism
with respect to its principal properties. Instances may in addition
store specific parameters of a serialization mechanism \(like
indentation of output for an XML-based mechanism).")

(defmethod documentation ((thing symbol) (type (eql 'mechanism)))
  "Obtain documentation of type MECHANISM from the mechanism class
designated by THING."
  (documentation (find-mechanism-class thing) t))
