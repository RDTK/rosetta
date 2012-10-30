;;; conditions.lisp --- Conditions used by the model.serialization module.
;;
;; Copyright (C) 2012 Jan Moringen
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

(define-condition type-invalid-for-mechanism (error
					      chainable-condition)
  ((mechanism :initarg  :mechanism
	      :reader   mechanism
	      :documentation
	      "Stores the mechanism for which the data type was
invalid.")
   (type      :initarg  :type
	      :reader   type1
	      :documentation
	      "Stores the type that was invalid for the mechanism."))
  (:default-initargs
   :mechanism (missing-required-initarg 'type-invalid-for-mechanism :mechanism)
   :type      (missing-required-initarg 'type-invalid-for-mechanism :type))
  (:report
   (lambda (condition stream)
     (format stream "~@<Type ~A is invalid for mechanism ~
~A~/more-conditions::maybe-print-cause/~@:>"
	     (type1     condition)
	     (mechanism condition)
	     condition)))
  (:documentation
   "This error is signaled when an attempt is made to use a data-type
in conjunction with a mechanism for which the data-type is not
suitable."))
