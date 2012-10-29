;;; conditions.lisp --- Conditions used in the serialization module of rosetta.
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

(cl:in-package :rosetta.serialization)

(define-condition serialization-error (error)
  ((mechanism   :initarg  :mechanism
		:type     standard-object
		:reader   serialization-error-mechanism
		:documentation
		"The serialization mechanism that was used in the
failed serialization of deserialization operation.")
   (source      :initarg  :source
		:reader   serialization-error-source
		:documentation
		"The source of the failed serialization or
deserialization operation.")
   (destination :initarg  :destination
		:reader   serialization-error-destination
		:documentation
		"The destination of the failed serialization or
deserialization operation."))
  (:report
   (lambda (condition stream)
     (format stream "~@<An error occurred when serializing ~A to ~A ~
using mechanism ~A.~@:>"
	     (serialization-error-source      condition)
	     (serialization-error-destination condition)
	     (serialization-error-mechanism   condition))))
  (:documentation
   "Errors of this type and subtypes when a serialization or
deserialization operation fails."))

(define-condition pack-error (serialization-error)
  ()
  (:documentation
   "Conditions of this type and subtypes are signaled when an error
occurs during packing \(serialization)."))

(define-condition unpack-error (serialization-error)
  ()
  (:documentation
   "Conditions of this type and subtypes are signaled when an error
occurs during unpacking \(deserialization)."))
