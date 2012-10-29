;;; conditions.lisp --- Conditions used in the model.data module.
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

(define-condition data-type-error (error)
  ((type :initarg  :type
	 :reader   data-type-error-type
	 :documentation
	 ""))
  (:documentation
   "This error condition class is intended to used as a superclass for
type-related error condition classes."))

(define-condition child-error (data-type-error)
  ((key :initarg  :key
	:reader   data-type-error-key
	:documentation
	""))
  (:report
   (lambda (condition stream)
     (format stream "~@<Child key ~S caused an error within type ~S.~@:>"
	     (data-type-error-key  condition)
	     (data-type-error-type condition))))
  (:documentation
   "This class serves as a superclass for errors related to composite
types and their children."))

(define-condition no-such-child (child-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<The requested child type ~S could not be found ~
within the type ~S.~@:>"
	     (data-type-error-key  condition)
	     (data-type-error-type condition))))
  (:documentation
   "This error is signaled when a requested child type cannot be found
within the specified container type."))

(define-condition duplicate-child-key (child-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<The child key ~S is already in use within type
~S.~@:>"
	     (data-type-error-key  condition)
	     (data-type-error-type condition))))
  (:documentation
   "This error is signaled when a attempt is made to add child to a
composite structure using a key that which is already in use."))
