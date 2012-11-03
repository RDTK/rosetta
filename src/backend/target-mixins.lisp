;;; target-mixins.lisp --- Mixins for target classes.
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

(cl:in-package :rosetta.backend)


;;; `code-generating-target-mixin' mixin class
;;

(defclass code-generating-target-mixin ()
  ((optimization-settings :initarg  :optimization-settings
			  :type     list
			  :accessor optimization-settings
			  :initform nil
			  :documentation
			  "Optimization settings that should be used
when generating code."))
  (:documentation
   "This class can be used as a superclass for target classes that
represent a code generation target."))


;;; `method-target-mixin' mixin class
;;

(defclass method-target-mixin (code-generating-target-mixin)
  ((body-target :initarg  :body-target
		:reader   body-target
		:documentation
		"Stores a target object that should be used to
generate the method body."))
  (:default-initargs
   :body-target (missing-required-initarg
		 'method-target-mixin :body-target))
  (:documentation
   "This class is intended to be mixed into target classes which emit
methods."))

(defmethod mechanism ((target method-target-mixin))
  (mechanism (body-target target)))

(defmethod emit ((node     t)
		 (target   method-target-mixin)
		 (language t)
		 &key)
  (generate node (body-target target) language))


;;; `mechanism-target-mixin' mixin class
;;

(defclass mechanism-target-mixin ()
  ((mechanism :initarg  :mechanism
	      :reader   mechanism
	      :documentation
	      "Stores the name of the mechanism class for which the
serialization-related code is `emit' ted."))
  (:default-initargs
   :mechanism (missing-required-initarg
	       'mechanism-target-mixin :mechanism))
  (:documentation
   "This class is intended to be mixed into target classes which are
used for generating serialization-related code. It provides a slot for
storing the mechanism for which the serialization code is `emit'
ted."))
