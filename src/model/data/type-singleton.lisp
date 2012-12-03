;;; type-singleton.lisp --- Singleton data type.
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
                                     &key
				     value)
  (validate-value (type1 instance) value))

(defmethod kind ((type singleton))
  :singleton)

(defmethod name ((type singleton))
  (format nil "=~A" (value type)))

(defmethod validate-value ((type singleton) (value t)
			   &key &allow-other-keys)
  (equal value (value type)))

(defmethod print-items append ((type singleton))
  (list (list :value (value type))))
