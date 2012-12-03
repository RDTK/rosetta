;;; type-enum.lisp --- Unit tests for the enum data type.
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

(cl:in-package :rosetta.model.data.test)

(deftestsuite model-data-enum-root (model-data-root)
  ()
  (:documentation
   "Tests for the `enum' and `enum-value' types."))

(addtest (model-data-enum-root
          :documentation
	  "Test method on `validate-value'.")
  validate-value

  (let ((enum (make-instance 'enum
			     :name "enum"
			     :type (make-instance 'type-uint8))))
    (setf (lookup enum :value "a")
	  (make-instance 'enum-value
			 :name  "a"
			 :value 1))
    (ensure-cases (value expected)
	'((-1 nil)
	  ( 0 nil)
	  ( 1 t)
	  ( 2 nil))

      (validate-value enum value :if-invalid nil))))
