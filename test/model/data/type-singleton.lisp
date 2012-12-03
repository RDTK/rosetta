;;; type-singleton.lisp ---
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

(deftestsuite model-data-singleton-root (model-data-root)
  ()
  (:documentation
   "Unit tests for the `singleton' class."))

(addtest (model-data-singleton-root
          :documentation
	  "Test constructing instances of class `singleton'.")
  construction

  (ensure-cases (initargs expected-value)
      `(;; These are invalid.
	(()                                              missing-required-initarg)
	((:type ,(make-instance 'type-uint8) :value -1)  value-invalid-for-type)

	;; These are valid.
	((:type ,(make-instance 'type-uint8) :value 1)   1)
	((:type ,(make-instance 'type-uint8) :value 255) 255))

    (let+ (((&flet do-it ()
	      (value (apply #'make-instance 'singleton initargs)))))
      (case expected-value
	(missing-required-initarg
	 (ensure-condition 'missing-required-initarg (do-it)))
	(value-invalid-for-type
	 (ensure-condition 'value-invalid-for-type (do-it)))
	(t
	 (ensure-same (do-it) expected-value))))))

(addtest (model-data-singleton-root
          :documentation
	  "Test method on `validate-value' for class `singleton'.")
  validate-value

  (ensure-cases (initargs value expected)
      `(;; These are invalid.
	((:type ,(make-instance 'type-uint8) :value 1)   -1   nil)
	((:type ,(make-instance 'type-uint8) :value 1)   2    nil)

	;; These are valid.
	((:type ,(make-instance 'type-uint8) :value 1)   1    t)
	((:type ,(make-instance 'type-uint8) :value 255) 255  t))

    (ensure-same (validate-value
		  (apply #'make-instance 'singleton initargs) value
		  :if-invalid nil)
		 expected)))
