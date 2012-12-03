;;; type-fundamental.lisp --- Unit tests for fundamental types.
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

(deftestsuite model-data-fundamental-root (model-data-root)
  ()
  (:documentation
   "Unit tests for fundamental data types."))

(addtest (model-data-fundamental-root
          :documentation
	  "Tests methods on `validate-value' for fundamental data
types.")
  validate-value

  (ensure-cases (type value expected)
      '(;; Boolean type
	(type-bool         :foo   nil) ; not valid

	(type-bool         nil    t)   ; valid
	(type-bool         t      t)

	;; Integer types
	(type-uint8        "foo"  nil) ; not valid
	(type-uint8        :foo   nil)
	(type-uint8        -1     nil)
	(type-uint8        256    nil)

	(type-uint8        0      t)   ; valid
	(type-uint8        255    t)
	(type-int8         -1     t)

	;; Float types
	(type-float32      "foo"  nil) ; not valid
	(type-float32      :foo   nil)
	(type-float32      1      nil)
	(type-float32      -1     nil)
	(type-float32      1/2    nil)

	(type-float32      1.0d0  t)   ; valid
	(type-float32      -1.0d0 t)
	(type-float32      0.5d0  t)

	;; String types
	(type-ascii-string "ä"    nil) ; not valid

	(type-ascii-string "a"    t)   ; valid
	(type-utf-8-string "ä"    t))

    (ensure-same (validate-value (make-instance type) value
				 :if-invalid nil)
		 expected)))
