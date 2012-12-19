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

(defun ensure-values (enum expected)
  (ensure-same (mapcar #'(lambda (value)
			   (list (name value) (value value)))
		       (contents enum :value))
	       expected
	       :test (rcurry #'set-equal :test #'equal)))

(addtest (model-data-enum-root
          :documentation
	  "Test constructing `enum' instances.")
  construction

  (ensure-cases ((initargs expected))
      `(;; Invalid
	(()                                  error)       ; missing :name
	((:name "foo" :values "a")           error)       ; :values type error
	((:name "foo" :values 1)             error)       ; :values type error
	((:name "foo" :values ("a" -1))      value-invalid-for-type)
	((:name "foo" :values ("a" 500))     value-invalid-for-type)
	((:name "foo" :values ("a" 1 "a" 2)) child-error) ; duplicate name
	((:name "foo" :values ("a" 1 "b" 1)) child-error) ; duplicate value

	;; Valid
	((:name "foo")                       nil)
	((:name "foo" :values ("a" 1))       (("a" 1)))
	((:name "foo" :values ("a" 1 "b" 2)) (("a" 1) ("b" 2))))

    (let+ (((&flet do-it ()
	      (apply #'make-instance 'enum
		     :type (make-instance 'type-uint8)
		     initargs))))
      (case expected
	(error
	 (ensure-condition 'error (do-it)))
	(child-error
	 (ensure-condition 'child-error (do-it)))
	(value-invalid-for-type
	 (ensure-condition 'value-invalid-for-type (do-it)))
	(t
	 (ensure-values (do-it) expected))))))

(addtest (model-data-enum-root
          :documentation
	  "Test methods on `lookup' for class `enum'.")
  lookup

  (ensure-cases (args expected)
      `((("a")                                    "a")
	(("a" :if-does-not-exist nil)             "a")
	((1)                                      "a")
	((1 :if-does-not-exist nil)               "a")
	(("b")                                    "b")
	(("b" :if-does-not-exist nil)             "b")
	((255)                                    "b")
	((255 :if-does-not-exist nil)             "b")
	(("no-such-child")                        no-such-child)
	(("no-such-child" :if-does-not-exist nil) nil)
	((2)                                      no-such-child)
	((2 :if-does-not-exist nil)               nil))

    (let+ ((enum (make-instance 'enum
				:name   "enum"
				:type   (make-instance 'type-uint8)
				:values '("a" 1 "b" 255)))
	   ((&flet do-it ()
	      (apply #'lookup enum :value args))))
     (case expected
       (no-such-child (ensure-condition 'no-such-child (do-it)))
       (t             (ensure-same (name (do-it)) expected
				   :test #'string=))))))

(addtest (model-data-enum-root
          :documentation
	  "Test methods on `(setf lookup)' for class `enum'.")
  setf-lookup

  (ensure-cases (type names-and-values expected)
      `(;; Invalid
	(type-uint8 ("a" -1)      child-error) ; -1 invalid for uint8
	(type-uint8 ("a" 500)     child-error) ; 500 invalid for uint8
	(type-uint8 ("a" 1 "a" 2) child-error) ; duplicate child name
	(type-uint8 ("a" 1 "b" 1) child-error) ; duplicate child value

	;; OK
	(type-uint8 ("a" 1)       (("a" 1)))
	(type-uint8 ("a" 1 "b" 2) (("a" 1) ("b" 2))))

    (let+ ((enum (make-instance 'enum
				:name "foo"
				:type (make-instance type)))
	   ((&flet do-it ()
	      (iter (for (name value) on names-and-values :by #'cddr)
		    (setf (lookup enum :value name)
			  (make-instance 'enum-value
					 :name  name
					 :type  (make-instance 'type-int32)
					 :value value)))
	      enum)))
      (case expected
	(child-error (ensure-condition 'child-error (do-it)))
	(t           (ensure-values (do-it) expected))))))

(addtest (model-data-enum-root
          :documentation
	  "Test method on `validate-value'.")
  validate-value

  (let ((enum (make-instance 'enum
			     :name   "enum"
			     :type   (make-instance 'type-uint8)
			     :values '("a" 1 "b" 255))))
    (ensure-cases (value expected)
	'(;; These are not valid.
	  (:|c| nil)

	  ;; These are valid.
	  (:|a| t)
	  (:|b| t))

      (validate-value enum value :if-invalid nil))))
