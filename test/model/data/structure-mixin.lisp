;;; structure-mixin.lisp --- Unit tests for the structure-mixin class.
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(in-package :rosetta.model.data.test)

(deftestsuite structure-mixin-root (model-data-root)
  ((simple-child (make-instance
		  'field-mixin
		  :name "f"
		  :type 'string))
   (simple-type))
  (:setup
   (setf simple-type (make-instance
		      'structure-mixin
		      :fields `("a" ,simple-child))))
  (:documentation
   "Test suite for the `structure-mixin' class."))

(addtest (structure-mixin-root
          :documentation
	  "Test constructing `structure-mixin' instances.")
  construction

  (ensure-cases ((args expected-children))
      `(((:fields 5)
	 :error)
	((:fields #("bla"))
	 :error)
	((:fields ("a" ,(make-instance 'field-mixin
				       :name "f"
				       :type 'string)))
	 ("f"))
	((:fields (,(make-instance 'field-mixin
				   :name "g"
				   :type 'string)))
	 ("g")))

    (if (eq expected-children :error)
	(ensure-condition 'type-error
	  (apply #'make-instance 'structure-mixin args))

	(let ((composite (apply #'make-instance 'structure-mixin args)))
	  (ensure-same (map 'list #'data-type-name
			    (composite-children composite))
		       expected-children
		       :test (rcurry #'set-equal :test #'string=))))))

(addtest (structure-mixin-root
          :documentation
	  "Test the `composite-child' method specialization
`structure-mixin'")
  composite-child

  (ensure-cases (args expected)
      `((("a")                         ,simple-child)
	(("a" :error? nil)             ,simple-child)
	(("no-such-child")             :error)
	(("no-such-child" :error? nil) nil))

    (if (eq expected :error)
	(ensure-condition 'no-such-child
	  (apply #'composite-child simple-type args))
	(ensure-same (apply #'composite-child simple-type args)
		     expected
		     :test #'eq))))
