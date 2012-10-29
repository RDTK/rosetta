;;; structure-mixin.lisp --- Unit tests for the structure-mixin class.
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

(cl:in-package :rosetta.model.data.test)

(deftestsuite structure-mixin-root (model-data-root)
  ((simple-child (make-instance 'field-mixin
				:name "a"
				:type 'string))
   (simple-type))
  (:setup
   (setf simple-type (make-instance 'structure-mixin
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
	((:fields ("a" ,simple-child))
	 ("a"))
	((:fields (,(make-instance 'field-mixin
				   :name "g"
				   :type 'string)))
	 ("g")))

    (if (eq expected-children :error)
	(ensure-condition 'type-error
	  (apply #'make-instance 'structure-mixin args))

	(let ((composite (apply #'make-instance 'structure-mixin args)))
	  (ensure-same (map 'list #'name (contents composite :field))
		       expected-children
		       :test (rcurry #'set-equal :test #'string=))))))

(addtest (structure-mixin-root
          :documentation
	  "Test the `composite-child' method specialization
`structure-mixin'")
  lookup

  (ensure-cases (args expected)
      `((("a")                                    ,simple-child)
	(("a" :if-does-not-exist nil)             ,simple-child)
	(("no-such-child")                        :error)
	(("no-such-child" :if-does-not-exist nil) nil))

    (if (eq expected :error)
	(ensure-condition 'no-such-child
	  (apply #'lookup simple-type :field args))
	(ensure-same (apply #'lookup simple-type :field args)
		     expected
		     :test #'eq))))
