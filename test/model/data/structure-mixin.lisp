;;; structure-mixin.lisp --- Unit tests for the structure-mixin class.
;;
;; Copyright (C) 2011, 2012, 2013 Jan Moringen
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
  ()
  (:documentation
   "Test suite for the `structure-mixin' class."))

(addtest (structure-mixin-root
          :documentation
	  "Test constructing `structure-mixin' instances.")
  construction

  (ensure-cases ((args expected))
      `(((:fields 5)                                       type-error)
	((:fields #("bla"))                                type-error)
	((:fields ("a" ,+utf-8-string+))                   ("a"))
	((:fields (,(make-instance 'base-field
				   :name "g"
				   :type +utf-8-string+))) ("g")))
    (let+ (((&flet do-it ()
	      (apply #'make-instance 'structure-mixin args))))
      (case expected
	(type-error (ensure-condition 'type-error (do-it)))
	(t          (ensure-same
		     (map 'list #'name (contents (do-it) :field))
		     expected
		     :test (rcurry #'set-equal :test #'string=)))))))

(addtest (structure-mixin-root
          :documentation
	  "Test the `composite-child' method specialization
`structure-mixin'")
  lookup

  (ensure-cases (struct args expected)
      `((,+struct/simple+ ("a")                                    ,(first (contents +struct/simple+ :field)))
	(,+struct/simple+ ("a" :if-does-not-exist nil)             ,(first (contents +struct/simple+ :field)))
	(,+struct/simple+ ("no-such-child")                        no-such-child)
	(,+struct/simple+ ("no-such-child" :if-does-not-exist nil) nil))
    (let+ (((&flet do-it () (apply #'lookup struct :field args))))
      (case expected
	(no-such-child (ensure-condition 'no-such-child (do-it)))
	(t             (ensure-same (do-it) expected :test #'eq))))))
