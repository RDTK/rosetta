;;; protocol.lisp --- Tests for the protocol provided by the model.data module.
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

(deftestsuite model-data-protocol-root (model-data-root)
  ()
  (:documentation
   "Unit tests for the protocol of the model.data module."))

(deftestsuite validate-value-root (model-data-protocol-root)
  ()
  (:documentation
   "Tests for the `validate-value' generic function."))

(addtest (validate-value-root
          :documentation
	  "Test default behavior of the `validate-value' generic
function.")
  default-behavior

  ;; Default behavior consists in considering every value invalid and
  ;; signaling an error.
  (ensure-condition 'value-invalid-for-type
    (validate-value :does-not-matter :does-not-matter))

  ;; Test returning nil instead of signaling an error.
  (ensure-null (validate-value :does-not-matter :does-not-matter
			       :if-invalid nil))

  ;; Test continuing despite an invalid value.
  (ensure-same (validate-value :does-not-matter :does-not-matter
			       :if-invalid #'continue)
	       t))

(addtest (validate-value-root
          :documentation
	  "Test that `validate-value' returns the causing condition as
a second return value.")
  cause-return-value

  ;; Expect result nil, second return value causing condition.
  (let+ ((type (make-instance 'mock-type/validate-value))
	 ((&values result cause) (validate-value type :does-not-matter
						 :if-invalid nil)))
    (ensure-null result)
    (ensure (typep cause 'value-invalid-for-type))))
