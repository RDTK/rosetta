;;; protocol.lisp --- Unit tests for the protocol of the model.serialization module.
;;
;; Copyright (C) 2012 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the
;; GNU Lesser General Public License Version 3 (the ``LGPL''),
;; or (at your option) any later version.
;;
;; Software distributed under the License is distributed
;; on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY KIND, either
;; express or implied. See the LGPL for the specific language
;; governing rights and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html
;; or write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

(cl:in-package :rosetta.model.serialization.test)

(deftestsuite model-serialization-protocol-root (model-serialization-root)
  ()
  (:documentation
   "Unit tests for the protocol of the model.serialization module."))

(deftestsuite validate-type-root (model-serialization-protocol-root)
  ()
  (:documentation
   "Tests for the `validate-type' generic function."))

(addtest (validate-type-root
          :documentation
	  "Test default behavior of the `validate/type' generic
function.")
  default-behavior

  ;; Test behavior in case of invalid types.
  (let ((mechanism :does-not-matter)
	(type      :does-not-matter))

    ;; Type is invalid; should signal an error.
    (ensure-condition type-invalid-for-mechanism
      (validate-type mechanism type))

    ;; Return nil instead of signaling an error.
    (ensure-same (validate-type mechanism type :if-invalid nil)
		 nil)

    ;; Use `continue' restart
    (ensure-same (validate-type mechanism type :if-invalid #'continue)
		 t)))

(addtest (validate-type-root
          :documentation
	  "Test that `validate-type' returns the causing condition as
a second return value.")
  cause

  ;; Expect result nil, second return value causing condition.
  (let+ ((mechanism (make-instance 'mock-mechanism/validate-type))
	 ((&values result cause) (validate-type mechanism :does-not-matter
						:if-invalid nil)))
    (ensure-null result)
    (ensure (typep cause 'type-invalid-for-mechanism))))
