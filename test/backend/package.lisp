;;; package.lisp --- Package definition for unit tests of the backend module.
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

(cl:defpackage :rosetta.backend.test
  (:use
   :cl
   :alexandria
   :iterate
   :let-plus
   :iterate
   :lift

   :rosetta.model.data
   :rosetta.model.language
   :rosetta.backend

   :rosetta.test
   :rosetta.model.data.test)

  (:import-from :rosetta.backend
   :context)

  (:shadowing-import-from :rosetta.backend
   :generate)

  (:shadowing-import-from :rosetta.test
   :root)

  (:import-from :rosetta.backend
   :context)

  (:export
   :backend-root)

  (:documentation
   "This package contains unit tests for the backend module."))

(cl:in-package :rosetta.backend.test)

(deftestsuite backend-root (root)
  ()
  (:documentation
   "Root unit test suite for the backend module."))


;;; mock-node/* classes
;;

(defclass mock-node/no-methods ()
  ())

(defclass mock-node/warning ()
  ())

(defmethod emit ((node     mock-node/warning)
		 (target   target-reference)
		 (language language-lisp))
  (warn "~@<Mock warning for unit tests.~@:>")
  :result)

(declaim (special *state-for-mock-node/context*))

(defvar *state-for-mock-node/context*)

(defclass mock-node/context ()
  ())

(defmethod emit/context ((node     mock-node/context)
			 (target   target-reference)
			 (language language-lisp))
  (let ((*state-for-mock-node/context* :result-from-context))
    (call-next-method)))

(defmethod emit ((node     mock-node/context)
		 (target   target-reference)
		 (language language-lisp))
  *state-for-mock-node/context*)

(defclass mock-node/callback ()
  ((callback :initarg :callback)))

(defmethod emit ((node     mock-node/callback)
		 (target   t)
		 (language t))
  (when-let ((callback (slot-value node 'callback)))
    (funcall callback node target language))
  :result-after-callback)
