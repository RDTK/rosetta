;;; protocol.lisp --- Unit tests for protocol functions of the backend module.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
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

(cl:in-package :rosetta.backend.test)

(deftestsuite backend-protocol-root (backend-root)
  ()
  (:documentation
   "Unit tests for protocol functions of the backend module."))

(addtest (backend-protocol-root
          :documentation
	  "Test default behavior of methods on `generate'.")
  generate/smoke

  (ensure-cases ((node target language) expected-condition expected-result)

    `(;; No emitter => `emit-error'.
      ((,(make-instance 'mock-node/no-methods) :reference :lisp)
       emit-error   nil)
      ;; `emit' warns => `emit-warning'.
      ((,(make-instance 'mock-node/warning)    :reference :lisp)
       emit-warning :result)
      ;; `emit/context' is used
      ((,(make-instance 'mock-node/context)    :reference :lisp)
       nil          :result-from-context))

    (let+ (((&flet do-it ()
	      (generate node target language)))
	   ((&values result result?)
	    (case expected-condition
	      (emit-error   (ensure-condition 'emit-error (do-it)))
	      (emit-warning (progn
			      (ensure-condition 'emit-warning (do-it))
			      (values (do-it) t)))
	      ((nil)        (values (do-it) t)))))
      (when result?
	(ensure-same result expected-result :test #'equal)))))

(addtest (backend-protocol-root
          :documentation
	  "Test context established by `generate'.")
  generate/context

  (macrolet
      ((check-context (&body body)
	 `(generate (make-instance
		     'mock-node/callback
		     :callback (lambda (node target language)
				 (declare (ignorable node target language))
				 ,@body))
		    :reference
		    :lisp)))

    ;; Ensure `*context*' gets bound.
    (check-context (ensure *context*))

    ;; Ensure `retry', `continue' and `use-value' restarts are
    ;; established.
    (ensure-same (check-context
		  (ensure (find-restart 'rs.b::retry))
		  (setf (slot-value node 'callback) nil)
		  (invoke-restart 'rs.b::retry))
		 :result-after-callback)
    (ensure-null (check-context
		  (ensure (find-restart 'continue))
		  (invoke-restart 'continue)))
    (ensure-same (check-context
		  (ensure (find-restart 'use-value))
		  (invoke-restart 'use-value :result-instead-of-callback))
		 :result-instead-of-callback)))
