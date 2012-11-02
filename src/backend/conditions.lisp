;;; conditions.lisp --- Conditions used by the backend module.
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

(cl:in-package :rosetta.backend)

(define-condition emit-condition (condition)
  ((context :initarg  :context
	    :reader   emit-condition-context
	    :documentation
	    "Stores a copy of the context that was current when the
condition was signaled."))
  (:default-initargs
   :context (missing-required-initarg 'emit-condition :context))
  (:documentation
   "Instances of subclasses of this condition class are signaled to
notify events, especially errors and warning, during an emission
process."))

(macrolet ((define-method (name)
	     (let ((condition-method (symbolicate "EMIT-CONDITION-" name))
		   (context-method   (symbolicate "CONTEXT-" name)))
	      `(defgeneric ,condition-method (condition)
		 (:method ((condition emit-condition))
		   (,context-method (emit-condition-context condition)))))))

  (define-method node)
  (define-method target)
  (define-method language))

(macrolet
    ((define-emit-condition (kind fail-verb)
       (let ((name (symbolicate "EMIT-" kind)))
	 ` (define-condition ,name (,kind
				    emit-condition
				    chainable-condition)
	     ()
	     (:report
	      (lambda (condition stream)
		(let+ (((&accessors-r/o
			 (node        context-node)
			 (target      context-target)
			 (language    context-language)
			 (stack       context-stack)
			 (environment context-environment/alist))
			(emit-condition-context condition))
		       (*package* #.*package*))
		  (format stream ,(format nil "~~@<Emit ~A for~~&
~~2@T~~11A~~A
~~2@T~~11A~~A
~~2@T~~11A~~A

Environment:
~~{~~&~~2@T~~32A:~~S~~}

Stack:
~~{~~&~~2@T]~~{~~A~~_~~4@T~~A~~_~~4@T~~A~~}~~}~~&~~/more-conditions::maybe-print-cause/~~@:>"
					  fail-verb)
			  'node     node
			  'target   target
			  'language language
			  (alist-plist environment)
			  stack
			  condition))))
	     (:documentation

	      ,(format nil "This ~(~A~) is signaled when something ~
goes wrong during an emission process."
		       kind))))))

  (define-emit-condition error   "failed")
  (define-emit-condition warning "warned"))
