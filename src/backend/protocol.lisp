;;; protocol.lisp --- Protocol of the compiler backend.
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


;;; Target protocol and class family
;;

(dynamic-classes:define-findable-class-family target
  "This family consists of target classes. Each target class is used
to control the emission of one kind of thing based on an abstract
description in form of model component instances.")

(intern "TARGET") ; for (documentation :TARGET 'rosetta.backend:target)

(defmethod documentation ((thing symbol) (type (eql 'target)))
  "Obtain documentation of type TARGET from the target class
designated by THING."
  (documentation (find-target-class thing) t))


;;; Backend Context
;;

(defgeneric context-get (context key
			 &key
			 default)
  (:documentation
   "Retrieve the item designated by KEY from CONTEXT."))

(defgeneric (setf context-get) (new-value context key)
  (:documentation
   "Store NEW-VALUE as the value of the item designated by KEY in
CONTEXT."))

(defgeneric context-node (context)
  (:documentation
   "Return node currently being processed in CONTEXT."))

(defgeneric context-target (context)
  (:documentation
   "Return the target currently being processed in CONTEXT."))

(defgeneric context-language (context)
  (:documentation
   "Return the language currently being processed in CONTEXT."))

(defgeneric context-environment/alist (context)
  (:documentation
   "Return the environment stored in CONTEXT as an alist."))

(defclass context ()
  ((stack       :initarg  :stack
		:type     list
		:accessor context-stack
		:initform nil
		:documentation
		"A stack of nodes currently being processed in the current
emission process.")
   (environment :initarg  :environment
		:type     list
		:accessor %context-environment
		:initform (list (make-hash-table))
		:documentation
		"A stack of hash-tables that stores additional
context-dependent information."))
  (:documentation
   "Instances of this class are used to keep track of the current
state of a particular emission process.

This state consists of:
* A stack of triples consisting of the current
  * node
  * target
  * language
* A stack of variable bindings (called \"environment\")"))

(defmethod context-get ((context context) (key symbol)
			&key
			default)
  ;; The `first'/`list' trickery and use of multiple return values
  ;; from `gethash' is necessary for correct handling of nil values.
  (first
   (or (some #'(lambda (env)
		 (let+ (((&values value found?) (gethash key env)))
		   (when found? (list value))))
	     (%context-environment context))
       (if (eq default :error)
	   (error "~@<Required environment entry ~S is missing.~@:>"
		  key)
	   (list default)))))

(defmethod (setf context-get) ((new-value t)
			       (context   context)
			       (key       symbol))
  (setf (gethash key (first (%context-environment context))) new-value))

(defmethod context-node ((context context))
  (first (first (context-stack context))))

(defmethod context-target ((context context))
  (second (first (context-stack context))))

(defmethod context-language ((context context))
  (third (first (context-stack context))))

(defmethod context-environment/alist ((context context))
  (remove-duplicates (mappend #'hash-table-alist
			      (%context-environment context))
		     :key      #'car
		     :from-end t))

(defun push-context (node target
		     &optional language (context *context*))
  "Push the tripe (NODE TARGET LANGUAGE) onto CONTEXT."
  (push (list node target (or language (context-language context)))
	(context-stack context))
  (push (make-hash-table) (%context-environment context))
  context)

(defun pop-context (&optional (context *context*))
  "Pop the current (node target language) triple from CONTEXT."
  (let+ (((&accessors (environment %context-environment)
		      (stack       context-stack)) context))
    (unless (and environment stack)
      (error "~@<Cannot pop from top-level context ~A.~@:>"
	     context))

    (pop environment)
    (pop stack))
  context)

(defun copy-context (context)
  "Return a copy of CONTEXT."
  (make-instance
   'context
   :stack       (copy-list (slot-value context 'stack))
   :environment (mapcar #'copy-hash-table
			(slot-value context 'environment))))

(defmethod print-object ((object context) stream)
  (let+ (((&accessors-r/o (target      context-target)
			  (stack       context-stack)
			  (environment context-environment/alist)) object))
   (print-unreadable-object (object stream :type t :identity t)
     (format stream "~A (S ~D) (E ~D)"
	     target (length stack) (length environment)))))


;;; Generate/emit protocol
;;
;; The protocol consists of the following cascade of method calls:
;; 1. client calls `generate'
;;    a) `generate' performs target and language lookup and
;;       instantiation
;; 2. `generate' calls `emit/setup'
;;    a) `emit/setup' establishes restarts
;;    b) `emit/setup' establishes a `context'
;;    c) `emit/setup' establishes condition translation to
;;       `emit-error', `emit-warning'
;; 3. `emit/setup' calls `emit/context'
;;    a) client-supplied methods on `emit/context' establish a
;;       suitable context for the respective target
;;    b) client-supplied methods on `emit/context' `call-next-method'
;;       until (emit/context t t t) is called
;; 4. (emit/context t t t) calls `emit'
;;    a) client-supplied methods on `emit', using the established
;;       context produce the actual result

(defgeneric generate (node target language
		      &key
		      verbose
		      print
		      &allow-other-keys)
  (:documentation
   "Emit the appropriate object for NODE with respect to TARGET."))

(defgeneric emit/setup (node target language)
  (:documentation
   "Emit the appropriate object for NODE with respect to TARGET.

No methods must not be installed on `emit/setup'."))

(defgeneric emit/context (node target language)
  (:argument-precedence-order target language node)
  (:documentation
   "Emit the appropriate object for NODE with respect to TARGET."))

(defgeneric emit (node target language)
  (:argument-precedence-order target language node)
  (:documentation
   "Emit the appropriate object for NODE with respect to TARGET."))


;;; Default behavior for `generate'
;;
;; 1. Maybe resolve target class
;; 2. Maybe resolve language class
;; 3. Dispatch to `emit/setup'

(defmethod generate ((node t) (target t) (language t)
		     &key)
  (emit/setup node target language))

(defmethod generate ((node t) (target list) (language t)
		     &key)
  (let+ (((name &rest args) target)
	 (class (find-target-class name))
	 (instance (apply #'make-instance class args)))
    (generate node instance language)))

(defmethod generate ((node t) (target symbol) (language t)
		     &key)
  (generate node (list target) language))

(defmethod generate ((node t) (target t) (language list)
		     &key)
  (let+ (((name &rest args) language)
	 (class    (rs.m.l:find-language-class name))
	 (instance (apply #'make-instance class args)))
    (generate node target instance)))

(defmethod generate ((node t) (target t) (language symbol)
		     &key)
  (generate node target (list language)))


;;; Default behavior for `emit/steup'
;;
;; Establish context, restarts and condition translation, then
;; dispatch to `emit/context'.

(defmethod emit/setup :around ((node     t)
			       (target   t)
			       (language t))
  (with-emit-restarts (node target)
    (with-updated-context (node target language)
      (with-condition-translation
	  (((error emit-error)
	    :context (copy-context *context*))
	   ((warning emit-warning
		     :signal-via warn)
	    :context (copy-context *context*)))
	(call-next-method)))))

(defmethod emit/setup ((node     t)
		       (target   t)
		       (language t))
  (emit/context node target language))

(defmethod emit/context ((node     t)
			 (target   t)
			 (language t))
  (emit node target language))

(macrolet
    ((define-delegating-method (name &optional method?)
       `(defmethod ,name ((function (eql (fdefinition 'emit/context)))
			  ,@(when method? '((method t)))
			  &rest args)
	  (if (compute-applicable-methods (fdefinition 'emit) args)
	      (apply #'emit args)
	      (error "~@<No emitter for ~{~A~^, ~}.~@:>" args)))))
  (define-delegating-method no-applicable-method)
  (define-delegating-method no-next-method t))
