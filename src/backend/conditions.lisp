;;;; conditions.lisp --- Conditions used by the backend module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.backend)

;;; Conversion-related conditions

(define-condition conversion-condition (condition)
  ((from :initarg  :from
         :reader   conversion-condition-from
         :documentation
         "Stores the source type of the conversion in question.")
   (to   :initarg  :to
         :reader   conversion-condition-to
         :documentation
         "Stores the target type of the conversion in question."))
  (:default-initargs
   :from (missing-required-initarg 'conversion-condition :from)
   :to   (missing-required-initarg 'conversion-condition :to))
  (:documentation
   "This class is intended to be mixed into condition classes which
describe conversions."))

(macrolet
    ((define-conversion-conditions (kind)
       (let ((name        (symbolicate "CONVERSION-" kind))
             (simple-name (symbolicate "SIMPLE-CONVERSION-" kind)))
         `(progn
            (define-condition ,name (,kind
                                     conversion-condition)
              ()
              (:report
               (lambda (condition stream)
                 (format stream "~@<Cannot convert from type ~A to ~
                                 type ~A.~@:>"
                         (conversion-condition-from condition)
                         (conversion-condition-to   condition))))
              (:documentation
               "Errors of this class and its subclasses are signaled
when a conversion fails."))

            (define-condition ,simple-name (simple-error
                                            ,name)
              ()
              (:report
               (lambda (condition stream)
                 (format stream "~@<Cannot convert from type ~A to type ~A: ~
                                 ~?~@:>"
                         (conversion-condition-from         condition)
                         (conversion-condition-to           condition)
                         (simple-condition-format-control   condition)
                         (simple-condition-format-arguments condition))))
              (:documentation
               ,(format nil "A simple `~(~A~)'." name)))))))

  (define-conversion-conditions error)
  (define-conversion-conditions warning))

(define-condition cannot-narrow (conversion-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Cannot convert from wider type ~A to narrower ~
                     type ~A.~@:>"
             (conversion-condition-from condition)
             (conversion-condition-to   condition))))
  (:documentation
   "This error is signaled when a conversion cannot be performed
because it would illegally narrow the source type."))

(define-condition loss-of-precision (conversion-warning)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Conversion from type ~A to type ~A looses ~
                     precision.~:>"
             (conversion-condition-from condition)
             (conversion-condition-to   condition))))
  (:documentation
   "This warning is signaled when a conversion looses precision."))

;;; Context-related conditions

(define-condition context-condition (condition)
  ((context :initarg  :context
            :reader   context-condition-context
            :documentation
            "Stores a copy of the context that was current when the
condition was signaled."))
  (:default-initargs
   :context (missing-required-initarg 'context-condition :context))
  (:documentation
   "This condition class is intended to be mixed into condition
classes which report the context in which the condition has been
signaled."))

(define-condition missing-environment-entry (error
                                             context-condition)
  ((name :initarg  :name
         :type     symbol
         :reader   missing-environment-entry-name
         :documentation
         "Stores the name of the missing environment entry."))
  (:report
   (lambda (condition stream)
     (let+ (((&accessors-r/o (name    missing-environment-entry-name)
                             (context context-condition-context)) condition))
       (format stream "~@<The requested entry ~S could not be found in ~
                       ~:[the empty environment~
                       ~;~:*environment~2%~2@T~<~@;~{~32S ~
                       ~S~^~&~}~:>~2%~]of context ~A.~@:>"
               name
               (list (alist-plist (context-environment/alist context)))
               context))))
  (:default-initargs
   :name (missing-required-initarg 'missing-environment-entry :name))
  (:documentation
   "This error is signaled when an environment entry is requested but
does not exist."))

;;; `generate'/`emit'-related conditions

(define-condition emit-condition (context-condition)
  ()
  (:documentation
   "Instances of subclasses of this condition class are signaled to
notify events, especially errors and warning, during an emission
process."))

(macrolet ((define-method (name)
             (let ((condition-method (symbolicate "EMIT-CONDITION-" name))
                   (context-method   (symbolicate "CONTEXT-" name)))
              `(defgeneric ,condition-method (condition)
                 (:method ((condition emit-condition))
                   (,context-method (context-condition-context condition)))))))

  (define-method node)
  (define-method target)
  (define-method language))

(macrolet
    ((define-emit-condition (kind fail-verb)
       (let ((name (symbolicate "EMIT-" kind)))
         `(define-condition ,name (,kind
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
                       (context-condition-context condition))
                      (*package*      #.*package*)
                      (*print-circle* nil))
                 (format stream ,(format nil "~~@<Emit ~A for~~&
~~2@T~~11A~~A
~~2@T~~11A~~A
~~2@T~~11A~~A

Environment:
~~{~~&~~2@T~~32S~~S~~}

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
