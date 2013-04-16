;;;; package.lisp --- Package definition for unit tests of the backend module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rosetta.backend.test
  (:use
   #:cl
   #:alexandria
   #:iterate
   #:let-plus
   #:iterate
   #:nibbles
   #:lift

   #:rosetta.model.data
   #:rosetta.model.language
   #:rosetta.model.serialization
   #:rosetta.serialization
   #:rosetta.backend

   #:rosetta.test
   #:rosetta.model.data.test)

  (:import-from #:rosetta.backend
   #:context)

  (:shadowing-import-from #:rosetta.model.data
   #:name
   #:type1)

  (:shadowing-import-from #:rosetta.backend
   #:generate)

  (:shadowing-import-from #:rosetta.test
   #:root)

  (:import-from #:rosetta.backend
   #:context)

  (:export
   #:backend-root

   #:ensure-serialization-cases)

  (:documentation
   "This package contains unit tests for the backend module."))

(cl:in-package #:rosetta.backend.test)

(deftestsuite backend-root (root)
  ()
  (:documentation
   "Root unit test suite for the backend module."))

;;; mock-node/* classes

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

;;;

(macrolet
    ((define-mock-mechanism (endian)
       (let* ((name/short (symbolicate '#:mock/ endian))
              (name       (symbolicate '#:mechanism- name/short))
              (spec       (make-keyword name/short)))
         `(progn
            (eval-when (:compile-toplevel :load-toplevel :execute)
              (defmethod find-mechanism-class ((spec (eql ,spec)))
                (find-class ',name))

              (defclass ,name (binary-mixin
                               data-holder-mixin
                               offset-type-mixin
                               length-type-mixin
                               constant-endian-mixin)
                ()
                (:default-initargs
                 :offset-type +uint16+
                 :length-type +uint8+
                 :endian      ,endian)))

            (defmethod validate-type ((mechanism ,name) (type t)
                                      &key &allow-other-keys)
              t)

            (define-mechanism-targets ,name/short)))))

  (define-mock-mechanism :little-endian)
  (define-mock-mechanism :big-endian))

;;; Test macros

(defmacro ensure-serialization-cases ((&key
                                       default-target
                                       (input-var           'input)
                                       (expected-var        'expected)
                                       (type-var            'type)
                                       (generated-var       'generated)
                                       destination-initform)
                                      cases
                                      &body body)
  "Execute BODY for each case in CASES which are of the form

  (TYPE-SPEC TARGET ((INPUT1 . EXPECTED1) ...))

code is generated based on TYPE-SPEC and TARGET, compiled and
GENERATED-VAR is bound to the function of one argument which is the
result of the compilation. BODY is executed for each INPUTN and is
supposed to `cl:funcall' GENERATED-VAR with INPUT-VAR as the argument
and compare the result of the function call with EXPECTED-VAR."
  `(ensure-cases (type-spec case-target inputs-and-expected)
       ,cases

     (let+ ((,type-var (etypecase type-spec
                         (symbol (make-instance type-spec))
                         (list   (apply #'make-instance type-spec))
                         (t      type-spec)))
            (target    (or case-target ,default-target
                           (error "~@<Neither case-specific target nor ~
                                   ~S has been supplied.~@:>"
                               :default-target)))
            ;; Generate code for TYPE and TARGET using fresh SOURCE,
            ;; DESTINATION and OFFSET variables.
            ((&with-gensyms source destination offset start end))
            ((&flet generate-code (type)
               (let+ ((*context* (make-instance 'context))
                      ((&env (:source-var      source)
                             (:destination-var destination)
                             (:offset-var      offset)
                             (:start-var       start)
                             (:end-var         end))))
                 (generate type target :lisp))))
            ;; Put the generated code into a simple context which binds
            ;; SOURCE, DESTINATION and OFFSET.
            (,generated-var
             (compile nil `(lambda (input)
                             (let* ((,source      input)
                                    (,destination ,,destination-initform)
                                    (,offset      0)
                                    (,start       ,offset)
                                    (,end         100))
                               (values ,(generate-code type) ,destination ,offset))))))
       (iter (for (,input-var . ,expected-var) in inputs-and-expected)
             (let+ (((&flet do-it () ,@body)))
               (case ,expected-var
                 (error (ensure-condition 'error (do-it)))
                 (t     (do-it))))))))
