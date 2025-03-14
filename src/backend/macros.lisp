;;;; macros.lisp --- Macros for backends.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.backend)

;;; Housekeeping macros

(defmacro with-emit-restarts ((node target) &body body)
  "Establish restarts."
  (with-unique-names (result-var read-value-var)
    (once-only (node target)
      `(let+ ((,result-var)
              ((&flet ,read-value-var ()
                 (format *query-io* "Replacement value: ")
                 (force-output *query-io*)
                 (list (read *query-io*)))))
         (tagbody
          :retry
            (restart-case
                (setf ,result-var (multiple-value-list (progn ,@body)))

              ;; Retry running the emit method.
              (retry ()
                :report
                (lambda (stream)
                  (format stream
                          "~@<Retry running the emit method for node ~
                           ~S and target ~S.~@:>"
                          ,node ,target))
                (go :retry))

              ;; Skip the emit method.
              (continue (&optional condition)
                :report (lambda (stream)
                          (format stream "~@<Skip the emit method for ~
                                          node ~S and target ~S.~@:>"
                                  ,node ,target))
                (declare (ignore condition)))

              ;; Use a replacement value.
              (use-value (value)
                :report
                (lambda (stream)
                  (format stream
                          "~@<Specify a value instead of running the ~
                           emit method for node ~S and target ~S.~@:>"
                          ,node ,target))
                :interactive ,read-value-var
                (setf ,result-var (list value)))))
         (values-list ,result-var)))))

(defmacro with-updated-context ((node target
                                 &optional
                                 language
                                 (context '(or *context* (make-instance 'context))))
                                &body body)
  "During the execution of BODY, set the current target type to
TARGET-VAR and push NODE-VAR onto the context stack."
  `(let ((*context* ,context))
     (push-context ,node ,target ,language *context*)
     (unwind-protect
          (progn ,@body)
       (pop-context *context*))))

;;; Convenience macros for clients

(defmacro with-emit-symbols (&body body)
  "Execute BODY with the following symbols added to the lexical scope:
+ package           :: The package that is the current target of the
                       emission process.
+ parent            :: The parent of the current node or nil.
+ grandparent       :: The parent of the parent of the current node
                       or nil.
+ ancestors         :: List of all ancestor nodes.
+ recur             :: A closure that accepts a node and calls `emit'
                       with the current target and language on that node.
+ cget, (setf cget) :: Retrieve and store values in the current
                       environment of context.
+ intern*           :: Similar to `intern' but use the context package."
  `(symbol-macrolet ((stack       (context-stack *context*))
                     (parent      (second (context-stack *context*)))
                     (grandparent (third (context-stack *context*)))
                     (ancestors   (rest (context-stack *context*))))
     (flet ((recur (node)
              (generate node
                        (context-target *context*)
                        (context-language *context*)))
            (cget (key)
              (context-get *context* key))
            ((setf cget) (new-value key)
              (setf (context-get *context* key) new-value)))
       (declare (ignorable #'recur #'cget #'(setf cget)))
       ,@body)))

(defmacro optimization-case ((target) &body clauses)
  "Similar to `cl:case' but dispatching on optimization settings in
TARGET. CLAUSES consists of clauses of the form

  (PREDICATE BODY)

where PREDICATE is an expressions containing the symbols `cl:speed',
`cl:debug', `cl:safety', `cl:space' and arbitrary expressions
forms. PREDICATEs and BODY can rely on these symbols being bound to
the respective optimization setting which is one of the integers 0, 1,
2 or 3."
  (once-only (target)
    (let+ ((qualities '(speed debug safety space))
           ((&with-gensyms settings))
           ((&flet+ do-clause ((&whole clause condition &body body))
              (if (member condition qualities :test #'eq)
                  (let ((remaining (remove condition qualities :test #'eq)))
                    `((and ,@(mapcar (lambda (other) `(> ,condition ,other))
                                     remaining))
                      ,@body))
                  clause)))
           ((&flet make-binding (quality)
              `(,quality (or (second (find ',quality  ,settings :key #'first)) 1)))))
      `(let* ((,settings (optimization-settings ,target))
              ,@(mapcar #'make-binding qualities))
         (declare (ignorable ,@qualities))
         (cond ,@(mapcar #'do-clause clauses))))))

;;;

(defmacro define-target (name superclasses slots &body options)
  "Define a target named by the symbol NAME. SUPERCLASSES, SLOTS and
OPTIONS have the same meaning as in `cl:defclass'."
  (check-type name symbol)

  (let ((spec       (make-keyword name))
        (class-name (format-symbol *package* "TARGET-~A" name)))
    `(progn
       (defclass ,class-name (,@superclasses)
         (,@slots)
         ,@options)

       (service-provider:register-provider/class
        'target ,spec :class ',class-name))))

(defmacro define-target/method (name superclasses slots
                                &body options)
  "Define a method target named by the symbol NAME. SUPERCLASSES,
SLOTS and OPTIONS have the same meaning as in `cl:defclass'.

A method target generates a method around a body which implements the
actual behavior of the target. The target used to generate this body
is derived from NAME. For NAME, the resulting targets are:

  method target: target-NAME/method
  body target:   target-NAME"
  (check-type name symbol)

  (let+ ((name/method (format-symbol *package* "~A/METHOD" name))
         (body-class  (format-symbol *package* "TARGET-~A" name))
         ((&flet default-documentation ()
            (format nil "The ~A target delegates to the ~A target ~
                         which, for example, emits methods on ~
                         `rosetta.serialization:~:*~(~A~)' or ~
                         otherwise generates code for given ~
                         serialization mechanisms and classes ~
                         described by model component instances."
                    name/method name)))
         (options (apply #'append options))
         ((&plist-r/o (documentation :documentation (default-documentation)))
          options))
    `(define-target ,name/method (,@(or superclasses '(method-target-mixin)))
       (,@slots)
       (:default-initargs
        :body-target (make-instance ',body-class))
       (:documentation ,documentation)
       ,@(remove-from-plist options :documentation))))

(defmacro define-mechanism-target ((mechanism target)
                                   (&rest mixins))
  "Define a target for the combination of MECHANISM and TARGET which
have to be symbols."
  (check-type mechanism symbol)
  (check-type target    symbol)
  (check-type mixins    list)

  (let ((name       (symbolicate mechanism "-" target))
        (superclass (symbolicate "TARGET-" target)))
    `(define-target ,name (,@mixins ,superclass)
         ()
       (:default-initargs
        :mechanism (service-provider:make-provider 'mechanism ,mechanism))
       (:documentation
        ,(format nil "~A~2%This target class generates code that ~
                      implements the ~(~A~) mechanism.~@[~2%~A~]"
                 (documentation superclass 'type)
                 mechanism
                 (when-let ((mechanism (service-provider:find-provider
                                        'mechanism mechanism)))
                   (documentation mechanism t)))))))

(defmacro define-mechanism-target/method ((mechanism method)
                                          (&rest mixins))
  "Define a method target for the combination of MECHANISM and METHOD
which have to be symbols."
  (check-type mechanism symbol)
  (check-type method    symbol)
  (check-type mixins    list)

    (let ((name       (format-symbol *package* "~A-~A" mechanism method))
          (superclass (format-symbol *package* "TARGET-~A/METHOD" method)))
      `(define-target/method ,name (,@mixins ,superclass)
         ()
         (:documentation
          ,(format nil "The ~S/METHOD target is a specialization of ~
                        the ~S target which generates methods that ~
                        implement the ~(~A~) function for the ~(~A~) ~
                        mechanism.~@[~2%~A~]~@[~2%~A~]"
                   name superclass method mechanism
                   (documentation superclass 'type)
                   (when-let ((mechanism (service-provider:find-provider
                                          'mechanism mechanism)))
                     (documentation mechanism t)))))))

(defmacro define-mechanism-targets
    (mechanism
     &key
     (prefix         "MECHANISM-")
     (targets        '(:packed-size :pack :unpack :location :extract))
     (method-targets targets)
     mixins)
  "Define target classes for MECHANISM for TARGETS. The names of the
target classes are target-MECHANISM-TARGET and
target-MECHANISM-TARGET/method for TARGET in TARGETS.

TARGETS could, for example, be `packed-size', `pack', `unpack',
`location' and `extract'.

METHOD-TARGETS specifies the list of targets for which the
corresponding \"method target\" should be generated. The default
behavior is generating method targets for all TARGETS.

MIXINS can be used to specify mixin classes which should be
superclasses of all generated target classes."
  (check-type mechanism symbol)
  (check-type mixins    list)
  (unless (subsetp method-targets targets)
    (error "~@<~S ~S is not a subset of ~S ~S.~@:>"
           'method-targets method-targets
           'targets        targets))

  (let* ((mechanism-string (string mechanism))
         (base-name        (if (starts-with-subseq prefix mechanism-string)
                               (subseq mechanism-string (length prefix))
                               mechanism-string))
         (mechanism-spec   (make-keyword base-name)))
    `(progn
       ,@(iter (for target in targets)
               (collect `(define-mechanism-target
                             (,mechanism-spec ,target) ,mixins))
               (when (member target method-targets)
                 (collect `(define-mechanism-target/method
                               (,mechanism-spec ,target) ,mixins)))))))

;;; let-plus extensions

(define-let+-expansion (&env args
                        :uses-value? nil
                        :body-var    body)
  "Bind environment entries. Syntax:

  (&env NAME | (NAME VALUE) ... [&context CONTEXT])

  NAME ::= ENTRY | (VARIABLE ENTRY)

The variables VARIABLE ... and environment entries ENTRY ... are bound
to VALUE ... or a gensyms if values are not supplied. When only
VARIABLE is supplied, it should be a symbol and the environment entry
name is generated by turning VARIABLE into a keyword.

VARIABLE can be nil, when only the environment entry but no variable
should be bound. Similarly, if NAME is a keyword.

When VARIABLE appears in VALUE, it is replaced with the old value of
ENTRY, allowing update-constructs like

  (&env (my-var (1+ my-var)))

CONTEXT can be used to specify the affected context. When not
supplied, `*context*' is used."
  (let+ (((args &optional (context '(*context*)))
          (split-sequence '&context args))
         ((&values bindings setters cleanup)
          (iter (for name in args)
                ;; Split the variable into the name and optional value
                ;; parts.
                (let+ (((name
                         &optional
                         (value `(gensym ,(string (first (ensure-list name))))))
                        (ensure-list name))
                       ((variable &optional (entry (make-keyword variable)))
                        (typecase name
                          (keyword (list nil name))
                          (t       (ensure-list name))))
                       (place `(context-get ,@context ,entry))
                       ((&with-gensyms old))
                       (value (if variable
                                  `(symbol-macrolet ((,variable ,old))
                                     ,value)
                                  value)))
                  (check-type variable (or null symbol))
                  (check-type entry    keyword)

                  ;; Collect a binding.
                  (collect `(,old  ,place) :into bindings)
                  (when variable
                    (collect `(,variable ,value) :into bindings))
                  ;; Collect a form to store the value in the `emit'
                  ;; context.
                  (collect `(setf ,place ,(or variable value)) :into setters)
                  (collect `(setf ,place ,old)                 :into cleanup))
                (finally (return (values bindings setters cleanup))))))
    `(let* ,bindings
       ,@setters
       (unwind-protect
            (progn ,@body)
         ,@cleanup))))

(define-let+-expansion (&env-r/o args
                        :uses-value? nil
                        :body-var    body)
  "Bind variable to environment entries. Syntax:

  (&env-r/o NAME | (NAME DEFAULT) |... [&context CONTEXT])

  NAME ::= VARIABLE | (VARIABLE ENTRY)

The variables VARIABLE ... are bound to the values of the environment
entries ENTRY .... When only VARIABLE is supplied, it should be a
symbol and the environment entryn name is generated by turning VARIABLE
into a keyword.

CONTEXT can be used to specify the affected context. When not
supplied, `*context*' is used."
  (let+ (((args &optional (context '(*context*)))
          (split-sequence '&context args))
         (bindings
          (iter (for name in args)
                (let+ (((name &optional (default :error))
                        (ensure-list name))
                       ((variable &optional (entry (make-keyword variable)))
                        (ensure-list name)))
                  (check-type variable symbol)
                  (check-type entry    keyword)

                  (collect
                      `(,variable (context-get ,@context ,entry
                                               :default ,default)))))))
    `(let* ,bindings ,@body)))
