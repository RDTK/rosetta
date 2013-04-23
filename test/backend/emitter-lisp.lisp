;;;; emitter-lisp.lisp --- Unit tests for the Lisp emitter.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.backend.test)

(deftestsuite backend-emitter-lisp-root (backend-root)
  ()
  (:documentation
   "Unit test for emitter for Lisp."))

(addtest (backend-emitter-lisp-root
          :documentation
          "Smoke test for emitting Lisp code for \"instantiate\"
target.")
  emit-instantiate/smoke

  (ensure-cases (node args expected)
      `(;; Fundamental types.
        (,+bool+               ()              nil)
        (,+bool+               (:value t)      t)
        (,+bool+               (:value 5)      error)

        (,+uint8+              ()              0)
        (,+uint8+              (:value 255)    255)
        (,+uint8+              (:value 256)    error)
        (,+int16+              ()              0)

        (,+float32+            ()              0.0f0)
        (,+float32+            (:value -2.5f0) -2.5f0)
        (,+float32+            (:value "a")    error)
        (,+float64+            ()              0.0d0)

        (,+utf-8-string+       ()              "")
        (,+utf-8-string+       (:value "foo")  "foo")
        (,+utf-8-string+       (:value 5)      error)

        (,+octet-vector+       ()              (octet-vector))

        ;; Singleton types.
        (,+singleton/uint32+   ()              1)
        (,+singleton/uint32+   (:value 2)      error)
        (,+singleton/uint32+   (:value "foo")  error)
        (,+singleton/float64+  ()              1.0d0)
        (,+singleton/float64+  (:value 1)      1.0d0)
        (,+singleton/float64+  (:value 2)      error)
        (,+singleton/float64+  (:value 2.0d0)  error)
        (,+singleton/uint32+   (:value "foo")  error)

        ;; Enum type.
        (,+enum/uint32/simple+ ()              :a)
        (,+enum/uint32/simple+ (:value :b)     :b)
        (,+enum/uint32/simple+ (:value :c)     error)

        ;; Structure type.
        (,+struct/simple+      (:|a| "foo")    :no-error) ; TODO(jmoringe, 2012-12-20): proper check
        (,+struct/simple+      (:|b| "foo")    error)     ; no such field
        (,+struct/simple+      (:|a| 5)        error)     ; type
        (,+struct/empty+       ()              :no-error)
        (,+struct/recursive+   ()              :no-error))
    (let+ ((initargs (if args
                        `(:instantiate :initargs ,args)
                        :instantiate))
           ((&flet do-it () (generate node initargs :lisp))))
      (case expected
        (error     (ensure-condition 'error (do-it)))
        (:no-error (do-it))
        (t         (ensure-same (do-it) expected))))))

(addtest (backend-emitter-lisp-root
          :documentation
          "Smoke test for generating classes.")
  emit-definition/class/smoke

  (ensure-cases (type slot-count expected/documentation)
      `((,+struct/simple+ 1
         "A simple structure with a single field.")
        (,+struct/empty+ 0
         "An empty structure.")
        (,+struct/recursive+ 3
         "A simple recursive structure."))

    (let* ((class (generate type :class :lisp/compiled))
           (name  (class-name class)))
      (ensure (find-class name nil))
      (closer-mop:finalize-inheritance class)
      (ensure-same (length (closer-mop:class-slots class)) slot-count)
      (ensure-same (documentation name 'type) expected/documentation)
      (make-instance class))))

(addtest (backend-emitter-lisp-root
          :documentation
          "Smoke test for generating enums.")
  emit-definition/enum/smoke

  (ensure-cases (type
                 expected/members expected/name<->code
                 expected/documentation)
      `((,+enum/uint8/simple+
         ;; Members
         (:A :B)
         ;; Code <-> name
         ((:A    . 1)     ; valid pair
          (:B    . 2)     ; valid pair
          (:C    . error) ; no such name
          (error . 3))    ; no such code
         ;; Documentation
         "A simple uint8 enum with two values."))

    (let+ (((&values name code->name name->code)
            (generate type :class :lisp/compiled)))
      ;; Check members of generated type.
      (iter (for member in expected/members)
            (ensure (typep member name)))

      ;; Check generated code <-> name mapping
      (iter (for (name . code) in expected/name<->code)
            (cond
              ((eq name 'error)
               (ensure-condition 'error (funcall code->name code)))

              ((eq code 'error)
               (ensure-condition 'error (funcall name->code name)))

              (t
               (ensure-same (funcall code->name code) name)
               (ensure-same (funcall name->code name) code))))

      (ensure-same (documentation name 'type) expected/documentation))))
