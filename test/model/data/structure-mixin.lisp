;;;; structure-mixin.lisp --- Unit tests for the structure-mixin class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.model.data.test)

(deftestsuite structure-mixin-root (model-data-root)
  ()
  (:documentation
   "Test suite for the `structure-mixin' class."))

(addtest (structure-mixin-root
          :documentation
          "Test constructing `structure-mixin' instances.")
  construction

  (ensure-cases ((args expected))
      `(((:fields 5)                                       type-error)
        ((:fields #("bla"))                                type-error)
        ((:fields ("a" ,+utf-8-string+))                   ("a"))
        ((:fields (,(make-instance 'base-field
                                   :name "g"
                                   :type +utf-8-string+))) ("g")))
    (let+ (((&flet do-it ()
              (apply #'make-instance 'structure-mixin args))))
      (case expected
        (type-error (ensure-condition 'type-error (do-it)))
        (t          (ensure-same
                     (map 'list #'name (contents (do-it) :field))
                     expected
                     :test (rcurry #'set-equal :test #'string=)))))))

(addtest (structure-mixin-root
          :documentation
          "Test constructing invalid parent relationships.")
  construction/invalid-parent

  (let ((foo (make-instance 'base-structure :name "foo"))
        (bar (make-instance 'base-structure :name "bar")))
    (ensure-condition 'child-error
      (setf (lookup foo :nested "foo") foo))
    (ensure-condition 'child-error
      (setf (lookup foo :nested "bar") bar
            (lookup bar :nested "foo") foo))))

(addtest (structure-mixin-root
          :documentation
          "Test constructing recursive `structure-mixin' instances.")
  construction/recusive

  (macrolet
      ((test (type)
         (let+ (((&flet make-ensure (type &rest body)
                   `(ensure-condition 'simple-child-error
                      (let* ((structure (make-instance 'structure-mixin))
                             (field     (make-instance 'base-field
                                                       :name "a"
                                                       :type ,type)))
                        (declare (ignorable field))
                        ,@body)))))
          `(progn
             ,(make-ensure type
               `(reinitialize-instance structure :fields (list field)))
             ,(make-ensure type
               `(reinitialize-instance structure :fields (list "a" ,type)))
             ,(make-ensure type
               `(setf (lookup structure :field "a") field))))))
    ;; Direct mandatory instantiation of STRUCTURE.
    (test structure)
    ;; Indirect mandatory instantiation of STRUCTURE.
    (test (make-instance 'structure-mixin
                         :fields `("a" ,structure)))))

(addtest (structure-mixin-root
          :documentation
          "Test method on `parent', `ancestors' and `root' for class
`structure-mixin'.")
  parent+acestors+root/smoke

  (ensure-cases (thing expected-parent expected-ancestors expected-root)
      `((,+struct/simple+
         nil              ()                 ,+struct/simple+)
        (,(lookup +struct/simple+ :field "a")
         ,+struct/simple+ (,+struct/simple+) ,+struct/simple+)
        (,+struct/empty+
         nil              ()                 ,+struct/empty+)
        (,+struct/recursive+
         nil              ()                 ,+struct/recursive+)
        (,+struct/nested+
         nil              ()                 ,+struct/nested+)
        (,(lookup +struct/nested+ :nested "inner")
         ,+struct/nested+ (,+struct/nested+) ,+struct/nested+)
        (,+struct/packaged+
         ,+package/simple+
         (,+package/simple+ ,+package/root+)
         ,+package/root+))

    (ensure-same (parent thing) expected-parent)
    (ensure-same (ancestors thing :include-self? nil) expected-ancestors
                 :test #'set-equal)
    (ensure-same (root thing) expected-root)))

(addtest (structure-mixin-root
          :documentation
          "Test the `contents' method specialization
`structure-mixin'.")
  lookup

  (ensure-cases (struct args expected)
      `((,+struct/simple+ ("a")
         ,(first (contents +struct/simple+ :field)))
        (,+struct/simple+ ((:relative "a"))
         ,(first (contents +struct/simple+ :field)))
        (,+struct/simple+ ((:absolute "a"))
         ,(first (contents +struct/simple+ :field)))
        (,+struct/simple+ ("a" :if-does-not-exist nil)
         ,(first (contents +struct/simple+ :field)))
        (,+struct/simple+ ("no-such-child")
         no-such-child)
        (,+struct/simple+ ((:relative "no-such-child"))
         no-such-child)
        (,+struct/simple+ ((:absolute "no-such-child"))
         no-such-child)
        (,+struct/simple+ ("no-such-child" :if-does-not-exist nil)
         nil))
    (let+ (((&flet do-it () (apply #'lookup struct :field args))))
      (case expected
        (no-such-child (ensure-condition 'no-such-child (do-it)))
        (t             (ensure-same (do-it) expected :test #'eq))))))

(addtest (structure-mixin-root
          :documentation
          "Test method on `direct-dependencies'.")
  direct-dependencies

  (ensure-cases (type expected)
      `((,+struct/simple+    (,+utf-8-string+))
        (,+struct/empty+     ())
        #+later (,+struct/recursive+ (,+uint16+ ,+utf-8-string+ ,THE-ARRAY)))

    (ensure-same (direct-dependencies type) expected :test #'set-equal)))
