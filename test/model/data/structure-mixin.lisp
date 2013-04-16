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
          "Test the `contents' method specialization
`structure-mixin'.")
  lookup

  (ensure-cases (struct args expected)
      `((,+struct/simple+ ("a")                                    ,(first (contents +struct/simple+ :field)))
        (,+struct/simple+ ("a" :if-does-not-exist nil)             ,(first (contents +struct/simple+ :field)))
        (,+struct/simple+ ("no-such-child")                        no-such-child)
        (,+struct/simple+ ("no-such-child" :if-does-not-exist nil) nil))
    (let+ (((&flet do-it () (apply #'lookup struct :field args))))
      (case expected
        (no-such-child (ensure-condition 'no-such-child (do-it)))
        (t             (ensure-same (do-it) expected :test #'eq))))))

(addtest (structure-mixin-root
          :documentation
          "Test method on `direct-dependencies'.")
  direct-dependencies

  (ensure-cases (type expected)
      `((,+struct/simple+ (,+utf-8-string+)))

    (ensure-same (direct-dependencies type) expected :test #'set-equal)))
