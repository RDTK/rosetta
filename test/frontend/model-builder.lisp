;;;; model-builder.lisp --- Unit tests for the model-builder class.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.frontend.test)

(defclass mock-structure-subclass (base-structure)
  ())

(deftestsuite model-builder-root (frontend-root)
  ()
  (:documentation
   "Unit tests for the `model-builder' class."))

(addtest (model-builder-root
          :documentation
          "Smoke test for `model-builder' class.")
  smoke

  (ensure-cases ((spec repository-queries package-queries))
      '(;; Empty structure.
        ((:structure (:qname (:absolute "foo") :name "foo"
                      :class base-structure))

         ((:structure (:absolute "foo")))

         ((:structure (:absolute "foo"))))

        ;; Empty structure with non-default class.
        ((:structure (:qname (:absolute "foo") :name "foo"
                      :class mock-structure-subclass))

         ((:structure (:absolute "foo")))

         ((:structure (:absolute "foo"))))

        ;; Structure with one field.
        ((:structure (:qname (:absolute "bar") :name "bar")
          (:field (:name "baz" :type (:find :fundamental
                                      :category :integer
                                      :width    32
                                      :signed?  t))))

         ((:structure (:absolute "bar")))

         ((:structure (:absolute "bar"))
          (:field     (:absolute "bar" "baz"))))

        ;; Nested structures.
        ((:structure (:qname (:absolute "di") :name "di")
          (:structure (:qname (:absolute "di" "doo") :name "doo")))

         ((:structure (:absolute "di"))
          (:structure (:absolute "di" "doo")))

         ((:structure (:absolute "di"))
          (:nested    (:absolute "di" "doo"))))

        ;; Empty enum.
        ((:enum (:qname (:absolute "fez") :name "fez"
                 :type (:find :fundamental
                        :category :integer
                        :width     32
                        :signed?   t)
                 :class enum))
         ((:enum (:absolute "fez")))

         ((:enum (:absolute "fez"))
          (:enum (:relative "fez"))))

        ;; Enum with one value.
        ((:enum (:qname (:absolute "whoop") :name "whoop"
                 :type (:find :fundamental
                        :category :integer
                        :width    32
                        :signed?  t))
          (:enum-value (:name "dee" :value 1
                        :class enum-value)))

         ((:enum  (:absolute "whoop")))

         ((:enum  (:absolute "whoop"))
          (:value (:absolute "whoop" "dee")))))

    (let+ (((&flet do-it (spec)
              (let+ ((repository (make-instance 'rs.m.d::base-repository))
                     (builder
                      (make-instance
                       (find-builder-class :model)
                       :resolver   (make-instance 'search-path-resolver)
                       :locations  (make-instance 'location-repository)
                       :repository repository))
                     (root-package (lookup repository :package '(:absolute)))
                     ((&flet+ process-arg ((kind &rest args))
                        (apply #'find-node builder kind args)))
                     ((&flet process-args (args)
                        (iter (for (key value) on args :by #'cddr)
                              (collect key)
                              (if (typep value '(cons (eql :find)))
                                  (collect (process-arg (rest value)))
                                  (collect value)))))
                     ((&labels+ do-spec ((kind args &rest children))
                        (reduce (curry #'add-child builder) (mapcar #'do-spec children)
                                :initial-value (apply #'make-node builder kind
                                                      (process-args args))))))
                (add-child builder root-package (do-spec spec))
                (values repository root-package)))))
      (case repository-queries
        (error
         (ensure-condition 'error (do-it spec)))
        (t
         (let+ (((&values repository package) (do-it spec)))
           (iter (for (kind name) in repository-queries)
                 (ensure (lookup repository kind name
                                 :if-does-not-exist nil)))
           (iter iter (for (kind name) in package-queries)
                 (ensure (lookup package kind name
                                 :if-does-not-exist nil))
                 (ensure (lookup package kind (list* :relative (rest name))
                                 :if-does-not-exist nil)))))))))

(addtest (model-builder-root
          :documentation
          "Smoke test for method on `ensure-package'.")
  ensure-package/smoke

  (let+ ((builder (make-instance (find-builder-class :model)
                                 :resolver   (make-instance 'search-path-resolver)
                                 :locations  (make-instance 'location-repository)
                                 :repository (make-instance 'rs.m.d::base-repository)))
         ((&flet do-it (args)
            (apply #'ensure-package builder args))))
    (ensure-cases (args expected)
        `(;; These are invalid.
          ((:qname (:absolute) :name "foo")             incompatible-arguments)
          ((:qname (:absolute "bar" "baz") :name "foo") incompatible-arguments)

          ;; These are OK. Note: repetitions are on purpose.
          ((:qname (:absolute))                         (:absolute))
          ((:qname (:absolute) :name "")                (:absolute))
          ((:qname (:absolute))                         (:absolute))
          ((:qname (:absolute "bar" "baz"))             (:absolute "bar" "baz"))
          ((:qname (:absolute "bar" "baz") :name "baz") (:absolute "bar" "baz"))
          ((:qname (:absolute "bar" "baz"))             (:absolute "bar" "baz")))

        (case expected
          (incompatible-arguments
           (ensure-condition 'incompatible-arguments (do-it args)))
          (t
           (ensure-same (qname (do-it args)) expected :test #'equal))))))
