;;;; model-builder.lisp --- Unit tests for the model-builder class.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rosetta.frontend.test)

(deftestsuite model-builder-root (frontend-root)
  ()
  (:documentation
   "Unit tests for the `model-builder' class."))

(addtest (model-builder-root
          :documentation
          "Smoke test for `model-builder' class.")
  smoke

  (let+ ((builder (make-instance (find-builder-class :model)
                                 :resolver   (make-instance 'search-path-resolver)
                                 :locations  (make-instance 'location-repository)
                                 :repository (make-instance 'rs.m.d::base-repository)))
         ((&flet+ process-arg ((kind &rest args))
            (apply #'find-node builder kind args)))
         ((&flet process-args (args)
            (iter (for (key value) on args :by #'cddr)
                  (collect key)
                  (if (typep value '(cons (eql :find)))
                      (collect (process-arg (rest value)))
                      (collect value)))))
         ((&labels+ do-it ((kind args &rest children))
            (reduce (curry #'add-child builder) (mapcar #'do-it children)
                    :initial-value (apply #'make-node builder kind
                                          (process-args args))))))

    (ensure-cases (spec)
        '(;; Empty structure.
          (:structure (:qname (:absolute "foo") :name "foo"))
          ;; Structure with one field.
          (:structure (:qname (:absolute "bar") :name "bar")
           (:field (:name "baz" :type (:find :fundamental
                                             :category :integer
                                             :width    32
                                             :signed?  t))))
          ;; Empty enum.
          (:enum (:qname (:absolute "fez") :name "fez"
                  :type (:find :fundamental
                               :category :integer
                               :width     32
                               :signed?   t)))
          ;; Enum with one value.
          (:enum (:qname (:absolute "whoop") :name "whoop"
                  :type (:find :fundamental
                               :category :integer
                               :width    32
                               :signed?  t))
           (:enum-value (:name "dee" :value 1))))

      (add-child builder
                 (find-node builder :package :qname '(:absolute))
                 (do-it spec)))))

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
