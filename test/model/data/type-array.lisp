;;;; type-array.lisp --- Unit tests for the type-array class.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.model.data.test)

(deftestsuite model-data-type-array-root (model-data-root)
  ()
  (:documentation
   "Unit tests for the `base-array' class."))

(define-print-items-test (base-array :suite model-data-type-array-root)
  `((:element-type ,+uint32+
     :index-type   ,+singleton/uint32+)   "UINT32[1]")
  `((:element-type ,+uint32+
     :index-type   (,+singleton/uint32+
                    ,+singleton/uint32+)) "UINT32[1, 1]")
  `((:element-type ,+utf-8-string+
     :index-type   ,+uint32+)             "UTF-8-STRING[*]")
  `((:element-type ,+utf-8-string+
     :index-type   (,+uint32+ ,+uint32+)) "UTF-8-STRING[*, *]")
  `((:element-type ,+struct/simple+
     :index-type   (,+uint32+ ,+uint32+)) "simple[*, *]"))
