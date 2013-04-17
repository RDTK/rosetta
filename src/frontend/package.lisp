;;;; package.lisp --- Package definition for frontend module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage :rosetta.frontend
  (:nicknames
   :rs.f)

  (:use
   :cl
   :alexandria
   :iterate
   :let-plus
   :more-conditions

   :rosetta
   :rosetta.model
   :rosetta.model.data)

  ;; Conditions
  (:export
   :location-condition
   :location

   :builder-condition
   :builder

   :parse-error1

   :parse-warning

   :processing-error

   :processing-warning

   :dependency-error
   :dependency-error-dependency

   :cannot-resolve-dependency
   :dependency-error-locations

   :ambiguous-dependency
   :dependency-error-candidates)

  ;; Restarts
  (:export
   :retry)

;;; Location-related stuff

  ;; Location protocol
  (:export
   :source
   :source-content
   :bounds
   :line
   :column

   :location=)

  ;; Location repository protocol and class
  (:export
   :location-of

   :location-repository)

  ;; Location Utilities
  (:export
   :location-info

   :format-location
   :format-content
   :format-content-with-delimiters)

;;; Parsing- and format-related stuff

  ;; Processing protocol
  (:export
   :process
   :parse)

  ;; Format class family
  (:export
   :no-such-format-class
   :find-format-class
   :format-classes)

  ;; `binary-format-mixin' mixin class
  (:export
   :binary-format-mixin)

  ;; `text-format-mixin' mixin class
  (:export
   :text-format-mixin)

;;; Builder-related stuff

  ;; Comment attaching protocol
  (:export
   :most-recent-comment
   :comment
   :comment?
   :prettify)

  ;; Dependency resolution protocol
  (:export
   :resolve)

  ;; Search path-based resolution protocol and class
  (:export
   :search-path
   :if-ambiguous

   :search-path-resolver)

  ;; Recursive package creation protocol
  (:export
   :ensure-package)

  ;; `location-attach-mixin' mixin class
  (:export
   :location-attaching-mixin)

  ;; `comment-attaching-mixin' mixin class
  (:export
   :comment-attaching-mixin)

  ;; `root-package-creating-mixin' mixin class
  (:export
   :root-package-creating-mixin)

  ;; `lazy-resolver-mixin' mixin class
  (:export
   :lazy-resolver-mixin)

  ;; `dependency-delegating-mixin' mixin class
  (:export
   :dependency-delegating-mixin)

  ;; `source-level-caching-mixin' mixin class
  (:export
   :source-level-caching-mixin)

  ;; `name-normalizing-mixin' mixin class
  (:export
   :name-normalizing-mixin)

  (:documentation
   "This package contains frontend-related protocols and
infrastructure of the rosetta compiler.

* `process'                         [generic function]
* `parse'                           [generic function]

There is a class family of format classes which can be manipulated
using:

* `no-such-format-class'            [condition]
* `find-format-class'               [generic function]
* `format-classes'                  [function]

See

  (documentation SYMBOL 'rs.f:format)"))
