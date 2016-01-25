;;;; package.lisp --- Package definition for frontend module.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rosetta.frontend
  (:nicknames
   #:rs.f)

  (:use
   #:cl
   #:alexandria
   #:iterate
   #:let-plus
   #:more-conditions
   #:print-items

   #:rosetta
   #:rosetta.model
   #:rosetta.model.data)

  (:import-from #:service-provider
   #:find-service
   #:service-providers
   #:find-provider
   #:make-provider)

  ;; Conditions
  (:export
   #:location-condition
   #:location

   #:format-guessing-error

   #:builder-condition
   #:builder

   #:parse-error1

   #:parse-warning

   #:processing-error

   #:processing-warning

   #:dependency-error
   #:dependency-error-dependency

   #:cannot-resolve-dependency
   #:dependency-error-locations

   #:ambiguous-dependency
   #:dependency-error-candidates)

  ;; Restarts
  (:export
   #:retry)

;;; Location-related stuff

  ;; Location protocol
  (:export
   #:source
   #:source-content
   #:bounds
   #:line
   #:column

   #:location=)

  ;; Location repository protocol and class
  (:export
   #:location-of

   #:location-repository)

  ;; Location Utilities
  (:export
   #:location-info

   #:format-location
   #:format-content
   #:format-content-with-delimiters)

;;; Parsing- and format-related stuff

  ;; Processing protocol
  (:export
   #:guess-format

   #:process
   #:parse)

  ;; Format guessing services
  (:export
   #:guess-format/string
   #:guess-format/pathname
   #:guess-format/uri-scheme)

  ;; format mixin classes
  (:export
   #:source-attaching-mixin
   #:common-sources-mixin
   #:binary-format-mixin
   #:text-format-mixin)

;;; Builder-related stuff

  ;; Comment attaching protocol
  (:export
   #:most-recent-comment
   #:comment
   #:comment?
   #:prettify)

  ;; Dependency resolution protocol
  (:export
   #:resolve)

  ;; Search path-based resolution protocol and class
  (:export
   #:search-path
   #:if-ambiguous

   #:merge-locations
   #:probe-location

   #:search-path-resolver)

  ;; Recursive package creation protocol
  (:export
   #:ensure-package)

  ;; Builder mixins
  (:export
   #:location-attaching-mixin
   #:comment-attaching-mixin
   #:root-package-creating-mixin
   #:lazy-resolver-mixin
   #:dependency-delegating-mixin
   #:source-level-caching-mixin
   #:name-normalizing-mixin)

  (:documentation
   "This package contains frontend-related protocols and
    infrastructure of the rosetta compiler.

    * `guess-format'                    [generic function]
    * `process'                         [generic function]
    * `parse'                           [generic function]"))
