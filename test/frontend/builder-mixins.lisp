;;;; builder-mixins.lisp --- Test for the builder mixins of the frontend module.
;;;;
;;;; Copyright (C) 2012, 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.frontend.test)

(deftestsuite builder-mixins-root (frontend-root)
  ()
  (:documentation
   "Tests for the builder mixin classes provided by the frontend
module."))

(defmacro define-builder-mixin-suite (class (&rest mixins) &body options)
  "Define a test suite for the mixin class CLASS. Add MIXINS as
superclasses to the generated mock builder classes. OPTIONS can contain

  (:initargs INITARGS)

Where INITARGS is a list of initargs expected by the mock builder
classes."
  (let+ ((suite-name (format-symbol *package* "~A-ROOT"         class))
         (mock-name  (format-symbol *package* "~A-MOCK-BUILDER" class))
         ((&plist-r/o (initargs :initargs)) (reduce #'append options)))
    `(progn
       (defclass ,mock-name (,class
                             ,@mixins
                             mock-builder)
         ())

       (deftestsuite ,suite-name (builder-mixins-root)
         ()
         (:documentation
          ,(format nil "Tests for the `~(~A~)' mixin class."
                   class)))

       (addtest (,suite-name
                 :documentation
                 ,(format nil "Test constructing a `~(~A~)' instance."
                          class))
         construct/smoke

         (make-instance ',mock-name ,@initargs)))))

(defmacro ensure-builder-cases ((class &rest initargs)
                                (&body cases)
                                &body body)
  "Execute BODY with an instance of CLASS (constructed with INITARGS)
for CASES entries of which have to be of the form

  (INITARGS ((FORMAT1 SOURCE1) (FORMAT2 SOURCE2) ...) EXPECTED)

where INITARGS is a case-specific plist of initargs that should be
passed to the constructed builder instance in addition to INITARGS and
FORMATN and SOURCEN have to be suitable arguments for `parse' and
EXPECTED can be `parse-error1', `processing-error' or some other
object which is then used in BODY."
  (let ((result-var 'result))
    `(ensure-cases (initargs formats-and-sources expected)
         (list ,@cases)

       (let+ ((builder  (apply #'make-instance ',class
                               (append initargs (list ,@initargs))))
              (formats  (mapcar #'first formats-and-sources))
              (sources  (mapcar #'second formats-and-sources))
              ((&flet do-it ()
                 (lastcar (mapcar (rcurry #'process builder) formats sources)))))
         (case expected
           (parse-error1     (ensure-condition 'parse-error1 (do-it)))
           (processing-error (ensure-condition 'processing-error (do-it)))
           (t                (let ((,result-var (do-it))) ,@body)))))))

;;; `location-attaching-mixin' mixin class

;; We need `dependency-delegating-mixin' to reach a different source
;; via an import.
(define-builder-mixin-suite location-attaching-mixin
    (dependency-delegating-mixin)
  (:initargs (:locations (make-instance 'location-repository)
              :resolver  (make-instance 'mock-resolver))))

(addtest (location-attaching-mixin-root
          :documentation
          "Test attaching of locations during `parse'.")
  parse/smoke

  (let ((bounds '((:bounds (1 . 2))                   ; package
                  (:source         #P"some-file.mock" ; imported structure
                   :source-content nil                ; resides in different
                   :bounds         (2 . 10))          ; source
                  (:bounds (3 . 4))                   ; comment1
                  (:bounds (4 . 5))                   ; comment2
                  (:bounds (5 . 6))                   ; field
                  (:bounds (3 . 6)))))                ; structure
    (ensure-builder-cases (location-attaching-mixin-mock-builder
                           :locations (make-instance 'location-repository)
                           :resolver  (make-instance 'mock-resolver))
        (`(nil ((:mock #P"does-not-matter")) (() ,@bounds))                       ; all
         `(nil ((:mock "does-not-matter"))   ((:source-content "does-not-matter") ; all
                                              ,@bounds)))

      (let+ (((expected/all
               expected/package expected/import
               expected/comment1 expected/comment2
               expected/field expected/structure) expected)
             ((&whole package &ign
                      (import
                       (&whole structure &ign (comment1 comment2 field) &rest &ign)
                       resolved)
                      &rest &ign) result)
             ((&flet remove-duplicates/plist (list)
                (alist-plist (remove-duplicates (plist-alist list)
                                                :key #'car :from-end t))))
             ((&flet ensure-location (node expected)
                (ensure-same (location-of (locations builder) node)
                             (apply #'make-instance 'location-info
                                    (remove-duplicates/plist
                                     (append expected
                                             expected/all
                                             (list :source (first sources)))))
                             :report    "Incorrect location for node ~A"
                             :arguments (node)
                             :test      #'location=))))
        (ensure-location package   expected/package)
        (ensure-location import    expected/import)
        (ensure-location comment1  expected/comment1)
        (ensure-location comment2  expected/comment2)
        (ensure-location field     expected/field)
        (ensure-location structure expected/structure)))))

;;; `comment-attaching-mixin' mixin class

(define-builder-mixin-suite comment-attaching-mixin ())

(addtest (comment-attaching-mixin-root
          :documentation
          "Test attaching of comments during `parse'.")
  parse/smoke

  (ensure-builder-cases (comment-attaching-mixin-mock-builder)
      ('(nil ((:mock "does-not-matter")) (nil "comment1
comment2")))

    (let+ (((expected/structure expected/field) expected)
           ((&whole package &ign
             (import (&whole structure &ign (field) &rest &ign) &ign)
             &rest &ign) result))
      (ensure-same (comment builder structure) expected/structure)
      (ensure-same (comment builder field)     expected/field))))

(addtest (comment-attaching-mixin-root
          :documentation
          "Test `prettify' methods.")
  prettify/smoke

  (let ((builder (make-instance 'comment-attaching-mixin-mock-builder)))
    (ensure-cases (input-lines expected)
        '(;; Different kind of empty comments.
          (nil                  "")
          (("")                 "")
          (("" "")              "")

          ;; Newline stripping.
          (("" "foo" "")        "foo")

          ;; Stripping of common whitespace prefix.
          (("  " "foo")         "foo")
          (("  foo" "  bar")    "foo
bar")
          (("    foo" "  bar")  "  foo
bar"))
      (ensure-same (prettify builder input-lines) expected
                   :test #'string=))))

(addtest (comment-attaching-mixin-root
          :documentation
          "Test interaction of comment attaching and
`ensure-package'.")
  ensure-package

  ;;; TODO(jmoringe, 2012-11-28): use simple builder
  (let* ((builder (service-provider:make-provider
                   'rosetta.model.data::builder :model
                   :repository (make-instance 'rs.m.d::base-repository)))
         (root    (let ((package (ensure-package
                                  builder :qname '(:absolute))))
                    (add-child builder package "at root")))
         (bar     (let ((package (ensure-package
                                  builder :qname '(:absolute "bar"))))
                    (add-child builder package "at bar")))
         (baz     (ensure-package builder :qname '(:absolute "bar" "baz"))))
    (ensure-same (documentation1 root) nil :test #'equal)
    (ensure-same (documentation1 bar)  "at root" :test #'equal)
    (ensure-same (documentation1 baz)  "at bar" :test #'equal)))

;;; `root-package-creating-mixin' mixin class

(define-builder-mixin-suite root-package-creating-mixin ())

(addtest (root-package-creating-mixin-root
          :documentation
          "Smoke test for method on `ensure-package'.")
  ensure-package/smoke

  (let ((builder (make-instance 'root-package-creating-mixin-mock-builder)))
    (ensure-package builder :qname '(:absolute))
    (ensure-package builder :qname '(:absolute))
    (ensure-package builder :qname '(:absolute "bar" "baz"))
    (ensure-package builder :qname '(:absolute "bar" "baz"))))

;;; `lazy-resolver-mixin' mixin class

(define-builder-mixin-suite lazy-resolver-mixin ()
  (:initargs (:repository (make-instance 'rs.m.d::base-repository))))

(addtest (lazy-resolver-mixin-root
          :documentation
          "Test creation of forward references during `parse'.")
  parse/smoke

  (ensure-builder-cases (lazy-resolver-mixin-mock-builder
                         :repository (make-instance 'rs.m.d::base-repository))
      ('(nil ((:mock "does-not-matter"))   nil)
       '(nil ((:mock "really-unresolved")) processing-error))))

;;; `dependency-delegating-mixin' mixin class

(define-builder-mixin-suite dependency-delegating-mixin ()
  (:initargs (:resolver (make-instance 'mock-resolver))))

(addtest (dependency-delegating-mixin-root
          :documentation
          "Test delegation of dependency resolution during `parse'.")
  parse/smoke

  (ensure-builder-cases (dependency-delegating-mixin-mock-builder
                         :resolver (make-instance 'mock-resolver))
      ('(nil ((:mock "does-not-matter")) ((:mock #P"some-file.mock")))
       '(nil ((:mock (or "one" "two")))  ((:mock #P"some-file.mock")
                                          (:mock #P"some-file.mock")
                                          (:mock #P"some-file.mock"))))

    (ensure-same (calls (resolver builder)) expected)))

;;; `source-level-caching-mixin' mixin class

(define-builder-mixin-suite source-level-caching-mixin ())

(addtest (source-level-caching-mixin-root
          :documentation
          "Test management of duplicate parsing requests.")
  parse/smoke

  (let* ((source1 "some source")
         (source2 "some source")
         (source3 "some other source")
         (source4 #.(or *compile-file-truename* *load-truename*))
         (source5 (merge-pathnames "../frontend/" source4))
         (source6 #P"/does-not-exist"))
    (ensure-builder-cases (source-level-caching-mixin-mock-builder)
        (;; A non-existing source cannot be cached (because of lacking
         ;; a truename), but should not signal an error from the cache
         ;; either.
         `(nil ((:mock ,source6))                  (,source6))

         ;; All sources equal => should produce exactly one `parse'
         ;; call.
         `(nil ((:mock ,source1))                  (,source1))
         `(nil ((:mock ,source1) (:mock ,source1)) (,source1)) ; `eq' sources
         `(nil ((:mock ,source1) (:mock ,source2)) (,source1)) ; `equal' sources
         `(nil ((:mock ,source4) (:mock ,source4)) (,source4))
         `(nil ((:mock ,source4) (:mock ,source5)) (,source4)) ; `equal' under `truename'
         `(nil ((:mock ,source5) (:mock ,source4)) (,source5)) ; likewise

         ;; Some sources not equal => should produce multiple `parse'
         ;; calls.
         `(nil ((:mock ,source1) (:mock ,source2)
                (:mock ,source3))                  (,source1 ,source3)))

     (ensure-same (mapcar #'second (calls builder)) expected
                  :test #'equal))))

;;; `name-normalizing-mixin' mixin class

(define-builder-mixin-suite name-normalizing-mixin ()
  (:initargs (:normalizer #'normalize-name)))

(addtest (name-normalizing-mixin-root
          :documentation
          "Test transparent name normalization performed.")
  parse/smoke

  (ensure-builder-cases (name-normalizing-mixin-mock-builder)
      (`((:normalizer ,#'string-capitalize) ((:mock #P"does-not-matter"))
         ((:relative "Foo" "Test")
          (:relative "Foo" "Test")
          (:relative "Foo" "Test" "Field")))
       `((:normalizer ,#'string-upcase)     ((:mock #P"does-not-matter"))
         ((:relative "FOO" "TEST")
          (:relative "FOO" "TEST")
          (:relative "FOO" "TEST" "FIELD")))
       `((:normalizer ,#'normalize-name)    ((:mock #P"does-not-matter"))
         ((:relative "foo" "test")
          (:relative "foo" "test")
          (:relative "foo" "test" "field"))))
    (let ((root `(:package (,result) :name "")))
      (iter (for spec in expected) (ensure (lookup root t spec))))))
