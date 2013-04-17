;;;; resolvers.lisp --- Unit tests for resolver classes.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rosetta.frontend.test)

(defvar +absolute-pathname-of-this-file+
  #.(or *compile-file-truename* *load-truename*))

(defvar +relative-pathname-of-this-file+
  (make-pathname
   :directory nil :defaults +absolute-pathname-of-this-file+))

(defvar +format-of-this-file+
  (make-keyword
   (string-upcase
    (pathname-type +relative-pathname-of-this-file+))))

(defvar +directory-of-this-file+
  (make-pathname
   :name nil :type nil :defaults +absolute-pathname-of-this-file+))

(deftestsuite rosetta.frontend.search-path-resolver-root (frontend-root)
  ()
  (:documentation
   "Unit tests for the `search-path-resolver' class."))

(addtest (rosetta.frontend.search-path-resolver-root
          :documentation
          "Smoke test for the `resolve' generic function with
searchpath-based resolver.")
  resolve/smoke

  (ensure-cases (resolver-spec args expected)
      `((:no-resolver                                   ; resolver-spec
         (nil #P"does-not-matter.proto")                ; format, location
         cannot-resolve-dependency)                     ; expected
        (:no-resolver
         (:does-not-matter #P"does-not-matter.proto")
         cannot-resolve-dependency)
        (:no-resolver
         (nil (or #P"foo.proto" #P"bar.proto"))
         cannot-resolve-dependency)

        ;; Searchpath-based resolver
        ((search-path-resolver :search-path ())         ; fail without path
         (nil #P"does-not-matter.proto")
         cannot-resolve-dependency)

        ((search-path-resolver :search-path ())
         (:does-not-matter #P"does-not-matter.proto")
         cannot-resolve-dependency)

        ;; These ones should be found.
        ((search-path-resolver)
         (nil ,+absolute-pathname-of-this-file+)
         (,+format-of-this-file+ ,+absolute-pathname-of-this-file+))

        ((search-path-resolver :search-path (,+directory-of-this-file+))
         (nil ,+relative-pathname-of-this-file+)
         (,+format-of-this-file+ ,+absolute-pathname-of-this-file+))

        ((search-path-resolver :search-path (,+directory-of-this-file+))
         (:foo ,+relative-pathname-of-this-file+)
         (:foo ,+absolute-pathname-of-this-file+))

        ((search-path-resolver :search-path (,+directory-of-this-file+))
         (nil (or #P"distractor.proto" ,+relative-pathname-of-this-file+))
         (,+format-of-this-file+ ,+absolute-pathname-of-this-file+))

        ;; Ambiguity: test error, using first candidate and continue
        ;; restart.
        ((search-path-resolver :search-path (,+directory-of-this-file+
                                             ,+directory-of-this-file+))
         (nil ,+relative-pathname-of-this-file+)
         ambiguous-dependency)

        ((search-path-resolver :search-path (,+directory-of-this-file+
                                             ,+directory-of-this-file+)
                               :if-ambiguous :first)
         (nil ,+relative-pathname-of-this-file+)
         (,+format-of-this-file+ ,+absolute-pathname-of-this-file+))

        ((search-path-resolver :search-path (,+directory-of-this-file+
                                             ,+directory-of-this-file+)
                               :if-ambiguous ,#'continue)
         (nil ,+relative-pathname-of-this-file+)
         (,+format-of-this-file+ ,+absolute-pathname-of-this-file+)))

    (let+ (((&flet make-resolver ()
              (case resolver-spec
                (:no-resolver resolver-spec)
                (t            (apply #'make-instance resolver-spec)))))
           ((&flet do-it (if-does-not-exist)
              (apply #'resolve (make-resolver)
                     (append
                      args (list :if-does-not-exist if-does-not-exist))))))
      (case expected
        ;; If we expect an error, test with and without suppression.
        (cannot-resolve-dependency
         (ensure-condition 'cannot-resolve-dependency (do-it #'error))
         (ensure-same (do-it nil) (values nil nil)))
        (ambiguous-dependency
         (ensure-condition 'ambiguous-dependency (do-it #'error)))
        (t
         (ensure-same (do-it #'error) (values-list expected)))))))
