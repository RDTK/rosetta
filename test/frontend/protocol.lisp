;;;; protocol.lisp --- Unit tests for the protocol of the frontend module.
;;;;
;;;; Copyright (C) 2012, 2013, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.frontend.test)

(deftestsuite rosetta.frontend.protocol-root (frontend-root)
  ()
  (:documentation
   "Unit tests for protocol functions of the frontend module."))

(addtest (rosetta.frontend.protocol-root
          :documentation
          "Smoke test for the `guess-format' function.")
  guess-format/smoke

  (ensure-cases (input expected)
      `(("foo"                            nil)   ; no substring match
        ("mock"                           :mock) ; substring match

        (,#P"foo/"                        nil)   ; no type
        (,#P"foo."                        nil)   ; empty type
        (,#P"foo.bla"                     nil)   ; unknown pathname type
        (,#P"foo.MOCK"                    nil)   ; unknown pathname type
        (,#P"foo.mock"                    :mock) ; pathname type match

        (,(puri:uri "file:///foo/")       nil)   ; all of these should also
        (,(puri:uri "file:///foo.")       nil)   ; work for file URLs
        (,(puri:uri "file:///foo.bar")    nil)
        (,(puri:uri "file:///foo.MOCK")   nil)
        (,(puri:uri "file:///foo.mock")   :mock)

        (,(puri:uri "http://foo/")        nil)   ; all of these should also
        (,(puri:uri "http://foo.")        nil)   ; work for HTTP URLs
        (,(puri:uri "http://foo.bar")     nil)
        (,(puri:uri "http://foo.MOCK")    nil)
        (,(puri:uri "http://foo.mock")    nil)
        (,(puri:uri "http:foo.mock")      :mock)
        (,(puri:uri "http://a/foo.mock")  :mock)

        (,(puri:uri "https://foo/")       nil)   ; all of these should also
        (,(puri:uri "https://foo.")       nil)   ; work for HTTPS URLs
        (,(puri:uri "https://foo.bar")    nil)
        (,(puri:uri "https://foo.MOCK")   nil)
        (,(puri:uri "https://foo.mock")   nil)
        (,(puri:uri "https:foo.mock")     :mock)
        (,(puri:uri "https://a/foo.mock") :mock)

        (,(puri:uri "somescheme:")        nil)   ; guess based on URI
        (,(puri:uri "ss://foo/")          nil)   ; scheme
        (,(puri:uri "ss://foo.")          nil)
        (,(puri:uri "ss://foo.bar")       nil)
        (,(puri:uri "ss://foo/bar.baz")   nil)
        (,(puri:uri "mock:")              :mock) ; should work for known
        (,(puri:uri "mock://foo/")        :mock) ; scheme
        (,(puri:uri "mock://foo.")        :mock)
        (,(puri:uri "mock://foo.bar")     :mock)
        (,(puri:uri "mock://foo/bar.baz") :mock))

    (ensure-same (guess-format input :if-not-guessable nil)
                 (values expected))))

(addtest (rosetta.frontend.protocol-root
          :documentation
          "Test the restarts established by `guess-format'.")
  guess-format/restarts

  ;; Restarts specs in RESTART-SPECS are processed and consumed
  ;; sequentially.
  (ensure-cases (source restart-specs expected)
      `(("does-not-matter"    ((use-value :foo))       :foo)
        ("does-not-matter"    (retry (use-value :foo)) :foo)

        (,#P"does-not-matter" ((use-value :foo))       :foo)
        (,#P"does-not-matter" (retry (use-value :foo)) :foo)

        (,(puri:uri "ss:")    ((use-value :foo))       :foo)
        (,(puri:uri "ss:")    (retry (use-value :foo)) :foo))

    (ensure-same
     (handler-bind ((format-guessing-error
                      (lambda (condition)
                        (let ((spec (ensure-list (pop restart-specs))))
                          ;; Make sure the condition prints properly.
                          (princ-to-string condition)
                          ;; Make sure the restart prints properly.
                          (princ-to-string (find-restart (first spec)))
                          ;; Invoke it.
                          (apply #'invoke-restart spec)))))
       (guess-format source))
     expected
     :test #'equal)))

(addtest (rosetta.frontend.protocol-root
          :documentation
          "Test the format and builder lookup performed by
`process'.")
  process/lookup

  (ensure-cases (args expected)
      `(;; Invalid format.
        ((:no-such-format   #P"does-not-matter"  list)               no-such-format-class)
        (((:no-such-format) #P"does-not-matter"  list)               no-such-format-class)
        ((:guess            #P"1.no-such-format" list)               format-guessing-error)

        ;; Invalid builder.
        ((:mock             #P"does-not-matter"  :no-such-builder)   no-such-builder-class)
        ((:mock             #P"does-not-matter"  (:no-such-builder)) no-such-builder-class)

        ;; These are OK.
        ((:mock             #P"does-not-matter"  list)               t)
        ((:mock             #P"does-not-matter"  :mock)              t)
        ((:mock             (#P"1" #P"2")        list)               (cons t (cons t null)))
        ((:mock             ,#(#P"1" #P"2")      list)               (cons t (cons t null)))
        ((:mock             "just-a-string"      list)               t)

        ;; Format guessing.
        ((:guess            #P"file.mock"        list)               t)
        ((:guess            (#P"1.mock" #P"2.mock") list)            (cons t (cons t null)))
        (((:guess :fail t)  #P"file.mock"        list)               format-guessing-error))
    (let+ (((&flet do-it () (apply #'process args))))
      (case expected
        (format-guessing-error
         (ensure-condition 'format-guessing-error (do-it)))
        (no-such-format-class
         (ensure-condition 'service-provider:missing-provider-error
           (do-it)))
        (no-such-builder-class
         (ensure-condition 'service-provider:missing-provider-error
           (do-it)))
        (t
         (ensure (typep (do-it) expected)))))))

(addtest (rosetta.frontend.protocol-root
          :documentation
          "Test the restarts established by `process'.")
  process/restarts

  ;; Restarts specs in RESTART-SPECS are processed and consumed
  ;; sequentially.
  (ensure-cases (sources restart-specs expected)
      `((,#P"does-not-matter" ((use-value :foo))                        :foo)
        ("does-not-matter"    ((use-value :foo))                        :foo)
        (,#P"does-not-matter" (retry (use-value :foo))                  :foo)
        ((,#P"1" ,#P"2")      ((use-value :foo) retry (use-value :bar)) (:foo :bar))
        (("1"    ,#P"2")      ((use-value :foo) retry (use-value :bar)) (:foo :bar))
        ((,#P"1" ,#P"2")      (continue (use-value :bar))               (:bar))
        ((,#P"1" ,#P"2")      ((use-value :foo) continue)               (:foo)))

    (ensure-same
     (handler-bind ((error (lambda (condition)
                             (let ((spec (ensure-list (pop restart-specs))))
                               ;; Make sure the condition prints properly.
                               (princ-to-string condition)
                               ;; Make sure the restart prints properly.
                               (princ-to-string (find-restart (first spec)))
                               ;; Invoke it.
                               (apply #'invoke-restart spec)))))
       (process :mock sources :error))
     expected
     :test #'equal)))

(addtest (rosetta.frontend.protocol-root
          :documentation
          "Test the restarts established by `resolve'.")
  resolve/restarts

  ;; Restarts specs in RESTART-SPECS are processed and consumed
  ;; sequentially.
  (ensure-cases (format pathname  restart-specs expected)
      `((nil   ,#P"does-not-matter" ((use-value :foo :bar))       (:foo :bar))
        (nil   ,#P"does-not-matter" (retry (use-value :foo :bar)) (:foo :bar))
        (nil   ,#P"does-not-matter" (continue)                    nil)
        (nil   ,#P"does-not-matter" (retry continue)              nil)

        (:mock ,#P"does-not-matter" ((use-value :foo :bar))       (:foo :bar))
        (:mock ,#P"does-not-matter" (retry (use-value :foo :bar)) (:foo :bar))
        (:mock ,#P"does-not-matter" (continue)                    nil)
        (:mock ,#P"does-not-matter" (retry continue)              nil)

        (nil   (or ,#P"1" ,#P"2")   ((use-value :foo :bar))       (:foo :bar))
        (nil   (or ,#P"1" ,#P"2")   (retry (use-value :foo :bar)) (:foo :bar))
        (nil   (or ,#P"1" ,#P"2")   (continue)                    nil)
        (nil   (or ,#P"1" ,#P"2")   (retry continue)              nil)

        (:mock (or ,#P"1" ,#P"2")   ((use-value :foo :bar))       (:foo :bar))
        (:mock (or ,#P"1" ,#P"2")   (retry (use-value :foo :bar)) (:foo :bar))
        (:mock (or ,#P"1" ,#P"2")   (continue)                    nil)
        (:mock (or ,#P"1" ,#P"2")   (retry continue)              nil))

    (ensure-same
     (handler-bind ((error (lambda (condition)
                             (let ((spec (ensure-list (pop restart-specs))))
                               ;; Make sure the condition prints properly.
                               (princ-to-string condition)
                               ;; Make sure the restart prints properly.
                               (princ-to-string (find-restart (first spec)))
                               ;; Invoke it.
                               (apply #'invoke-restart spec)))))
       (resolve (make-instance 'mock-resolver :fail? t) format pathname))
     (apply #'values expected)
     :test #'equal)))
