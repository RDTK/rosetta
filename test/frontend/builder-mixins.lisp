;;; builder-mixins.lisp --- Test for the builder mixins of the frontend module.
;;
;; Copyright (C) 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the GNU Lesser General
;; Public License Version 3 (the ``LGPL''), or (at your option) any
;; later version.
;;
;; Software distributed under the License is distributed on an ``AS
;; IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the LGPL for the specific language governing rights
;; and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html or
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

(cl:in-package :rosetta.frontend.test)

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

  (((FORMAT1 SOURCE1) (FORMAT2 SOURCE2) ...) EXPECTED)

where FORMATN and SOURCEN have to be suitable arguments for `parse'
and EXPECTED can be `parse-error1', `processing-error' or some other
object which is then used in BODY."
  (let ((result-var 'result))
    `(ensure-cases (formats-and-sources expected)
	 (list ,@cases)

       (let+ ((builder (make-instance ',class ,@initargs))
	      (formats (mapcar #'first formats-and-sources))
	      (sources (mapcar #'second formats-and-sources))
	      ((&flet do-it ()
		 (lastcar (mapcar (rcurry #'process builder) formats sources)))))
	 (case expected
	   (parse-error1     (ensure-condition 'parse-error1 (do-it)))
	   (processing-error (ensure-condition 'processing-error (do-it)))
	   (t                (let ((,result-var (do-it))) ,@body)))))))


;;; `location-attaching-mixin' mixin class
;;

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
	(`(((:mock #P"does-not-matter")) (()                                  ; all
					  ,@bounds))
	 `(((:mock "does-not-matter"))   ((:source-content "does-not-matter") ; all
					  ,@bounds)))

      (let+ (((expected/all
	       expected/package expected/import
	       expected/comment1 expected/comment2
	       expected/field expected/structure) expected)
	     ((&whole package nil
		      (import
		       (&whole structure nil (comment1 comment2 field) &rest nil)
		       resolved)
		      &rest nil) result)
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
;;

(define-builder-mixin-suite comment-attaching-mixin ())

(addtest (comment-attaching-mixin-root
          :documentation
	  "Test attaching of comments during `parse'.")
  parse/smoke

  (ensure-builder-cases (comment-attaching-mixin-mock-builder)
      ('(((:mock "does-not-matter")) (nil "comment1
comment2")))

    (let+ (((expected/structure expected/field) expected)
	   ((&whole package nil
	     (import (&whole structure nil (field) &rest nil) nil)
	     &rest nil) result))
      (ensure-same (comment builder structure) expected/structure)
      (ensure-same (comment builder field)     expected/field))))

(addtest (comment-attaching-mixin-root
          :documentation
	  "Test interaction of comment attaching and
`ensure-package'.")
  ensure-package

  ;;; TODO(jmoringe, 2012-11-28): use simple builder
  (let* ((builder (make-instance (find-builder-class :model)
				 :resolver   (make-instance 'search-path-resolver)
				 :locations  (make-instance 'location-repository)
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
;;

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
;;

(define-builder-mixin-suite lazy-resolver-mixin ()
  (:initargs (:repository (make-instance 'rs.m.d::base-repository))))

(addtest (lazy-resolver-mixin-root
          :documentation
	  "Test creation of forward references during `parse'.")
  parse/smoke

  (ensure-builder-cases (lazy-resolver-mixin-mock-builder
			 :repository (make-instance 'rs.m.d::base-repository))
      ('(((:mock "does-not-matter"))   nil)
       '(((:mock "really-unresolved")) processing-error))))


;;; `dependency-delegating-mixin' mixin class
;;

(define-builder-mixin-suite dependency-delegating-mixin ()
  (:initargs (:resolver (make-instance 'mock-resolver))))

(addtest (dependency-delegating-mixin-root
          :documentation
	  "Test delegation of dependency resolution during `parse'.")
  parse/smoke

  (ensure-builder-cases (dependency-delegating-mixin-mock-builder
			 :resolver (make-instance 'mock-resolver))
      ('(((:mock "does-not-matter")) ((:mock #P"some-file.mock"))))

    (ensure-same (calls (resolver builder)) expected)))


;;; `source-level-caching-mixin' mixin class
;;

(define-builder-mixin-suite source-level-caching-mixin ())

(addtest (source-level-caching-mixin-root
          :documentation
	  "Test management of duplicate parsing requests.")
  parse/smoke

  (let ((source1 "some source")
	(source2 "some source")
	(source3 "some other source")
	(source4 #.(or *compile-file-truename* *load-truename*))
	(source5 (merge-pathnames "../frontend/"
				  #.(or *compile-file-truename* *load-truename*))))
   (ensure-builder-cases (source-level-caching-mixin-mock-builder)
       (;; All sources equal => should produce exactly one `parse'
	;; call.
	`(((:mock ,source1))                  (,source1))
	`(((:mock ,source1) (:mock ,source1)) (,source1)) ; `eq' sources
	`(((:mock ,source1) (:mock ,source2)) (,source1)) ; `equal' sources
	`(((:mock ,source4) (:mock ,source4)) (,source4))
	`(((:mock ,source4) (:mock ,source5)) (,source4)) ; `equal' under `truename'
	`(((:mock ,source5) (:mock ,source4)) (,source5)) ; likewise

	;; Some sources not equal => should produce multiple `parse'
	;; calls.
	`(((:mock ,source1) (:mock ,source2)
	   (:mock ,source3))                  (,source1 ,source3)))

     (ensure-same (mapcar #'second (calls builder)) expected
		  :test #'equal))))
