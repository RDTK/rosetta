;;; package.lisp --- Package definition for unit tests of the frontend module.
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

(cl:defpackage :rosetta.frontend.test
  (:use
   :cl
   :alexandria
   :let-plus
   :lift

   :rosetta.model.data

   :rosetta.frontend)

  (:import-from :rosetta.frontend
   :guess-format)

  (:export
   :frontend-root)

  (:documentation
   "This package contains unit tests for the rosetta.frontend system"))

(cl:in-package :rosetta.frontend.test)

(deftestsuite frontend-root (root)
  ()
  (:documentation
   "Root unit test suite for the rosetta.frontend system."))


;;; `format-mock' mock class
;;

(defmethod find-format-class ((spec (eql :mock)))
  (find-class 'format-mock))

(defclass format-mock ()
  ()
  (:documentation
   "Mock format for unit tests."))

(defmethod parse ((format  format-mock)
		  (source  t)
		  (builder t)
		  &key &allow-other-keys)
  (when (equal source #P"some-file.mock")
    (return-from parse
      (make-node builder :structure
		 :name   "foo"
		 :qname  '(:absolute "bar")
		 :bounds '(2 . 10))))

  (let* ((package    (make-node builder :package
				:qname  '(:absolute "foo")
				:bounds '(1 . 2)))
	 (import     (make-node builder :dependency/file
				:format   :mock
				:pathname #P"some-file.mock"
				:bounds   '(2 . 3)))
	 (comment1   (make-node builder :comment
				:content "comment1"
				:bounds  '(3 . 4)))
	 (comment2   (make-node builder :comment
				:content "comment2"
				:bounds  '(4 . 5)))
	 (unresolved (find-node builder :structure
				:qname  '(:absolute "unresolved")
				:bounds '(5 . 6)))
	 (field      (make-node builder :field
				:name   "field"
				:type   unresolved
				:bounds '(5 . 6)))
	 (structure  (make-node builder :structure
				:name   "test"
				:qname  '(:absolute "test")
				:bounds '(3 . 6)))
	 (name       (if (equal source "really-unresolved")
			 "really-unresolved" "unresolved"))
	 (resolved   (make-node builder :structure
				:name   name
				:qname  `(:absolute ,name)
				:bounds '(6 . 7))))
    (add-child builder package import)
    (add-child builder structure comment1)
    (add-child builder structure comment2)
    (add-child builder structure field)
    (add-child builder package structure)
    (add-child builder package resolved)))


;;; `mock-builder' mock class
;;

(defclass mock-builder ()
  ()
  (:documentation
   "Mock builder for unit tests. Delegates to list builder."))

(defmethod find-node ((builder mock-builder) (kind t) &rest args)
  (apply #'find-node 'list kind args))

(defmethod make-node ((builder mock-builder) (kind t) &rest args)
  ;; Workaround for `change-class': we return a proper CLOS instance
  ;; so the `forward-reference' instance can `change-class'.
  (if (equal (getf args :name) "unresolved")
      (make-instance 'rs.m.d:type-uint8)
      (apply #'make-node 'list kind args)))

(defmethod make-node ((builder mock-builder) (kind (eql :comment))
		      &key
		      content
		      &allow-other-keys)
  content)

(defmethod add-child ((builder mock-builder) (parent t) (child t))
  (add-child 'list parent child))


;;; `mock-resolver' mock class
;;

(defclass mock-resolver ()
  ((calls :initarg  :calls
	  :type     list
	  :accessor calls
	  :initform nil
	  :documentation
	  "Stores the arguments to all `resolve' calls."))
  (:documentation
   "A mock resolver which simply stores the arguments to `resolve'
calls."))

(defmethod resolve ((resolver mock-resolver)
		    (format   t)
		    (location t))
  (push (list format location) (calls resolver))
  (values format location))
