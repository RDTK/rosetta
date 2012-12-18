;;; package.lisp --- Package definition for unit tests of the model.data module.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
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

(cl:defpackage :rosetta.model.data.test
  (:use
   :cl
   :alexandria
   :let-plus
   :iterate
   :more-conditions
   :lift

   :rosetta
   :rosetta.model
   :rosetta.model.data

   :rosetta.test
   :rosetta.model.test)

  (:shadow
   :root)

  ;; Some simple types
  (:export
   :+enum/uint8/simple+
   :+enum/uint8/one+
   :+enum/uint32/simple+

   :+struct/simple+)

  (:documentation
   "This package contains unit tests for the model.data module."))

(cl:in-package :rosetta.model.data.test)

(deftestsuite model-data-root (model-root)
  ()
  (:documentation
   "Root unit test suite for the model.data module."))


;;; `mock-type/validate-value'
;;

(defclass mock-type/validate-value ()
  ())

(defmethod validate-value ((type  mock-type/validate-value)
			   (value t)
			   &key &allow-other-keys)
  (error "~@<Mock value validation error.~@:>"))


;;; Simple data types
;;

(defparameter +enum/uint8/simple+
  (make-instance 'enum :name   "simple/uint8"
		       :type   (make-instance 'type-uint8)
		       :values '(:a 1 :b 2))
  "A simple uint8 enum with two values.")

(defparameter +enum/uint8/one+
  (make-instance 'enum :name   "one/uint8"
		       :type   (make-instance 'type-uint8)
		       :values '(:a 1))
  "A simple uint8 enum with a single value.")

(defparameter +enum/uint32/simple+
  (make-instance 'enum :name   "simple/uint32"
		       :type   (make-instance 'type-uint32)
		       :values '(:a 1 :b 2))
  "A simple uint32 enum with two values.")

(defparameter +struct/simple+
  (make-instance
   'base-structure
   :name   "simple"
   :fields (list (make-instance
		  'base-field
		  :name "a"
		  :type (make-instance 'type-utf-8-string))))
  "A simple structure with a single field.")
