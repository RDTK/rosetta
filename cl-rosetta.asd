;;; cl-rosetta.asd ---
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(defpackage :cl-rosetta-system
  (:use
   :cl
   :asdf))

(in-package :cl-rosetta-system)

(defsystem :cl-rosetta
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.1.0"
  :license     "GPL3; see COPYING file for details."
  :description "Cross-compiler for robotic systems components and frameworks."
  :depends-on  (:alexandria
		:split-sequence
		:cl-protobuf
		:yacc
		:cxml-location)
  :components  ((:module     "ros-frontend"
		 :pathname   "src/ros/frontend"
		 :components ((:file       "package")
			      (:file       "serialization"
			       :depends-on ("package")) ;; TODO more like backend

			      (:file       "lexer"
			       :depends-on ("package"))
			      (:file       "message"
			       :depends-on ("package" "lexer"))
			      (:file       "service"
			       :depends-on ("package" "lexer" "message"))

			      (:file       "pack"
			       :depends-on ("package" "message" "service"))))

		(:module     "yarp"
		 :pathname   "src/yarp"
		 :components ((:file       "package")
			      (:file       "types"
			       :depends-on ("package"))
			      (:file       "bottle"
			       :depends-on ("package" "types"))))

		(:module     "yarp-backend"
		 :pathname   "src/yarp/backend"
		 :components ((:file       "package")
			      (:file       "serialization"
			       :depends-on ("package")))))

  :in-order-to  ((test-op (test-op :cl-rosetta-test))))

(defsystem :cl-rosetta-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.1.0"
  :license     "GP3L; see COPYING file for details."
  :description "Unit tests for the cl-rosetta system."
  :depends-on  (:cl-rosetta
		:lift)
  :components  ((:file         "package"
		 :pathname     "test/package"))
  :in-order-to ((test-op (load-op :cl-rosetta-test))))

(defmethod perform ((op test-op) (system (eql (find-system :cl-rosetta-test))))
  (funcall (find-symbol "RUN-TESTS" :lift) :config :generic))
