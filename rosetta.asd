;;; rosetta.asd --- System definition for the rosetta system.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
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

(cl:defpackage :rosetta-system
  (:use
   :cl
   :asdf)

  (:export
   :version/list
   :version/string))

(cl:in-package :rosetta-system)


;;; Version stuff
;;

(defparameter +version-major+ 0
  "Major component of version number.")

(defparameter +version-minor+ 2
  "Minor component of version number.")

(let* ((version-file (merge-pathnames "version.sexp" *load-truename*))
       stream)
  (when (probe-file version-file)
    (setf stream (open version-file)))

  (defparameter +version-revision+ (if stream (read stream) 0)
    "Revision component of version number.")

  (defparameter +version-commit+ (when stream (read stream))
    "Commit component of version number.")

  (when stream (close stream)))

(defun version/list (&key
		     (revision? t)
		     commit?)
  "Return a version of the form (MAJOR MINOR [REVISION [COMMIT]])
where REVISION and COMMIT are optional.

REVISION? controls whether REVISION should be included. Default
behavior is to include REVISION.

COMMIT? controls whether COMMIT should be included. Default behavior
is to not include COMMIT."
  (append (list +version-major+ +version-minor+)
	  (when revision? (list +version-revision+))
	  (when (and commit? +version-commit+)
	    (list +version-commit+))))

(defun version/string (&rest args
		       &key
		       revision?
		       commit?)
  "Return a version string of the form
\"MAJOR.MINOR[.REVISION[-.COMMIT]]\" where REVISION and COMMIT are
optional.

See `version/list' for details on keyword parameters."
  (declare (ignore revision? commit?))
  (format nil "~{~A.~A~^.~A~^-~A~}" (apply #'version/list args)))


;;; System definitions
;;

(defsystem :rosetta
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3; see COPYING file for details."
  :description "Cross-compiler for robotic systems components and frameworks."
  :depends-on  (:alexandria
		:split-sequence
		:let-plus
		:more-conditions
		:cl-dynamic-classes
		:flexi-streams

		:nibbles)
  :components  ((:module     "src"
		 :serial     t
		 :components ((:file       "package")
			      (:file       "print-items")))

		(:module     "model-data-early"
		 :pathname   "src/model/data"
		 :depends-on ("src")
		 :serial     t
		 :components ((:file       "package")
			      (:file       "types")
			      (:file       "conditions")
			      (:file       "protocol")))

		(:module     "model-data"
		 :pathname   "src/model/data"
		 :depends-on ("src" "model-data-early")
		 :serial     t
		 :components ((:file       "mixins")

			      (:file       "forward-reference")
			      (:file       "repository")
			      (:file       "package1")

			      (:file       "type-singleton")
			      (:file       "type-fundamental")
			      (:file       "type-enum")
			      (:file       "type-structure")
			      (:file       "type-array")

			      (:file       "mapping")))

		(:module     "model-serialization"
		 :pathname   "src/model/serialization"
		 :depends-on ("src" "model-data")
		 :serial     t
		 :components ((:file       "package")
			      (:file       "protocol")))

		(:module     "model-language"
		 :pathname   "src/model/language"
		 :depends-on ("src")
		 :components ((:file       "package")
			      (:file       "protocol"
			       :depends-on ("package"))
			      (:file       "abstract"
			       :depends-on ("package" "protocol"))
			      (:file       "lisp"
			       :depends-on ("package" "protocol"))
			      (:file       "c++"
			       :depends-on ("package" "protocol"))))

		(:module     "serialization"
		 :pathname   "src/serialization"
		 :depends-on ("src" "model-serialization")
		 :components ((:file       "package")
			      (:file       "conditions"
			       :depends-on ("package"))
			      (:file       "protocol"
			       :depends-on ("package"))

			      (:file       "textual-mixin"
			       :depends-on ("package" "protocol"))
			      (:file       "textual-stream-mixin"
			       :depends-on ("package" "protocol"
					    "textual-mixin"))
			      (:file       "binary-mixin"
			       :depends-on ("package" "protocol"))
			      (:file       "data-holder-mixin"
			       :depends-on ("package" "protocol"))))

		(:module     "frontend"
		 :pathname   "src/frontend"
		 :depends-on ("src" "model-data")
		 :components ((:file       "package")
			      (:file       "types"
			       :depends-on ("package"))
			      (:file       "protocol"
			       :depends-on ("package" "types"))
			      (:file       "locations"
			       :depends-on ("package" "types"
					    "protocol"))
			      (:file       "conditions"
			       :depends-on ("package" "types"
					    "locations"))
			      (:file       "util"
			       :depends-on ("package"))

			      (:file       "builder-mixins"
			       :depends-on ("package" "protocol"))
			      (:file       "text-format-mixin"
			       :depends-on ("package" "protocol"))
			      (:file       "binary-format-mixin"
			       :depends-on ("package" "protocol"))

			      (:file       "builder"
			       :depends-on ("package" "protocol"
					    "util" "builder-mixins"))))

		(:module     "backend-early"
		 :pathname   "src/backend"
		 :depends-on ("model-data" "model-language" "model-serialization"
			      "serialization")
		 :serial     t
		 :components ((:file       "package")
			      (:file       "variables")
			      (:file       "macros")
			      (:file       "protocol")))

		(:module     "backend"
		 :pathname   "src/backend"
		 :depends-on ("backend-early")
		 :serial     t
		 :components ((:file       "target-mixins")
			      (:file       "targets")

			      (:file       "emitter-serializer")

			      (:file       "emitter-lisp")
			      (:file       "emitter-lisp-serializer"))))

  :in-order-to  ((test-op (test-op :rosetta-test))))

(defsystem :rosetta-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3; see COPYING file for details."
  :description "Unit tests for the rosetta system."
  :depends-on  ((:version :rosetta #.(version/string))

		(:version :lift    "1.7.1"))
  :components  ((:file         "package"
		 :pathname     "test/package")

		(:module     "serialization"
		 :pathname   "test/serialization"
		 :depends-on ("package")
		 :components ((:file       "package")
			      (:file       "protocol"
			       :depends-on ("package"))
			      (:file       "textual-mixin"
			       :depends-on ("package"))
			      (:file       "textual-stream-mixin"
			       :depends-on ("package" "textual-mixin"))
			      (:file       "binary-mixin"
			       :depends-on ("package"))
			      (:file       "data-holder-mixin"
			       :depends-on ("package"))))

		(:module     "model"
		 :pathname   "test/model"
		 :depends-on ("package")
		 :components ((:file       "package")))

		(:module     "model-data"
		 :pathname   "test/model/data"
		 :depends-on ("package" "model")
		 :components ((:file       "package")
			      (:file       "named-mixin"
			       :depends-on ("package"))
			      (:file       "structure-mixin"
			       :depends-on ("package"))))))

(defmethod perform ((op test-op) (system (eql (find-system :rosetta-test))))
  (funcall (find-symbol "RUN-TESTS" :lift) :config :generic))
