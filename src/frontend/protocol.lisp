;;; protocol.lisp --- Protocol definitions of the cl-rosetta compiler frontend.
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

(cl:in-package :rosetta.frontend)


;;; Formats
;;

(intern "FORMAT") ;; for (documentation :FORMAT 'rosetta.frontend:format)

(dynamic-classes:define-findable-class-family format
    "This family consists of input format classes. Each input format
class is associated with input sources, encodings and syntax. Input
formats may be file-based, stream-based, buffer-based, may use textual
or binary encodings and may be expressed using different kind of
grammars. Furthermore, input formats may describe semantically
different aspects like data types and software system components."
  (:package *package*))

(defmethod documentation ((thing symbol) (type (eql 'format)))
  "Obtain documentation of type FORMAT from the target class
designated by THING."
  (documentation (find-format-class thing) t))


;;; Builder
;;

(intern "BUILDER")




;;; Parse protocol
;;

(defgeneric parse (format source builder
		   &key
		   dependency-handler)
  (:documentation
   "Parse content of SOURCE assuming it uses the format or syntax
described by FORMAT. Return an object that represents the parsed
content.

DEPENDENCY-HANDLER has to be a function of single argument that
accepts a source and loads the content of the resource designated by
the source. If this fails for some reason, the supplied function
should signal an error of a subtype of `dependency-error' such as
`cannot-resolve-dependency'."))


;;; Format class lookup
;;

(defmethod parse ((format list) (source t) (builder t)
		  &rest args &key &allow-other-keys)
  (let+ (((format-name &rest format-args) format)
	 (format-class    (find-format-class format-name))
	 (format-instance (apply #'make-instance
				 format-class format-args)))
    (apply #'parse format-instance source builder args)))

(defmethod parse ((format symbol) (source t) (builder t)
		  &rest args &key &allow-other-keys)
  (apply #'parse (list format) source builder args))
