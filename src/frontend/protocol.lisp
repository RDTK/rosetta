;;; protocol.lisp ---
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

(in-package :rosetta.frontend)


;;; Formats
;;

(intern "FORMAT") ;; for (documentation :FORMAT 'rosetta.frontend:target)

(dynamic-classes:define-findable-class-family format
    ""
  (:package *package*))

(defmethod documentation ((thing symbol) (type (eql 'format)))
  "Obtain documentation of type TARGET from the target class
designated by THING."
  (documentation (find-format-class thing) t))


;;; Parse protocol
;;

(defgeneric parse (format source
		   &key)
  (:documentation
   "Parse content of SOURCE
Return a `pb::file-set-desc' instance that contains the complete
description in its child nodes.

"))


;;; Format class lookup
;;

(defmethod parse ((format list) (source t)
		  &key)
  (bind (((format-name &rest format-args) format)
	 (format-class    (find-format-class format-name))
	 (format-instance (apply #'make-instance
				 format-class format-args)))
    (parse format-instance source)))

(defmethod parse ((format symbol) (source t)
		  &key)
  (parse (list format) source))
