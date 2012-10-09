;;; text-format-mixin.lisp --- Mixin class for format classes for text formats.
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

(defclass text-format-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into format classes that
operate on textual input data."))

(defmethod parse ((format  text-format-mixin)
		  (source  pathname)
		  (builder t)
		  &rest args &key &allow-other-keys)
  "Open a character input stream for the file designated by SOURCE and
call a method specialized on streams."
  (handler-bind ((location-condition
		  #'(lambda (condition)
		      (let+ (((&accessors-r/o location) condition)
			     ((&accessors (source1 source) source-content) location))
			(unless (pathnamep source1)
			  (setf source1 source))
			(unless source-content
			  (setf source-content
				(read-file-into-string source)))))))
    (with-input-from-file (stream source)
      (apply #'parse format stream builder args))))

(defmethod parse ((format  text-format-mixin)
		  (source  string)
		  (builder t)
		  &rest args &key &allow-other-keys)
  "Create an input stream for the content of SOURCE and call a method
specialized on streams."
  (handler-bind ((location-condition
		  #'(lambda (condition)
		      (let+ (((&accessors-r/o location) condition)
			     ((&accessors source-content) location))
			(unless source-content
			  (setf source-content source))))))
    (with-input-from-string (stream source)
      (apply #'parse format stream builder args))))
