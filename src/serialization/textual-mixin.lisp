;;; textual-mixin.lisp --- Mixin class for textual mechanism classes.
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

(in-package :rosetta.serialization)

(defclass textual-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into mechanism classes that
represent textual serialization mechanisms."))

(defmethod pack ((mechanism   textual-mixin)
		 (source      t)
		 (destination (eql nil))
		 &rest args
		 &key)
  "Map nil DESTINATION to 'string."
  (apply #'pack mechanism source 'string args))

(defmethod pack ((mechanism   textual-mixin)
		 (source      t)
		 (destination stream)
		 &rest args
		 &key)
  "Write packing result to stream DESTINATION."
  (bind (((:values size result)
	  (apply #'pack mechanism source 'string args)))
    (write-string result destination)
    (values size destination)))

(defmethod pack ((mechanism   textual-mixin)
		 (source      t)
		 (destination pathname)
		 &rest args
		 &key)
  "Write packing result to file designated by pathname DESTINATION."
  (with-output-to-file (stream destination :if-exists :supersede)
    (values (apply #'pack mechanism source stream args) destination)))
