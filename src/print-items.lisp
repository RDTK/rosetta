;;; print-items.lisp --- Composable printing mechanism
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

(in-package :rosetta)


;;; Print Items Protocol
;;

(defgeneric print-items (object)
  (:method-combination append)
  (:documentation
   "Return a list of items that should appear in the printed
representation of OBJECT."))

(defmethod print-items append ((object t))
  "Default behavior is to not return any print items for OBJECT."
  nil)


;;; Print Items Mixin
;;

(defclass print-items-mixin ()
  ()
  (:documentation
   "This mixin class adds printing via `print-items' to classes."))

(defmethod print-object ((object print-items-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format-print-items (print-items object) stream)))


;;; Utility Functions
;;

(defun format-print-items (items stream)
  "Print ITEMS onto STREAM.
ITEMS is a list of items of the form ITEM where
ITEM   ::= (KEY VALUE [FORMAT])
KEY    ::= any Lisp object
VALUE  ::= any Lisp object
FORMAT ::= a format string (Default is \"~A\")"
  (iter (for (key value . rest) in (remove-duplicates items
						      :key #'first
						      :from-end t))
	(let ((format (first rest)))
	  (format stream " ~A " key)
	  (format stream (or format "~A") value))))
