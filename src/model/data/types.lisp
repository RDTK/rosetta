;;; types.lisp --- Types used in the model.data module.
;;
;; Copyright (C) 2012 Jan Moringen
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

(cl:in-package :rosetta.model.data)


;;; Naming-related types
;;

(deftype name-component ()
  'string)

(deftype name-components ()
  '(and list (satisfies %every-name-component)))

(deftype name/absolute ()
  '(cons (eql :absolute) (or null name-components)))

(deftype name/relative ()
  '(cons (eql :relative) name-components))

(deftype name ()
  '(or name/absolute name/relative))


;;; Utility functions
;;

(defun %every-name-component (list)
  "TODO(jmoringe): document"
  (every (of-type 'name-component) list))
