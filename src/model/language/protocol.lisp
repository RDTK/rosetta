;;; protocol.lisp --- Protocol for the model.language module.
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

(cl:in-package :rosetta.model.language)


;;; Languages
;;

(intern "LANGUAGE") ;; for (documentation :LANGUAGE 'rosetta.model:language)

(dynamic-classes:define-findable-class-family language
    "This family consists of language classes. Each language class is
used to control the output language when emitting things based on an
abstract description in form of model component instances.")

(defmethod documentation ((thing symbol) (type (eql 'language)))
  "Obtain documentation of type LANGUAGE from the language class
designated by THING."
  (documentation (find-language-class thing) t))
