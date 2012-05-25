;;; package.lisp --- Package definition for model.serialization module.
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

(cl:defpackage :rosetta.model.serialization
  (:nicknames
   :rs.m.s)

  (:use
   :cl
   :alexandria
   :more-conditions

   :rosetta.model.data)

  (:shadow
   :name)

  ;; Mechanism protocol
  (:export
   :name
   :wire-type
   :validate-type)

  ;; Mechanism class family
  (:export
   :no-such-mechanism-class
   :find-mechanism-class
   :mechanism-classes)

  ;; Symbol for mechanism documentation
  (:export
   :mechanism)

  (:documentation
   "This package contains model elements which represent
serializations.

Each modeled serialization mechanism specializes the generic functions:

* `name'                            [generic function]
* `wire-type'                       [generic function]
* `validate-type'                   [generic function]

There is a family of serialization mechanisms which can be manipulated
using:

* `no-such-mechanism-class'         [condition]
* `find-mechanism-class'            [method]
* `mechanisms-classes'              [function]

See

  (documentation SYMBOL 'rs.m.s:mechanism)"))
