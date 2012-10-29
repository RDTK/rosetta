;;; package.lisp --- Package definition for model.serialization module.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the GNU Lesser General
;; Public License Version 3 (the ``LGPL''), or (at your option) any
;; later version.
;;
;; Software distributed under the License is distributed on an ``AS
;; IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the LGPL for the specific language governing rights
;; and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html or
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

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
   :offset-type
   :length-type

   :validate-type)

  ;; Mechanism class family
  (:export
   :no-such-mechanism-class
   :find-mechanism-class
   :mechanism-classes)

  ;; Symbol for mechanism documentation
  (:export
   :mechanism)

  ;; Mixins
  (:export
   :wire-type-mixin
   :offset-type-mixin
   :length-type-mixin)

  (:documentation
   "This package contains model elements which represent
serializations.

Each modeled serialization mechanism specializes the generic
functions:

* `name'                            [generic function]

* `wire-type'                       [generic function]
* `offset-type'                     [generic function]
* `length-type'                     [generic function]

* `validate-type'                   [generic function]

There is a family of serialization mechanisms which can be manipulated
using:

* `no-such-mechanism-class'         [condition]
* `find-mechanism-class'            [generic function]
* `mechanism-classes'               [function]

See

  (documentation SYMBOL 'rs.m.s:mechanism)"))
