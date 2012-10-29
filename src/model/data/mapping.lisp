;;; mapping.lisp --- Associate wire-schema to given data-holder.
;;
;; Copyright (C) 2012 Jan Moringen
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

(cl:in-package :rosetta.model.data)

(defclass mapping (print-items-mixin)
  ((data-holder :initarg  :data-holder
		:reader   data-holder
		:documentation
		"Stores the structure type which describes the
data-holder part of the mapping.")
   (wire-schema :initarg  :wire-schema
		:reader   wire-schema
		:documentation
		"Stores the structure type which describes the
wire-schema part of the mapping.")
   (rules       :initarg  :rules
		:type     list
		:accessor rules
		:initform nil
		:documentation
		"Stores the rules which associate field of the
data-holder and wire-schema structure types."))
  (:default-initargs
   :data-holder (missing-required-initarg 'mapping     :data-holder)
   :wire-schema (missing-required-initarg 'wire-schema :wire-schema))
  (:documentation
   "instances of this class associate an arbitrary wire-schema (which
is described as a structure type) to a data-holder (which is described
as a structure type as well) by means of a set of rules.

Rules associates the values fields of the data-holder and wire-schema
via simple expressions."))

(defmethod name ((type mapping))
  (name (data-holder type)))

(defmethod qname ((type mapping))
  (qname (data-holder type)))

(defmethod print-items append ((object mapping))
  (let+ (((&accessors-r/o data-holder wire-schema rules) object))
    (list (list :data-holder (name data-holder))
	  (list :wire-schema (name wire-schema)  " -> ~A")
	  (list :num-items   (length rules)      " (~D)"))))
