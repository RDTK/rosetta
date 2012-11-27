;;; types.lisp --- Types used in the model module.
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

(cl:in-package :rosetta.model)


;;; Naming-related types
;;

(deftype name-component ()
  'string)

(deftype name-components ()
  '(and list (satisfies %every-name-component)))

(deftype name/absolute ()
  '(cons (eql :absolute) (or null name-components)))

(deftype name-expression/absolute ()
  "A `name/absolute' or a disjunction (with `cl:or') of multiple
`name/absolute's."
  '(or name/absolute
       (cons (eql or) (cons name/absolute))))

(deftype name/relative ()
  '(cons (eql :relative) name-components))

(deftype name-expression/relative ()
  "A `name/relative' or a disjunction (with `cl:or') of multiple
`name/relative's."
  '(or name/relative
       (cons (eql or) (cons name/relative))))

(deftype name ()
  '(or name/absolute name/relative))

(deftype name-expression ()
  "A `name' or a disjunction (with `cl:or') of multiple `name's."
  '(or name (cons (eql or) (cons name))))


;;; Utility functions
;;

(defun %every-name-component (list)
  (every (of-type 'name-component) list))
