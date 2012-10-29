;;; repository.lisp ---
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


;;;
;;

(defclass forward-reference-upgrader-mixin ()
  ((forward-references :initarg  :forward-references
		       :type     list
		       :accessor forward-references
		       :initform nil
		       :documentation
		       ""))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod lookup ((repository forward-reference-upgrader-mixin)
		   (kind       symbol)
		   (name       list)
		   &key &allow-other-keys)
  (let+ (((&labels+ matches? ((entry-kind &rest entry-name))
	    (cond
	      ((not (member kind (ensure-list entry-kind)))
	       nil)

	      ((eq (first entry-name) 'or)
	       (some (compose #'matches? (curry #'cons entry-kind))
		     (rest entry-name)))

	      (t
	       (equal entry-name name))))))

    (cdr (find-if #'matches? (forward-references repository)
		  :key #'car))))

(defmethod (setf lookup) ((new-value  forward-reference)
			  (repository forward-reference-upgrader-mixin)
			  (kind       t)
			  (name       list)
			  &key &allow-other-keys)
  (check-type name (or name/absolute (cons (eql or)))) ;;; TODO(jmoringe, 2012-05-03): proper type

  (push (cons (cons kind name) new-value)
	(forward-references repository))
  new-value)

(defmethod (setf lookup) :around ((new-value  t)
				  (repository forward-reference-upgrader-mixin)
				  (kind       symbol)
				  (name       list)
				  &key &allow-other-keys)
  (check-type name name/absolute)

  (if (typep new-value 'forward-reference)
      (call-next-method)
      (let ((entry (lookup repository kind name
			   :if-does-not-exist nil)))
	(if (typep entry 'forward-reference)
	    (restart-case
		(call-next-method)
	      (upgrade (&optional (value new-value))
		:test (lambda (condition)
			(typep condition 'duplicate-child-key)
			t) ;;; TODO(jmoringe, 2012-04-24):
		(removef (forward-references repository) entry
			 :key #'cdr :test #'eq :count 1)
		(setf (lookup repository kind name) (upgrade! entry value))))
	    (call-next-method)))))


;;;
;;

(defclass base-repository (container/absolute-mixin
			   forward-reference-upgrader-mixin
			   print-items-mixin)
  ()
  (:documentation
   "TODO(jmoringe): document"))

(defmethod contents/plist ((repository base-repository))
  (hash-table-plist (%nested repository)))
