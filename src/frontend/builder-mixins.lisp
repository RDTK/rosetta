;;; builder-mixins.lisp --- Mixin classes for builder classes.
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

(cl:in-package :rosetta.frontend)


;;; Class `location-attaching-mixin'
;;

(declaim (special *source* *source-content*))

(defvar *source* nil
  "Dynamically bound to the source (e.g. stream, pathname) being
processed around `parse' calls.")

(defvar *source-content* nil
  "Dynamically bound to the contents of the source being
processed (e.g. contents of a source file) around `parse' calls.")

(defclass location-attaching-mixin ()
  ((locations :initarg  :locations
	      :accessor locations
	      :documentation
	      "Stores a location repository, for example an instance
of the `location-repository' class."))
  (:default-initargs
   :locations (missing-required-initarg
	       'location-attaching-mixin :locations))
  (:documentation
   "This mixin adds to builder classes the ability to associate source
location information to elements."))

(macrolet
    ((define-method (name)
       `(defmethod ,name :around ((builder location-attaching-mixin)
				  (kind    t)
				  &key
				  bounds)
		   (let+ (((&accessors-r/o locations) builder)
			  (element (call-next-method)))
		     (when element
		       (setf (location-of locations element)
			     (make-instance 'location-info
					    :source         *source*
					    :source-content *source-content*
					    :bounds         bounds)))
		     element))))
  (define-method find-node)
  (define-method make-node))

(defmethod parse :around ((format  t)
			  (source  pathname)
			  (builder location-attaching-mixin)
			  &key &allow-other-keys)
  (let ((*source*         source)
	(*source-content* (ignore-errors
			    (read-file-into-string source))))
    (call-next-method)))

(defmethod parse :around ((format  t)
			  (source  string)
			  (builder location-attaching-mixin)
			  &key &allow-other-keys)
  (let ((*source-content* source))
    (call-next-method)))


;;; `comment-attaching-mixin' mixin class
;;

(declaim (special *processing-comment?*))

(defvar *processing-comment?* nil
  "Dynamically bound to a boolean indicating whether a comment is
being processed.")

(defclass comment-attaching-mixin ()
  ((most-recent-comments :initarg  :most-recent-comments
			 :type     hash-table
			 :accessor %most-recent-comments
			 :initform (make-hash-table :test #'eq)
			 :documentation
			 "Associates most recently parsed comment
elements to their respective parent elements for later association to
the appropriate child elements.")
   (assoc                :initarg  :assoc
			 :type     hash-table
			 :accessor %assoc
			 :initform (make-hash-table :test #'eq)
			 :documentation
			 "Associates comment elements to the elements
to which the comments refer."))
  (:documentation
   "This mixin adds to builder classes the ability to associate
comment elements to the elements to which they refer."))

(defmethod most-recent-comment ((builder comment-attaching-mixin)
				(for     t))
  (values (gethash for (%most-recent-comments builder))))

(defmethod (setf most-recent-comment) ((new-value t)
				       (builder   comment-attaching-mixin)
				       (for       t))
  (appendf (gethash for (%most-recent-comments builder)) (list new-value)))

(defmethod (setf most-recent-comment) ((new-value (eql nil))
				       (builder   comment-attaching-mixin)
				       (for       t))
  (remhash for (%most-recent-comments builder)))

(defmethod comment ((builder comment-attaching-mixin)
		    (for     t))
  (values (gethash for (%assoc builder))))

(defmethod (setf comment) ((new-value t)
			   (builder   comment-attaching-mixin)
			   (for       t))
  (setf (gethash for (%assoc builder)) new-value))

(defmethod comment? ((builder   comment-attaching-mixin)
		     (thing     t))
  nil)

(defmethod comment? ((builder   comment-attaching-mixin)
		     (thing     string))
  thing)

(defmethod add-child :around ((builder comment-attaching-mixin)
			      (parent  t)
			      (child   t))
  (cond
    ;; When processing a comment, just call the next method.
    (*processing-comment?*
     (call-next-method))

    ;; When CHILD is a comment, store it for association with the
    ;; following element.
    ((comment? builder child)
     (setf (most-recent-comment builder parent) child)
     parent)

    ;; If CHILD is not a comment and comments are queued, concatenate
    ;; them and attach the resulting string to CHILD.
    (t
     (let ((*processing-comment?* t))
       (when-let ((comment (most-recent-comment builder parent)))
	 ;; Note that `comment?' returns a string representation of
	 ;; comment nodes.
	 (setf (comment builder child)
	       (string-trim
		'(#\Space #\Tab #\Newline)
		(format nil "~{~A~%~}"
			(mapcar (curry #'comment? builder) comment)))
	       (most-recent-comment builder parent)
	       nil))
       (call-next-method)))))


;;; `root-package-creating-mixin' mixin class
;;

(defclass root-package-creating-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into builder classes which rely
on a root package being present in the type repository."))

(defmethod shared-initialize :after ((instance   root-package-creating-mixin)
                                     (slot-names t)
                                     &key)
  ;; Create the root package unless there is one.
  (unless (find-node instance :package
		     :name  ""
		     :qname '(:absolute)
		     :if-does-not-exist nil)
    (make-node instance :package
	       :name  ""
	       :qname '(:absolute))))


;;; `lazy-resolver-mixin' mixin class
;;

(defclass lazy-resolver-mixin ()
  ((repository :initarg  :repository
	       :reader   repository
	       :documentation
	       "Stores forward references encountered during the
building process."))
  (:default-initargs
   :repository (missing-required-initarg
		'lazy-resolver-mixin :repository))
  (:documentation
   "This mixin adds to builder classes the ability to treat initially
unresolved references as forward references and resolve them later."))

;;; TODO(jmoringe, 2012-05-03): do not repeat these methods
;;; TODO(jmoringe, 2012-05-03): can we use toplevel-mixin?
(macrolet
    ((define-resolver-methods (kind)
       `(progn
	  (defmethod find-node ((builder lazy-resolver-mixin)
				(kind    ,kind)
				&rest args
				&key
				(qname                    (missing-required-argument :qname))
				(if-does-not-exist        #'error)
				(allow-forward-reference? t))
	    (check-type qname (or name/absolute (cons (eql or)))) ;;; TODO(jmoringe, 2012-05-03):  proper type

	    (let+ (((&accessors-r/o repository) builder))
	      (or (query repository kind qname)
		  (typecase if-does-not-exist
		    (null
		     nil)
		    (t
		     (when allow-forward-reference?
		       (setf (lookup repository kind qname)
			     (make-instance 'rs.m.d::forward-reference
					    :kind kind
					    :args args))))))))

	  #+maybe?
	  (defmethod add-child :around ((builder lazy-resolver-mixin)
					(parent  t)
					(child   t))
	    (setf (lookup (repository builder) kind qname
			  :if-exists
			  (lambda (condition)
			    (declare (ignore condition))
			    (when-let ((restart (find-restart 'upgrade)))
			      (invoke-restart restart child))))
		  child)
	    (call-next-method))

	  (defmethod make-node :around ((builder lazy-resolver-mixin)
					(kind    ,kind)
					&key
					(qname (missing-required-argument :qname))
					&allow-other-keys)
	    (check-type qname name/absolute)

	    (let+ (((&accessors-r/o repository) builder)
		   (node (call-next-method)))
	      (setf (lookup repository kind qname
			    :if-exists
			    (lambda (condition)
			      (declare (ignore condition))
			      (when-let ((restart (find-restart 'upgrade)))
				(invoke-restart restart node))))
		    node))))))

  (define-resolver-methods list) ;;; TODO(jmoringe, 2012-05-03): only find-node
  (define-resolver-methods (eql :enum))
  (define-resolver-methods (eql :structure))
  (define-resolver-methods (eql :package)))

(defmethod parse :after ((format  t)
			 (source  t)
			 (builder lazy-resolver-mixin)
			 &key &allow-other-keys)
  (when-let ((unresolved (rs.m.d::forward-references (repository builder))))
    (iter (for (name . object) in unresolved)
	  (restart-case
	      (error 'processing-error
		     :location (if (compute-applicable-methods #'locations (list builder))
				   (location-of (locations builder) object)
				   (make-instance 'location-info))
		     :builder  builder
		     :cause    (make-instance
				'simple-error
				:format-control   "~@<Unresolved forward reference to ~{~(~A~) \"~*~@{~A~^.~}\"~}.~@:>"
				:format-arguments (list name)))
	    (continue ()
	      :report (lambda (stream)
			(format stream "~@<Ignore the unresolved ~
reference and continue.~@:>")))))))


;;; `dependency-delegating-mixin' mixin class
;;

(defclass dependency-delegating-mixin ()
  ((resolver :initarg  :resolver
	     :accessor resolver
	     :documentation
	     "Stores an object implementing the resolver protocol
which is consulted when dependencies have to be resolved."))
  (:default-initargs
   :resolver (missing-required-initarg
	      'dependency-delegating-mixin :resolver))
  (:documentation
   "This class is intended to be mixed into builder classes which have
to resolve dependencies."))

(defmethod make-node ((builder dependency-delegating-mixin)
		      (kind    (eql :dependency/file))
		      &key
		      (pathname (required-argument :pathname))
		      format)
  (check-type pathname pathname)

  (let+ (((&values format location)
          (resolve (resolver builder) format pathname)))
    (parse format location builder)))
