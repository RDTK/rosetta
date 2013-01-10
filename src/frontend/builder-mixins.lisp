;;; builder-mixins.lisp --- Mixin classes for builder classes.
;;
;; Copyright (C) 2012, 2013 Jan Moringen
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
	    ;;; TODO(jmoringe, 2012-11-13): if ELEMENT already has a
	    ;;; location, attach a sequence of locations
	    (when (and element (not (location-of locations element)))
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
  (let ((*source*         (if (and *source* (not (stringp *source*)))
			      *source*
			      source))
	(*source-content* source))
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

(defmethod prettify ((builder comment-attaching-mixin)
		     (comment list))
  (let+ (((&flet starts-with-or-harmless (character)
	    #'(lambda (line)
		(or (emptyp line) (starts-with character line)))))
	 ((&flet trim-first (line)
	    (if (emptyp line)
		line
		(subseq line 1))))
	 ((&labels trim (lines)
	    "Strip common leading whitespace from LINES."
	    (cond
	      ((not lines) ; terminate recursion
	       nil)
	      ((or (every (starts-with-or-harmless #\Space) lines)
		   (every (starts-with-or-harmless #\Tab) lines))
	       (trim (mapcar #'trim-first lines)))
	      (t
	       (mapcar (curry #'string-right-trim '(#\Space #\Tab)) lines)))))
	 ;; Note that `comment?' returns a string representation of
	 ;; comment nodes.
	 (lines (trim (mapcar (curry #'comment? builder) comment))))
    (string-trim '(#\Newline) (format nil "~{~A~%~}" lines))))

(defmethod prettify ((builder comment-attaching-mixin)
		     (comment string))
  (string-trim '(#\Space #\Tab #\Newline) comment))

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
    ;; and prettify them and attach the resulting string to CHILD.
    (t
     (let ((*processing-comment?* t))
       (when-let ((comment (most-recent-comment builder parent)))
	 (setf (comment builder child)              (prettify builder comment)
	       (most-recent-comment builder parent) nil))
       (call-next-method)))))

(defmethod ensure-package :around ((builder comment-attaching-mixin)
				   &key &allow-other-keys)
  ;; Disable comment processing during `ensure-package' to prevent
  ;; intermediate parent packages from picking up comments intended
  ;; for the ultimately created package.
  (let* ((*processing-comment?* t)
	 (package               (call-next-method)))
    ;; Successively visit all parents of PACKAGE, the ensured package,
    ;; until one has a dangling comment. If such a comment exists,
    ;; attach it to PACKAGE.
    (iter (for parent initially (parent package) then (parent parent))
	  (while parent)
	  (when-let ((comment (most-recent-comment builder parent)))
	    (setf (comment builder package)            (prettify builder comment)
		  (most-recent-comment builder parent) nil)
	    (terminate)))
    package))


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
  (ensure-package instance :qname '(:absolute)))


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

(macrolet
    ((define-resolver-methods (find? make? kind)
       `(progn
	  ,@(when find?
	      `((defmethod find-node
		    ((builder lazy-resolver-mixin)
		     (kind    ,kind)
		     &rest args
		     &key
		     (qname                    (missing-required-argument :qname))
		     (if-does-not-exist        #'error)
		     (allow-forward-reference? t))
		  (check-type qname name-expression/absolute)

		  ;; If the object designated by KIND and QNAME cannot
		  ;; be found in REPOSITORY, return nil, if requested
		  ;; via IF-DOES-NOT-EXIST or put a
		  ;; `forward-reference' instance into REPOSITORY, if
		  ;; allowed via ALLOW-FORWARD-REFERENCE?.
		  (let+ (((&accessors-r/o repository) builder))
		    (or (query repository kind qname)
			(typecase if-does-not-exist
			  (null
			   nil)
			  (t
			   (when allow-forward-reference?
			     (setf (lookup repository kind qname)
				   (make-instance 'forward-reference
						  :kind kind
						  :args args))))))))))

	  ,@(when make?
	      `((defmethod make-node :around
		    ((builder lazy-resolver-mixin)
		     (kind    ,kind)
		     &key
		     (qname (missing-required-argument :qname))
		     &allow-other-keys)
		  (check-type qname name/absolute)

		  ;; Put the object created and returned by the next
		  ;; method into the repository. If REPOSITORY has an
		  ;; object for KIND and QNAME and the object is a
		  ;; `forward-reference' instance, an `upgrade'
		  ;; restart will be active. Invoke it to upgrade the
		  ;; forward reference to the constructed object.
		  (let+ (((&accessors-r/o repository) builder)
			 (node (call-next-method)))
		    (setf (lookup repository kind qname
				  :if-exists
				  (lambda (condition)
				    (declare (ignore condition))
				    (when-let ((restart (find-restart 'upgrade)))
				      (invoke-restart restart node))))
			  node))))))))

  (define-resolver-methods t   nil list)
  (define-resolver-methods t   t   (eql :enum))
  (define-resolver-methods t   t   (eql :structure))
  (define-resolver-methods t   t   (eql :package)))

(defmethod parse :after ((format  t)
			 (source  t)
			 (builder lazy-resolver-mixin)
			 &key &allow-other-keys)
  (when-let ((unresolved (rs.m.d::forward-references (repository builder))))
    (iter (for ((kind . qname) . object) in unresolved)
	  (restart-case
	      (error 'processing-error
		     :location (if (compute-applicable-methods #'locations (list builder))
				   (location-of (locations builder) object)
				   (make-instance 'location-info))
		     :builder  builder
		     :cause    (make-instance
				'simple-error
				:format-control "~@<Unresolved forward ~
reference to the ~A named \"~/rosetta.model::print-qname/\".~@:>"
				:format-arguments (list kind qname)))
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
  (check-type pathname (or (cons (eql or)) pathname))

  (let+ (((&values format location)
          (resolve (resolver builder) format pathname)))
    (process format location builder)))


;;; `source-level-caching-mixin' mixin class
;;

(defclass source-level-caching-mixin ()
  ((cache :type     hash-table
	  :initform (make-hash-table :test #'equal)
	  :reader   %cache
	  :documentation
	  "Stores a mapping from previously processed sources two the
respectively produced results."))
  (:documentation
   "This class is intended to be mixed into builder classes which
cache parsing results."))

(defmethod process :around ((format  t)
			    (source  t)
			    (builder source-level-caching-mixin)
			    &key &allow-other-keys)
  ;; Construct a key based on FORMAT and a normalized version of
  ;; SOURCE and retrieve the parsing result from the cache or delegate
  ;; to the next method to perform a parse.
  (let ((key (typecase source
	       (pathname (unless (wild-pathname-p source)
			   (cons format (truename source))))
	       (stream   nil)
	       (t        (cons format source)))))
    (if key
	(values (ensure-gethash key (%cache builder) (call-next-method)))
	(call-next-method))))


;;; `name-normalizing-mixin' mixin class
;;

(defclass name-normalizing-mixin ()
  ((normalizer :initarg  :normalizer
	       :type     function
	       :reader   normalizer
	       :documentation
	       "Stores a function which is called to normalize
names. The function has to accept the unnormalized names as its sole
argument and return the normalized name."))
  (:default-initargs
   :normalizer (missing-required-initarg 'name-normalizing-mixin :normalizer))
  (:documentation
   "This mixin class adds to builder classes on-the-fly normalization
of names in created/searched nodes.

The actual normalization is performed by a function supplied when
constructing the builder instance."))

(macrolet
    ((define-name-normalizing-method (name)
       `(defmethod ,name :around ((builder name-normalizing-mixin)
				  (kind    t)
				  &rest args
				  &key
				  (name  nil name-supplied?)
				  (qname nil qname-supplied?))
	  (let+ (((&accessors-r/o normalizer) builder)
		 ((&labels normalize-qname (name)
		    (etypecase name
		      (name
		       (cons (first name) (mapcar normalizer (rest name))))
		      (name-expression
		       (cons (first name)
			     (mapcar #'normalize-qname (rest name))))))))
	    (apply #'call-next-method builder kind
		   (append
		    (when name-supplied?
		      (check-type name string)
		      (list :name (funcall normalizer name)))
		    (when qname-supplied?
		      (check-type qname name-expression)
		      (list :qname (normalize-qname qname)))
		    (remove-from-plist args :name :qname)))))))

  (define-name-normalizing-method find-node)
  (define-name-normalizing-method make-node))
