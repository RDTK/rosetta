;;; resolvers.lisp --- Dependency resolvers for use with builders.
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

(defclass search-path-resolver (print-items-mixin)
  ((search-path  :initarg  :search-path
		 :type     list
		 :accessor search-path
		 :initform nil
		 :documentation
		 "Stores a list of pathnames which are consulted when
trying to resolve a pathname dependency.")
   (if-ambiguous :initarg  :if-ambiguous
		 :type     (or (eql :first) symbol function)
		 :accessor if-ambiguous
		 :initform #'error
		 :documentation
		 "Stores a policy which is applied when the resolver
encounters an ambiguous dependency."))
  (:documentation
   "Instances of this resolver class try to resolve pathname
dependencies by using a list of pathnames in which dependencies should
be searched for."))

(defmethod resolve ((resolver search-path-resolver)
		    (format   t)
		    (location pathname)
		    &key
		    if-does-not-exist)
  (declare (ignore if-does-not-exist))

  (let+ (((&accessors-r/o search-path if-ambiguous) resolver)
	 ;; Produce initial candidate set from specified relative or
	 ;; absolute pathname.
	 (locations (ecase (first (pathname-directory location))
		      (:absolute
		       (list location))
		      ((nil :relative)
		       (mapcar (curry #'merge-pathnames location)
			       search-path))))
	 ;; Restrict locations to existing files and select location
	 ;; from resulting candidate set.
	 (location/resolved
	  (let ((candidates (remove-if-not #'probe-file locations)))
	    (cond
	      ;; No candidate => signal an error.
	      ((emptyp candidates)
	       nil)

	      ;; Exactly one candidate => just use it.
	      ((length= 1 candidates)
	       (first candidates))

	      ;; Multiple candidates => apply resolution strategy or
	      ;; signal an error with restarts established.
	      (t
	       (etypecase if-ambiguous
		 ((eql :first)
		  (first candidates))
		 ((or symbol function)
		  (restart-case
		      (funcall if-ambiguous
			       (make-condition 'ambiguous-dependency
					       :dependency location
					       :candidates candidates))
		    (use-value (value)
		      value)
		    (continue (&optional condition)
		      :report (lambda (stream)
				(format stream "~@<Use the first candidate.~@:>"))
		      (declare (ignore condition))
		      (first candidates))))))))))

    ;; LOCATION/RESOLVED is the *truename* of the resolved
    ;; dependency. The builder can use it to detect already processed
    ;; locations.
    ;;
    ;; If necessary, try to guess format from resolved location.
    (if location/resolved
	(values (or format (guess-format location/resolved))
		location/resolved)
	(values nil nil locations))))

(defmethod print-items append ((object search-path-resolver))
  `((:search-path ,(length (search-path object)) "(~D)")))
