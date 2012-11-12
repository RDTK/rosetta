;;; protocol.lisp --- Protocol definitions of the rosetta compiler frontend.
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

(cl:in-package :rosetta.frontend)


;;; Source and location protocol
;;

(defgeneric source (thing)
  (:documentation
   "Return the source associated to THING."))

(defgeneric source-content (thing)
  (:documentation
   "Return the content as string of the source associated to THING.
Return nil if the content is not available."))

(defgeneric bounds (thing)
  (:documentation
   "Return the bounds of the region of the source string associated to
THING as a cons cell

  (START . END)

. Return nil if the information is not available."))

(defgeneric line (thing &key of)
  (:documentation
   "Return the line within the source string associated to
THING. Return nil if the information is not available. Line numbers
start at 0.

OF can be :START or :END and controls whether the line of the
beginning or end of the region associated to THING is used."))

(defgeneric column (thing &key of)
  (:documentation
   "Return the column within the source string associated to
THING. Return nil if the information is not available. Column numbers
start at 0.

OF can be :START or :END and controls whether the line of the
beginning or end of the region associated to THING is used."))

(defgeneric location= (left right
		       &key
		       compare-source?
		       compare-source-content?
		       compare-bounds?)
  (:documentation
   "Compare locations LEFT and RIGHT for equality and return non-nil
if they can be considered equal under the requested comparison.

COMPARE-SOURCE? controls whether sources should be compared.

COMPARE-SOURCE-CONTENT? controls whether the contents of sources
should be compared.

COMPARE-BOUNDS? controls whether bounds should be compared."))


;;; Location repository protocol
;;

(defgeneric location-of (repository for)
  (:documentation
   "Return a `location-info' instance for FOR in REPOSITORY or nil if
there is no such information."))

(defgeneric (setf location-of) (new-value repository for)
  (:documentation
   "Set the location information for FOR within REPOSITORY to
NEW-VALUE."))


;;; Parse protocol
;;

(defgeneric parse (format source builder
		   &key &allow-other-keys)
  (:documentation
   "Parse content of SOURCE assuming it uses the format or syntax
described by FORMAT. Return an object that represents the parsed
content."))

;; error handling

(define-condition-translating-method
    parse ((format standard-object) (source t) (builder t)
				    &key &allow-other-keys)
  (((and error (not processing-error)) parse-error1)
   :location (make-instance 'location-info
			    :source source)
   :builder  builder)
  (((and warning (not processing-warning)) parse-warning
					   :signal-via warn)
   :location (make-instance 'location-info
			    :source source)
   :builder  builder))

;; Format class lookup

(defmethod parse ((format list) (source t) (builder t)
		  &rest args &key &allow-other-keys)
  (let+ (((format-name &rest format-args) format)
	 (format-class    (find-format-class format-name))
	 (format-instance (apply #'make-instance
				 format-class format-args)))
	(apply #'parse format-instance source builder args)))

(defmethod parse ((format symbol) (source t) (builder t)
		  &rest args &key &allow-other-keys)
  (apply #'parse (list format) source builder args))


;;; Formats
;;

(intern "FORMAT") ;; for (documentation :FORMAT 'rosetta.frontend:format)

(dynamic-classes:define-findable-class-family format
  "This family consists of input format classes. Each input format
class is associated with input sources, encodings and syntax. Input
formats may be file-based, stream-based, buffer-based, may use textual
or binary encodings and may be expressed using different kind of
grammars. Furthermore, input formats may describe semantically
different aspects like data types and software system components."
  (:package *package*))

(defmethod documentation ((thing symbol) (type (eql 'format)))
  "Obtain documentation of type FORMAT from the target class
designated by THING."
  (documentation (find-format-class thing) t))


;;; Comment attaching protocol
;;

(defgeneric most-recent-comment (builder for)
  (:documentation
   "Return the most recent comment (usually a string) encountered by
BUILDER for object FOR or nil if there is no such comment."))

(defgeneric (setf most-recent-comment) (new-value builder for)
  (:documentation
   "Set NEW-VALUE (usually a string) as the most recent comment
encountered by BUILDER for object FOR."))

(defgeneric comment (builder for)
  (:documentation
   "Return the complete comment object (usually a string, concatenated
from individual comment strings) BUILDER created for object FOR."))

(defgeneric (setf comment) (new-value builder for)
  (:documentation
   "Install NEW-VALUE as the complete comment object BUILDER should
associate to object FOR."))

(defgeneric comment? (builder thing)
  (:documentation
   "Return non-nil when BUILDER should treat THING as a comment."))


;;; Dependency resolution protocol
;;

(defgeneric resolve (resolver format location)
  (:documentation
   "Use RESOLVER to resolve the dependency described by FORMAT,
and LOCATION.

FORMAT can be nil to indicate that the format is not known and should
be derived from LOCATION, if possible."))

(defmethod no-applicable-method  ((generic-function (eql #'resolve)) &rest args)
  (let+ (((nil nil location) args))
    (cannot-resolve-dependency location)))


;;; Search path-based resolution protocol
;;

(defgeneric search-path (resolver)
  (:documentation
   "Return the list of paths consulted by RESOLVER to resolve pathname
dependencies."))

(defgeneric (setf search-path) (new-value resolver)
  (:documentation
   "Set the list of paths consulted by RESOLVER to resolve pathname
dependencies to NEW-VALUE."))

(defgeneric if-ambiguous (resolver)
  (:documentation
   "Return the policy RESOLVER applies when encountering ambiguous
dependencies."))

(defgeneric (setf if-ambiguous) (new-value resolver)
  (:documentation
   "Set the policy RESOLVER applies when encountering ambiguous
dependencies to NEW-VALUE."))
