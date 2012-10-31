;;; format-mixins.lisp --- Mixin classes for format classes.
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


;;; `binary-format-mixin' mixin class
;;

(defclass binary-format-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into format classes for binary
formats."))

(defmethod parse ((format  binary-format-mixin)
		  (source  pathname)
		  (builder t)
		  &rest args &key &allow-other-keys)
  "Open a binary input stream for the file designated by SOURCE and
call a method specialized on streams."
  (handler-bind ((location-condition
		   #'(lambda (condition)
		       (let+ (((&accessors-r/o location) condition)
			      ((&accessors (source1 source)) location))
			 (unless (pathnamep source1)
			   (setf source1 source))))))
    (with-input-from-file (stream source :element-type '(unsigned-byte 8))
      (apply #'parse format stream builder args))))


;;; `text-format-mixin' mixin class
;;

(defclass text-format-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into format classes that
operate on textual input data."))

(defmethod parse ((format  text-format-mixin)
		  (source  pathname)
		  (builder t)
		  &rest args &key &allow-other-keys)
  "Open a character input stream for the file designated by SOURCE and
call a method specialized on streams."
  (handler-bind ((location-condition
		   #'(lambda (condition)
		       (let+ (((&accessors-r/o location) condition)
			      ((&accessors (source1 source) source-content) location))
			 (unless (pathnamep source1)
			   (setf source1 source))
			 (unless source-content
			   (setf source-content
				 (read-file-into-string source)))))))
    (with-input-from-file (stream source)
      (apply #'parse format stream builder args))))

(defmethod parse ((format  text-format-mixin)
		  (source  string)
		  (builder t)
		  &rest args &key &allow-other-keys)
  "Create an input stream for the content of SOURCE and call a method
specialized on streams."
  (handler-bind ((location-condition
		   #'(lambda (condition)
		       (let+ (((&accessors-r/o location) condition)
			      ((&accessors source-content) location))
			 (unless source-content
			   (setf source-content source))))))
    (with-input-from-string (stream source)
      (apply #'parse format stream builder args))))
