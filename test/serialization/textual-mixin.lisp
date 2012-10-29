;;; textual-mixin.lisp --- Unit tests for the textual-mixin class.
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

(cl:in-package :rosetta.serialization.test)


;;; Mock mechanism class
;;

(defmethod find-mechanism-class ((spec (eql :mock-for-textual-mixin)))
  (find-class 'mechanism-mock-for-textual-mixin))

(defclass mechanism-mock-for-textual-mixin (textual-mixin)
  ())

(defmethod pack ((mechanism   mechanism-mock-for-textual-mixin)
		 (source      t)
		 (destination (eql 'string))
		 &key)
  (let ((result (format nil "~S" source)))
    (values (length result) result)))

(defmethod unpack ((mechanism   mechanism-mock-for-textual-mixin)
		   (source      string)
		   (destination t)
		   &key)
  (read-from-string source))


;;; Test suite
;;

(deftestsuite textual-mixin-root (serialization-root)
  ((simple-mechanism (make-instance 'mechanism-mock-for-textual-mixin)))
  (:documentation
   "Unit tests for the `textual-mixin' mixin class."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-basic-textual-mechanism-tests (class)
    (let ((suite-name (symbolicate class "-ROOT")))
      `(progn
	 (addtest (,suite-name
		   :documentation
		   "Test method on `pack' for nil destination.")
	   pack/nil

	   (ensure-cases (source expected-size expected-destination)
	       '((:foo 4 ":FOO")
		 (5    1 "5"))

	     (let+ (((&values size destination)
		     (pack simple-mechanism source nil)))
	       (when size
		 (ensure-same size expected-size :test #'=))
	       (ensure-same destination expected-destination :test #'string=))))

	 (addtest (,suite-name
		   :documentation
		   "Test method on `pack' for stream destinations.")
	   pack/stream

	   (ensure-cases (source expected-size expected-output)
	       '((:foo 4 ":FOO")
		 (5    1 "5"))

	     (let+ ((stream (make-string-output-stream))
		    ((&values size destination)
		     (pack simple-mechanism source stream))
		    (output (get-output-stream-string stream)))
	       (ensure-same destination stream :test #'eq)
	       (when size
		 (ensure-same size expected-size :test #'=))
	       (ensure-same output expected-output :test #'string=))))

	 (addtest (,suite-name
		   :documentation
		   "Test method on `pack' for pathname destinations.")
	   pack/pathname

	   (ensure-cases (source pathname expected-size expected-output)
	       '((:foo #P"/tmp/foo.txt" 4 ":FOO")
		 (:foo #P"/tmp/foo.txt" 4 ":FOO") ;; requires superseding the file
		 (5    #P"/tmp/foo"     1 "5"))   ;; no file type

	     (let+ (((&values size destination)
		     (pack simple-mechanism source pathname))
		    (output (read-file-into-string pathname)))
	       (ensure-same destination pathname :test #'eq)
	       (when size
		 (ensure-same size expected-size :test #'=))
	       (ensure-same output expected-output :test #'string=))))

	 (addtest (,suite-name
		   :documentation
		   "Test method on `unpack' for stream source.")
	   unpack/stream

	   (ensure-cases (source expected-output expected-size)
	       '((":FOO" :foo 4)
		 ("5"    5    1))

	     (with-input-from-string (stream source)
	       (let+ (((&values output size)
		       (unpack simple-mechanism stream :unused)))
		 (ensure-same output expected-output :test #'equalp)
		 (when size
		   (ensure-same size expected-size :test #'=))))))

	 (addtest (,suite-name
		   :documentation
		   "Test method on `unpack' for pathname source.")
	   unpack/pathname

	   (ensure-cases (input pathname expected-output expected-size)
	       '((":FOO" #P"/tmp/foo.txt" :foo 4)
		 ("5"    #P"/tmp/foo"     5    1))

	     (write-string-into-file input pathname :if-exists :supersede)
	     (let+ (((&values output size)
		     (unpack simple-mechanism pathname :unused)))
	       (ensure-same output expected-output :test #'equalp)
	       (when size
		 (ensure-same size expected-size :test #'=)))))))))

(define-basic-textual-mechanism-tests textual-mixin)
