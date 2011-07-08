;;; textual-mixin.lisp --- Unit tests for the textual-mixin class.
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(in-package :rosetta.serialization.test)


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


;;; Test suite
;;

(deftestsuite textual-mixin-root (serialization-root)
  ((simple-mechanism (make-instance 'mechanism-mock-for-textual-mixin)))
  (:documentation
   "Unit tests for the `textual-mixin' mixin class."))

(addtest (textual-mixin-root
          :documentation
	  "Test method on `pack' for nil destination.")
  pack/nil

  (ensure-cases (source expected-size expected-destination)
      '((:foo 4 ":FOO")
	(5    1 "5"))

    (bind (((:values size destination)
	    (pack simple-mechanism source nil)))
      (ensure-same size        expected-size        :test #'=)
      (ensure-same destination expected-destination :test #'string=))))

(addtest (textual-mixin-root
          :documentation
	  "Test method on `pack' for stream destinations.")
  pack/stream

  (ensure-cases (source expected-size expected-output)
      '((:foo 4 ":FOO")
	(5    1 "5"))

    (bind ((stream (make-string-output-stream))
	   ((:values size destination)
	    (pack simple-mechanism source stream))
	   (output (get-output-stream-string stream)))
      (ensure-same destination stream          :test #'eq)
      (ensure-same size        expected-size   :test #'=)
      (ensure-same output      expected-output :test #'string=))))

(addtest (textual-mixin-root
          :documentation
	  "Test method on `pack' for pathname destinations.")
  pack/pathname

  (ensure-cases (source pathname expected-size expected-output)
      '((:foo #P"/tmp/foo.txt" 4 ":FOO")
	(:foo #P"/tmp/foo.txt" 4 ":FOO") ;; required superseding the file
	(5    #P"/tmp/foo"     1 "5"))   ;; no file type

    (bind (((:values size destination)
	    (pack simple-mechanism source pathname))
	   (output (read-file-into-string pathname)))
      (ensure-same destination pathname        :test #'eq)
      (ensure-same size        expected-size   :test #'=)
      (ensure-same output      expected-output :test #'string=))))
