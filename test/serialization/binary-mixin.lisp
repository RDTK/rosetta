;;; binary-mixin.lisp --- Unit tests for the binary-mixin class.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
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

(cl:in-package :rosetta.serialization.test)


;;; Mock mechanism class
;;

(defmethod find-mechanism-class ((spec (eql :mock-for-binary-mixin)))
  (find-class 'mechanism-mock-for-binary-mixin))

(defclass mechanism-mock-for-binary-mixin (binary-mixin)
  ())

(defmethod packed-size ((mechanism mechanism-mock-for-binary-mixin)
			(source    t)
			&key)
  (length (sb-ext:string-to-octets (format nil "~S" source))))

(defmethod pack ((mechanism   mechanism-mock-for-binary-mixin)
		 (source      t)
		 (destination simple-array)
		 &key
		 (start 0))
  (let ((result (sb-ext:string-to-octets (format nil "~S" source))))
    (replace destination result :start1 start)
    (values (length result) destination)))

(defmethod unpack ((mechanism   mechanism-mock-for-binary-mixin)
		   (source      simple-array)
		   (destination t)
		   &key)
  (read-from-string (sb-ext:octets-to-string source)))


;;; Test suite
;;

(deftestsuite binary-mixin-root (serialization-root)
  ((simple-mechanism (make-instance 'mechanism-mock-for-binary-mixin)))
  (:function
   (octetify (thing)
     (etypecase thing
       (string
	(sb-ext:string-to-octets thing))
       (sequence
	(coerce thing 'binio:octet-vector)))))
  (:documentation
   "Unit tests for the `binary-mixin' mixin class."))

(addtest (binary-mixin-root
          :documentation
	  "Test method on `pack' for nil destination.")
  pack/nil

  (ensure-cases (source start expected-size expected-destination)
      `((:foo nil 4 ,(octetify ":FOO"))
	(5    nil 1 ,(octetify "5"))
	(:foo 0   4 ,(octetify ":FOO"))
	(5    0   1 ,(octetify "5"))
	;; (:foo 1   4 ,(octetify "0:FOO"))
	;; (5    1   1 ,(octetify "05"))
	)

    (bind (((:values size destination)
	    (if start
		(pack simple-mechanism source nil :start start)
		(pack simple-mechanism source nil))))
      (ensure-same size        expected-size        :test #'=)
      (ensure-same destination expected-destination :test #'equalp))))

(addtest (binary-mixin-root
          :documentation
	  "Test method on `pack' for stream destinations.")
  pack/stream

  (ensure-cases (source start expected-size expected-output)
      `((:foo nil 4 ,(octetify ":FOO"))
	(5    nil 1 ,(octetify "5"))
	(:foo 0   4 ,(octetify ":FOO"))
	(5    0   1 ,(octetify "5"))
	;; (:foo 1   4 ,(octetify "0:FOO"))
	;; (5    1   1 ,(octetify "05"))
	)

    (bind ((stream (make-in-memory-output-stream))
	   ((:values size destination)
	    (if start
		(pack simple-mechanism source stream :start start)
		(pack simple-mechanism source stream)))
	   (output (get-output-stream-sequence stream)))
      (ensure-same destination stream          :test #'eq)
      (ensure-same size        expected-size   :test #'=)
      (ensure-same output      expected-output :test #'equalp))))

(addtest (binary-mixin-root
          :documentation
	  "Test method on `pack' for pathname destinations.")
  pack/pathname

  (ensure-cases (source start pathname expected-size expected-output)
      `((:foo nil #P"/tmp/foo.bin" 4 ,(octetify ":FOO"))
	(:foo nil #P"/tmp/foo.bin" 4 ,(octetify ":FOO")) ;; requires superseding the file
	(5    nil #P"/tmp/foo"     1 ,(octetify "5"))    ;; no file type
	(:foo 0   #P"/tmp/foo.bin" 4 ,(octetify ":FOO"))
	;; (:foo 1   #P"/tmp/foo.bin" 4 ,(octetify "0:FOO"))
	)

    (bind (((:values size destination)
	    (if start
		(pack simple-mechanism source pathname :start start)
		(pack simple-mechanism source pathname)))
	   (output (read-file-into-byte-vector pathname)))
      (ensure-same destination pathname        :test #'eq)
      (ensure-same size        expected-size   :test #'=)
      (ensure-same output      expected-output :test #'equalp))))

(addtest (binary-mixin-root
          :documentation
	  "Test method on `unpack' for stream source.")
  unpack/stream

  (ensure-cases (source start expected-output expected-size)
      `((,(octetify ":FOO")  nil :foo 4)
	(,(octetify "5")     nil 5    1)
	(,(octetify ":FOO")  0   :foo 4)
	(,(octetify "5")     0   5    1)
	(,(octetify "_:FOO") 1   :foo 4)
	(,(octetify "_5")    1   5    1))

    (with-input-from-sequence (stream source)
      (bind (((:values output size)
	      (if start
		  (unpack simple-mechanism stream :unused :start start)
		  (unpack simple-mechanism stream :unused))))
	(ensure-same output expected-output :test #'equalp)
	(ensure-same size   expected-size   :test #'=)))))

(addtest (binary-mixin-root
          :documentation
	  "Test method on `unpack' for pathname source.")
  unpack/pathname

  (ensure-cases (input pathname start expected-output expected-size)
      `((,(octetify ":FOO")  #P"/tmp/foo.txt" nil :foo 4)
	(,(octetify "5")     #P"/tmp/foo"     nil 5    1)
	(,(octetify ":FOO")  #P"/tmp/foo.txt" 0   :foo 4)
	(,(octetify "5")     #P"/tmp/foo"     0   5    1)
	(,(octetify "_:FOO") #P"/tmp/foo.txt" 1   :foo 4)
	(,(octetify "_5")    #P"/tmp/foo"     1   5    1))

    (write-byte-vector-into-file input pathname :if-exists :supersede)
    (bind (((:values output size)
	    (if start
		(unpack simple-mechanism pathname :unused :start start)
		(unpack simple-mechanism pathname :unused))))
      (ensure-same output expected-output :test #'equalp)
      (ensure-same size   expected-size   :test #'=))))
