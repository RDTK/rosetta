;;; protocol.lisp --- Unit tests for the serialization protocol functions.
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


;;; Mock mechanism
;;

(defmethod find-mechanism-class ((spec (eql :mock-for-protocol)))
  (find-class 'mechanism-mock-for-protocol))

(defclass mechanism-mock-for-protocol ()
  ((arg :initarg  :arg
	:reader   mechanism-arg
	:initform 0)))

(defmethod packed-size ((mechanism mechanism-mock-for-protocol)
			(source    t)
			&key)
  (mechanism-arg mechanism))

(defmethod pack ((mechanism   mechanism-mock-for-protocol)
		 (source      t)
		 (destination t)
		 &key)
  (values (mechanism-arg mechanism) destination))

(defmethod pack ((mechanism   mechanism-mock-for-protocol)
		 (source      t)
		 (destination (eql nil))
		 &key)
  (values (mechanism-arg mechanism) :created))

(defmethod unpack ((mechanism   mechanism-mock-for-protocol)
		   (source      t)
		   (destination t)
		   &key)
  (values destination (mechanism-arg mechanism)))


;;; Test suite
;;

(deftestsuite protocol-root (serialization-root)
  ((mechanism (make-instance 'mechanism-mock-for-protocol)))
  (:documentation
   "Unit test suite for the `packed-size', `pack', `pack*' and
`unpack' protocol functions."))

(addtest (protocol-root
          :documentation
	  "Smoke test for the `packed-size' function.")
  packed-size/smoke

  (ensure-cases (input expected-output)
      `((:no-such-mechanism          :error)
	(:mock-for-protocol          0)
	((:mock-for-protocol)        0)
	((:mock-for-protocol :arg 5) 5)
	(,mechanism                  0))

    (if (eq expected-output :error)
	(ensure-condition 'no-such-mechanism-class
	  (packed-size input :does-not-matter))
	(ensure-same (packed-size input :does-not-matter) expected-output
		     :test #'equalp))))

(addtest (protocol-root
          :documentation
	  "Smoke test for the `pack' and `pack*' functions.")
  pack-and-pack*/smoke

  (ensure-cases (mechanism source destination expected-output)
      `((:no-such-mechanism          :bar :foo :error)
	(:mock-for-protocol          :bar :foo (0 :foo))
	((:mock-for-protocol)        :bar :foo (0 :foo))
	((:mock-for-protocol :arg 5) :bar :foo (5 :foo))
	(,mechanism                  :bar :foo (0 :foo)))

    ;; Test `pack'
    (if (eq expected-output :error)
	(ensure-condition 'no-such-mechanism-class
	  (pack mechanism source destination))
	(ensure-same (pack mechanism source destination)
		     (apply #'values expected-output)
		     :test #'equalp))

    ;; Test `pack*'
    (if (eq expected-output :error)
	(ensure-condition 'no-such-mechanism-class
	  (pack* mechanism source))
	(ensure-same (pack* mechanism source) :created
		     :test #'eq))))

(addtest (protocol-root
          :documentation
	  "Smoke test for the `unpack' method.")
  unpack/smoke

  (ensure-cases (mechanism destination expected-output)
      `((:no-such-mechanism          :dest :error)
	(:mock-for-protocol          :dest (:dest 0))
	((:mock-for-protocol)        :dest (:dest 0))
	((:mock-for-protocol :arg 5) :dest (:dest 5))
	(,mechanism                  :dest (:dest 0)))

    (if (eq expected-output :error)
	(ensure-condition 'no-such-mechanism-class
	  (unpack mechanism :does-not-matter destination))
	(ensure-same (unpack mechanism :does-not-matter destination)
		     (apply #'values expected-output)
		     :test #'equalp))))
