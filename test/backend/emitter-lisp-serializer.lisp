;;; emitter-lisp-serializer.lisp --- Unit tests for Lisp serialization emitter.
;;
;; Copyright (C) 2012, 2013 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the
;; GNU Lesser General Public License Version 3 (the ``LGPL''),
;; or (at your option) any later version.
;;
;; Software distributed under the License is distributed
;; on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY KIND, either
;; express or implied. See the LGPL for the specific language
;; governing rights and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html
;; or write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

(cl:in-package :rosetta.backend.test)

(deftestsuite backend-emitter-lisp-serializer-root (backend-root)
  ()
  (:setup
   ;; Generate enums
   (generate +enum/uint8/one+     :class :lisp/compiled)
   (generate +enum/uint32/simple+ :class :lisp/compiled))
  (:documentation
   "Unit test for serialization-related emitter for Lisp."))

(addtest (backend-emitter-lisp-serializer-root
          :documentation
	  "Smoke test for emitting Lisp code for \"packed-size\" target.

We do not verify the emitted code, but its behavior.")
  emit-packed-size/smoke

  (ensure-serialization-cases ()
      `(;; bool, little and big endian
	(type-bool         :mock/little-endian-packed-size ((   nil . 1)
							    (     t . 1)))
	(type-bool         :mock/big-endian-packed-size    ((   nil . 1)
							    (     t . 1)))
	;; uint8, little and big endian
	(type-uint8        :mock/little-endian-packed-size ((     1 . 1)
							    (   255 . 1)))
	(type-uint8        :mock/big-endian-packed-size    ((     1 . 1)
							    (   255 . 1)))
	;; int8, little endian
	(type-int8         :mock/little-endian-packed-size ((     1 . 1)
							    (   127 . 1)
							    (    -1 . 1)
							    (  -128 . 1)))
	;; uint16, little and big endian
	(type-uint16       :mock/little-endian-packed-size ((#x0001 . 2)
							    (#x007f . 2)
							    (#xff00 . 2)))
	(type-uint16       :mock/big-endian-packed-size    ((#x0001 . 2)
							    (#x007f . 2)
							    (#xff00 . 2)))
	;; float32, little endian
	(type-float32      :mock/little-endian-packed-size (( 0.0f0 . 4)
							    ( 1.0f0 . 4)
							    (-2.5f0 . 4)))
	;; ASCII and UTF-8 strings
	(type-ascii-string :mock/little-endian-packed-size ((""     . 1)
							    ("a"    . 2)
							    ("ab"   . 3)))
	(type-utf-8-string :mock/little-endian-packed-size ((""     . 1)
							    ("ä"    . 3)
							    ("äb"   . 4)))
	;; Enum
	(,+enum/uint8/one+ :mock/little-endian-packed-size ((:a . 1)))
	(,+enum/uint32/simple+ :mock/little-endian-packed-size ((:a . 4)
								(:b . 4))))
    (ensure-same (funcall generated input) expected :test #'=)))

(addtest (backend-emitter-lisp-serializer-root
          :documentation
	  "Smoke test for emitting Lisp code for \"pack\" target.

We do not verify the emitted code, but its behavior.")
  emit-pack/smoke

  (ensure-serialization-cases
      (:destination-initform (nibbles:make-octet-vector 100))
      `(;; bool, little and big endian
	(type-bool         :mock/little-endian-pack ((   nil . (#x00))
						     (     t . (#x01))))
	(type-bool         :mock/big-endian-pack    ((   nil . (#x00))
						     (     t . (#x01))))
	;; uint8, little and big endian
	(type-uint8        :mock/little-endian-pack ((     1 . (#x01))
						     (   255 . (#xff))))
	(type-uint8        :mock/big-endian-pack    ((     1 . (#x01))
						     (   255 . (#xff))))
	;; int8, little endian
	(type-int8         :mock/little-endian-pack ((     1 . (#x01))
						     (   127 . (#x7f))
						     (    -1 . (#xff))
						     (  -128 . (#x80))))
	;; uint16, little and big endian
	(type-uint16       :mock/little-endian-pack ((#x0001 . (#x01 #x00))
						     (#x007f . (#x7f #x00))
						     (#xff00 . (#x00 #xff))))
	(type-uint16       :mock/big-endian-pack    ((#x0001 . (#x00 #x01))
						     (#x007f . (#x00 #x7f))
						     (#xff00 . (#xff #x00))))
	;; float32, little endian
	(type-float32      :mock/little-endian-pack (( 0.0f0 . (#x00 #x00 #x00 #x00))
						     ( 1.0f0 . (#x00 #x00 #x80 #x3f))
						     (-2.5f0 . (#x00 #x00 #x20 #xc0))))
	;; ASCII and UTF-8 strings
	(type-ascii-string :mock/little-endian-pack ((""     . (#x00))
						     ("a"    . (#x01 #x61))
						     ("ab"   . (#x02 #x61 #x62))))
	(type-utf-8-string :mock/little-endian-pack ((""     . (#x00))
						     ("ä"    . (#x02 #xc3 #xa4))
						     ("äb"   . (#x03 #xc3 #xa4 #x62))))
	;; Enum
	(,+enum/uint8/one+ :mock/little-endian-pack ((:a . (#x01))))
	(,+enum/uint32/simple+ :mock/little-endian-pack ((:a . (#x01 #x00 #x00 #x00))
							 (:b . (#x02 #x00 #x00 #x00)))))

    (let+ (((&values num-bytes-written result) (funcall generated input)))
      (ensure-same (values num-bytes-written (subseq result 0 num-bytes-written))
		   (values (length expected)
			   (coerce expected 'nibbles:simple-octet-vector))
		   :test #'equalp))))

(addtest (backend-emitter-lisp-serializer-root
          :documentation
	  "Smoke test for emitting Lisp code for \"unpack\" target.

We do not verify the emitted code, but its behavior.")
  emit-unpack/smoke

  (ensure-serialization-cases ()
      `(;; bool, little and big endian
	(type-bool         :mock/little-endian-unpack (((#x00)                . nil)
						       ((#x01)                . t)
						       ((#x02)                . error)))
	(type-bool         :mock/big-endian-unpack    (((#x00)                . nil)
						       ((#x01)                . t)
						       ((#x02)                . error)))
	;; uint8, little and big endian
	(type-uint8        :mock/little-endian-unpack (((#x01)                . 1)
						       ((#xff)                . 255)))
	(type-uint8        :mock/big-endian-unpack    (((#x01)                . 1)
						       ((#xff)                . 255)))
	;; int8, little endian
	(type-int8         :mock/little-endian-unpack (((#x01)                . 1)
						       ((#x7f)                . 127)
						       ((#xff)                . -1)
						       ((#x80)                . -128)))
	;; uint16, little and big endian
	(type-uint16       :mock/little-endian-unpack (((#x01 #x00)           . #x0001)
						       ((#x7f #x00)           . #x007f)
						       ((#x00 #xff)           . #xff00)))
	(type-uint16       :mock/big-endian-unpack    (((#x00 #x01)           . #x0001)
						       ((#x00 #x7f)           . #x007f)
						       ((#xff #x00)           . #xff00)))
	;; float32, little endian
	(type-float32      :mock/little-endian-unpack (((#x00 #x00 #x00 #x00) .  0.0f0)
						       ((#x00 #x00 #x80 #x3f) .  1.0f0)
						       ((#x00 #x00 #x20 #xc0) . -2.5f0)))
	;; ASCII and UTF-8 strings
	(type-ascii-string :mock/little-endian-unpack (((#x00)                . "")
						       ((#x01 #x61)           . "a")
						       ((#x02 #x61 #x62)      . "ab")))
	(type-utf-8-string :mock/little-endian-unpack (((#x00)                . "")
						       ((#x02 #xc3 #xa4)      . "ä")
						       ((#x03 #xc3 #xa4 #x62) . "äb")))
	;; Enum
	(,+enum/uint8/one+ :mock/little-endian-unpack (((#x01)                . :a)
						       #+later ((#x02)                . error))) ;;; TODO(jmoringe, 2012-12-11): we optimize too aggressively to catch this
	(,+enum/uint32/simple+ :mock/little-endian-unpack (((#x01 #x00 #x00 #x00) . :a)
							   ((#x02 #x00 #x00 #x00) . :b)
							   ((#x03 #x00 #x00 #x00) . error))))
    (ensure-same (funcall generated (coerce input 'nibbles:simple-octet-vector))
		 (values (length input) expected)
		 :test #'equalp)))
