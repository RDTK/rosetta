;;; protocol.lisp --- Unit tests for the protocol of the frontend module.
;;
;; Copyright (C) 2012 Jan Moringen
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

(cl:in-package :rosetta.frontend.test)

(deftestsuite rosetta.frontend.protocol-root (frontend-root)
  ()
  (:documentation
   "Unit tests for protocol functions of the frontend module."))

(addtest (rosetta.frontend.protocol-root
          :documentation
	  "Test the format and builder lookup performed by
`process'.")
  process/lookup

  (ensure-cases (args expected)
      `(;; Invalid format.
	((:no-such-format   #P"does-not-matter" :model)             no-such-format-class)
	(((:no-such-format) #P"does-not-matter" :model)             no-such-format-class)

	;; Invalid builder.
	((:mock             #P"does-not-matter" :no-such-builder)   no-such-builder-class)
	((:mock             #P"does-not-matter" (:no-such-builder)) no-such-builder-class)

	;; These are OK.
	((:mock             #P"does-not-matter" list)               t)
	((:mock             #P"does-not-matter" list)               t))
    (let+ (((&flet do-it () (apply #'process args))))
     (ecase expected
       (no-such-format-class  (ensure-condition 'no-such-format-class  (do-it)))
       (no-such-builder-class (ensure-condition 'no-such-builder-class (do-it)))
       ((t)                   (do-it))))))
