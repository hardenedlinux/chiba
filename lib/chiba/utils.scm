;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2024
;;      HardenedLinux developers
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.
;;  If not, see <http://www.gnu.org/licenses/>.

(define-module (chiba utils)
  #:export (bmc-test-addr
            run/status))

(define bmc-test-addr (make-parameter "localhost:2443"))

(define-syntax-rule (run/status status-code body ...)
  (catch #t
    (lambda () body ...)
    (lambda e
      (cond
       ((eq? 'artanis-err (car e)) (apply throw e))
       (else (throw 'artanis-err status-code 'Unknown "~a" e))))))
