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

(define-module (chiba rule-engine utils)
  #:use-module (chiba app model node)
  #:use-module (chiba redfish)
  #:use-module (artanis third-party json)
  #:export (rule-general-call
            general-config
            make-item-getter))

(define (general-config redfish-call node-ip config-expr cmd)
  (let* ((config (scm->json-string config-expr))
         (token (try-to-get-token-from-ip node-ip))
         (api-call (make-redfish-call node-ip token)))
    (redfish-call api-call cmd config)))

(define (rule-general-call redfish-call node-ip . args)
  (let* ((token (try-to-get-token-from-ip node-ip))
         (api-call (make-redfish-call node-ip token)))
    (apply redfish-call api-call args)))

(define (make-item-getter config-expr)
  (lambda (key)
    (and=> (assoc-ref config-expr key) car)))
