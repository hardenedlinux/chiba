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

(define-module (chiba rule-engine config)
  #:use-module (chiba app model node)
  #:use-module (chiba redfish)
  #:use-module (ice-9 match)
  #:use-module (artanis third-party json)
  #:export (chiba:node-config))

(define (storage-config node-ip id storage-config-expr rc)
  (let* ((config (scm->json-string storage-config-expr))
         (token (try-to-get-token-from-ip node-ip))
         (api-call (make-redfish-call node-ip token)))
    (redfish:systems-config! api-call id config)))

(define (chiba:node-config node-ip config-expr rc)
  (match config-expr
    (('storage id storage-config-expr ...)
     (storage-config node-ip id storage-config-expr rc))
    (else
     (throw 'artanis-err 400 chiba:node-config
            "Unknown config type: ~a" config-expr))))
