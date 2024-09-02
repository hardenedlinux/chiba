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

(define (general-config redfish-call node-ip config-expr cmd)
  (let* ((config (scm->json-string config-expr))
         (token (try-to-get-token-from-ip node-ip))
         (api-call (make-redfish-call node-ip token)))
    (redfish-call api-call cmd config)))

(define (system-config node-ip node-ip config-expr cmd)
  (general-config redfish:systems-config! node-ip system-config-expr cmd))

(define (system-update node-ip system-id system-config-expr cmd)
  (general-config redfish:systems-update! node-ip system-config-expr cmd))

(define (system-delete node-ip system-id system-config-expr cmd)
  (general-config redfish:systems-delete! node-ip system-config-expr cmd))

(define (storage-config/volumes node-ip system-id storage-id storage-config-expr rc)
  (let ((cmd (format #f "~a/Storage/~a/Volumes" system-id storage-id)))
    (system-config node-ip system-id storage-config-expr cmd)))

(define (storage-update/volumes node-ip system-id storage-id volume-id storage-config-expr rc)
  (let ((cmd (format #f "~a/Storage/~a/Volumes/~a" system-id storage-id volume-id)))
    (system-update node-ip system-id storage-config-expr cmd)))

(define (storage-config/drive node-ip system-id storage-id storage-config-expr rc)
  (let ((cmd (format #f "~a/Storage/~a/Drives" system-id storage-id)))
    (system-config node-ip system-id storage-config-expr cmd)))

(define (storage-update/drive node-ip system-id storage-id drive-id storage-config-expr rc)
  (let ((cmd (format #f "~a/Storage/~a/Drives/~a" system-id storage-id drive-id)))
    (system-update node-ip system-id storage-config-expr cmd)))

(define (storage-delete/volumes node-ip system-id storage-id storage-config-expr rc)
  (let ((cmd (format #f "~a/Storage/~a/Volumes" system-id storage-id)))
    (system-delete node-ip system-id storage-config-expr cmd)))

(define (storage-delete/drive node-ip system-id storage-id storage-config-expr rc)
  (let ((cmd (format #f "~a/Storage/~a/Drives" system-id storage-id)))
    (system-delete node-ip system-id storage-config-expr cmd)))

(define (storage-action/volumes node-ip system-id storage-id volume-id act storage-config-expr rc)
  (let ((cmd (format #f "~a/Storage/~a/Volumes/~a/Actions/~a" system-id storage-id volume-id act)))
    (system-config node-ip system-id storage-config-expr cmd)))

(define (network-config node-ip system-id eth network-config-expr rc)
  (let ((cmd (format #f "~a/EthernetInterfaces/~a" system-id eth)))
    (system-config node-ip system-id network-config-expr cmd)))

(define (chiba:node-config node-ip config-expr rc)
  (match config-expr
    (('storage-create ('mode mode) ('system-id system-id) ('storage-id storage-id) storage-config-expr ...)
     (case mode
       ((volumes)
        (storage-config/volumes node-ip system-id storage-id storage-config-expr rc))
       ((drive)
        (storage-config/drive node-ip system-id storage-id storage-config-expr rc))
       (else
        (throw 'artanis-err 400 chiba:node-config "Unknown storage mode: ~a" mode))))
    (('storage-update ('mode mode) ('system-id system-id) ('storage-id storage-id)
                      ('volume-id volume-id) storage-config-expr ...)
     (storage-update/volumes node-ip system-id storage-id volume-id storage-config-expr rc))
    (('storage-update ('mode mode) ('system-id system-id) ('storage-id storage-id)
                      ('drive-id drive-id) storage-config-expr ...)
     (storage-update/drive node-ip system-id storage-id drive-id storage-config-expr rc))
    (('storage-delete ('mode mode) ('system-id system-id) ('storage-id storage-id) storage-config-expr ...)
     (case mode
       ((volumes)
        (storage-delete/volumes node-ip system-id storage-id storage-config-expr rc))
       ((drive)
        (storage-delete/drive node-ip system-id storage-id storage-config-expr rc))
       (else
        (throw 'artanis-err 400 chiba:node-config "Unknown storage mode: ~a" mode))))
    (('storage-action ('mode mode) ('system-id system-id) ('storage-id storage-id) ('volume-id volume-id)
                      ('act act) storage-config-expr ...)
     (storage-action/volumes node-ip system-id storage-id volume-id act storage-config-expr rc))
    (('network system-id eth network-config-expr ...)
     (network-config node-ip system-id eth network-config-expr rc))

    (else
     (throw 'artanis-err 400 chiba:node-config
            "Unknown config type: ~a" config-expr))))
