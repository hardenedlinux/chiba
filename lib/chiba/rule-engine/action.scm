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

(define-module (chiba rule-engine action)
  #:use-module (chiba rule-engine utils)
  #:use-module (chiba app model node)
  #:use-module (chiba redfish)
  #:use-module (ice-9 match)
  #:use-module (artanis third-party json)
  #:export (chiba:node-action))

(define (setup-guest-node node-ip config)
  (define get (make-item-getter config-expr))
  (let ((ip (get 'guest-ip))
        (mask (get 'guest-mask))
        (gateway (get 'guest-gateway))
        (dns-servers (or (get 'guest-dns-servers)
                         '()))
        (system-id (or (get 'system-id) "1")))
    (general-config redfish:system-config!
                    node-ip
                    `(("DHCPv4"
                       ("DHCPEnabled" . #t))
                      ("IPv4Addresses"
                       (("Address" . ,ip)
                        ("SubnetMask" . ,mask)
                        ("Gateway" . ,gateway)))
                      ("StaticNameServers" . ,dns-servers))
                    system-id)))

;; Install OS from PXE
;; NOTE:
;; 1. The Redfish API doesn't support the OS installation directly.
;; 2. The OS installation is done by the PXE server.
;; 3. The PXE server should be configured outside of Chiba.
;;    The related OS images, scripts, and preseed files should be provided.
;; 4. Chiba only config the guest node, then force to reboot and let PXE
;;    manage the OS installation.
;; 5. After the OS installation is done, Chiba should check the status and
;;    call the reboot action again.
(define (install-os-from-pxe node-ip config-expr)
  (define get (make-item-getter config-expr))
  (let ((os-image (get 'os-image))
        (preseed-url (get 'preseed-url))
        (post-install (get 'post-install))
        (system-id (or (get 'system-id) "1")))
    (setup-guest-node ip config-expr)
    (general-action redfish:set-boot-first ip system-id "Pxe")
    (general-action redfish:force-pxe-boot ip)
    ;; TODO: Check if the installation is done, then call reboot again
    ;; (general-action redfish:force-reboot ip system-id)
    ))

(define (chiba:node-action node-ip action-expr cmd)
  (match action-expr
    (('reboot system-id)
     (general-action redfish:force-reboot node-ip system-id))
    (('install-os config-expr ...)
     (match boot-mode
       ("PXE" (install-os-from-pxe node-ip config-expr))
       (else (throw 'artanis-err 400 chiba:node-action
                    "The boot mode `~a' hasn't been implemented yet!"
                    boot-mode))))
    (else
     (throw 'artanis-err 400 chiba:node-action
            "Unknown node `~a'" node))))
