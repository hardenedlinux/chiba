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

(define-module (chiba bmc)
  #:use-module (chiba utils)
  #:use-module (chiba linux)
  #:use-module (artanis client)
  #:use-module (web response)
  #:use-module (json)
  #:use-module ((rnrs) #:select (utf8->string
                                 bytevector?))
  #:export (get-token-from-bmc-node
            bmc-get-test-token
            bmc/api-get
            bmc/api-post
            bmc/api-patch))

(define (http-body-cooker res body)
  (let ((headers (response-headers res)))
    (cond
     ((eq? (assoc-ref headers 'content-encode) 'gzip) (gunzip body))
     ((string? body) body)
     ((bytevector? body) (utf8->string body))
     (else (throw 'artanis-error http-body-cooker
                  "Unknown content-encoding `~a'" headers body)))))

(define (get-token-from-bmc-node addr username password)
  (call-with-values
      (lambda ()
        (artanis:http-post
         (format #f "https://~a/login" addr)
         #:headers '((content-type . application/json))
         #:body (scm->json-string
                 `((username . ,username) (password . ,password)))))
    (lambda (_ body)
      (assoc-ref (json-string->scm body) "token"))))

(define (bmc-get-test-token)
  (get-token-from-bmc-node (bmc-test-addr) 'root '0penBmc))

(define (bmc/api-get addr api token)
  (let ((url (format #f "https://~a/~a" addr api)))
    (call-with-values
        (lambda ()
          (artanis:http-get
           url
           #:headers `((x-auth-token . ,token)) #:bytevector? #t))
      (lambda (res body)
        (cond
         ((= (response-code res) 200) (http-body-cooker res body))
         ((= (response-code res) 404) (throw 'artanis-error bmc/api-get
                                             "Not found `~a'" url))
         ((= (response-code res) 401) (throw 'artanis-error bmc/api-get
                                             "Unauthorized `~a'" url))
         (else (throw 'artanis-error
                      "Unexpected response code `~a' from `~a'"
                      (list res body))))))))

(define (bmc/api-post addr api token body)
  (let ((url (format #f "https://~a/~a" addr api)))
    (call-with-values
        (lambda ()
          (artanis:http-post
           (pk 'post-url url)
           #:headers `((x-auth-token . ,token)
                       (content-type . application/json))
           #:body (pk 'post-body  body)))
      (lambda (res body)
        (cond
         ((= (response-code res) 200) (http-body-cooker res body))
         ((= (response-code res) 404) (throw 'artanis-error bmc/api-post
                                             "Not found `~a'" url))
         ((= (response-code res) 401) (throw 'artanis-error bmc/api-post
                                             "Unauthorized `~a'" url))
         (else (throw 'artanis-error
                      "Unexpected response code `~a' from `~a'"
                      (list res body))))))))

(define (bmc/api-patch addr api token body)
  (let ((url (format #f "https://~a/~a" addr api)))
    (call-with-values
        (lambda ()
          (artanis:http-patch
           url
           #:headers `((x-auth-token . ,token)
                       (content-type . application/json))
           #:body body))
      (lambda (res body)
        (cond
         ((= (response-code res) 200) (http-body-cooker res body))
         ((= (response-code res) 404) (throw 'artanis-error bmc/api-patch
                                             "Not found `~a'" url))
         ((= (response-code res) 401) (throw 'artanis-error bmc/api-patch
                                             "Unauthorized `~a'" url))
         (else (throw 'artanis-error bmc/api-patch
                      "Unexpected response code `~a' from `~a'"
                      (list res body))))))))
