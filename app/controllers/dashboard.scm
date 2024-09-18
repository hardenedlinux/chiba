;; Controller dashboard definition of chiba
;; Please add your license header here.
;; This file is generated automatically by GNU Artanis.
(define-artanis-controller dashboard) ; DO NOT REMOVE THIS LINE!!!

(import (app models user)
        (web uri)
        (ice-9 match)
        (chiba utils))

(define (gen-login-page rc)
  (let ((failed (params rc "failed")))
    (view-render "login" (the-environment))))

(dashboard-define
 "login"
 gen-login-page)

(dashboard-define
 "logout"
 (options #:session #t)
 (lambda (rc)
   (:session rc 'drop)
   (rc-set-cookie rc)
   (redirect-to rc "/")))

(dashboard-define
 "/"
 (options #:with-auth gen-login-page)
 (lambda (rc)
   (view-render "dashboard" (the-environment))))
