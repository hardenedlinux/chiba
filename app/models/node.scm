(import (artanis mvc model)
        (chiba bmc))

(export get-node-by-ip
        apply-token-for-node!
        init-new-node
        try-to-get-token-from-ip)

;; Model node definition of chiba
;; Please add your license header here.
;; This file is generated automatically by GNU Artanis.

(create-artanis-model
 node
 (id auto (#:primary-key))
 (ip char-field (#:unique #:not-null #:maxlen 45))
 (token char-field (#:maxlen 512))
 (vendor char-field (#:maxlen 64))
 (hostname char-field (#:maxlen 255))
 (model char-field (#:maxlen 64))
 (firmware_version char-field (#:maxlen 50))
 (last_updated datetime)
 (status char-field (#:maxlen 10))
 (location char-field (#:maxlen 255))
 (contact_person char-field (#:maxlen 100))
 (maintenance_mode boolean)
 ) ; DO NOT REMOVE THIS LINE!!!

(define* (get-node-by-ip ip #:key (check? #f))
  (run/status
   500
   (cond
    (($node 'get #:condition (where #:ip ip))
     => identity))
   (check? #f)
   (else (throw 'artanis-err 404 get-node-by-ip
                "Node ip `~a` not found!" ip))))

(define (apply-token-for-node! node)
  (let* ((username (assoc-ref node "username"))
         (password (assoc-ref node "password"))
         (token (get-token-from-bmc-node ip username password)))
    (run/status
     500
     ($node 'update #:condition (where #:id (assoc-ref node "id"))
            #:token token
            #:last_updated (current-datetime))
     token)))

(define* (init-new-node #:key ip username password)
  (run/status
   500
   ($node 'set #:ip ip
          #:username username
          #:password password
          #:token (get-token-from-bmc-node ip username password)
          #:last_updated (current-datetime))
   (get-node-by-ip ip))
  (apply-token-for-node! node))

(define (try-to-get-token-from-ip ip)
  (let ((node (get-node-by-ip ip #:check? #t)))
    (if (not node)
        (assoc-ref node "token")
        (apply-token-for-node-ip! node))))
