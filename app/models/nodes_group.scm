(import (artanis mvc model)
        (chiba utils))

(export )

;; Model nodes_group definition of chiba
;; Please add your license header here.
;; This file is generated automatically by GNU Artanis.

(create-artanis-model
 nodes_group
 (:deps node)
 (id auto (#:primary-key))
 (name char-field (#:maxlen 255 #:not-null))
 (created_at bigint (#:unsigned #:not-null))
 (modified_at bigint (#:unsigned #:not-null))
 (valid boolean) ; set false to remove group
 (description longtext) ; necessary?
 ) ; DO NOT REMOVE THIS LINE!!!
