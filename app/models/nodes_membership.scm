(import (artanis mvc model)
        (chiba utils))

(export )

;; Model nodes_membership definition of chiba
;; Please add your license header here.
;; This file is generated automatically by GNU Artanis.
(create-artanis-model
 nodes_membership
 (:deps node group)
 (id auto (#:primary-key))
 (node_id int (#:unsigned #:not-null)) ; 2^32 nodes, enough!
 (group_id int (#:unsigned #:not-null)) ; 2^32 memberships, enough?
 (valid boolean) ; set false to remove the node from group
 ); DO NOT REMOVE THIS LINE!!!
