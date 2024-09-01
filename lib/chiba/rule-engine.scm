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

(define-module (chiba rule-engine)
  #:use-module (chiba rule-engine config)
  #:use-module (artanis db)
  #:use-module (ice-9 match)
  #:export (rule-engine-run))

(define (node-config node-name config-expr rc)
  (let ((node (db-get-node node-name)))
    (if node
        (db-update-node node-name config-expr)
        (throw 'artanis-err 400 node-config
               "Node `~a` not found!" node-name))))

(define (chiba:node-monitor node-ip monitor-expr rc)
  (throw 'artanis-err 501 chiba:node-monitor "Not implemented yet!"))

(define (chiba:node-action node-ip action-expr rc)
  (throw 'artanis-err 501 chiba:node-action "Not implemented yet!"))

(define (chiba:node-create node-ip create-expr rc)
  (throw 'artanis-err 501 chiba:node-create "Not implemented yet!"))

(define *node-command-exprs*
  `((config . ,chiba:node-config)
    (monitor . ,chiba:node-monitor)
    (action . ,chiba:node-action)
    (create . ,chiba:node-create)))

(define (node-handle-get cmd)
  (or (assq-ref *node-command-exprs* cmd)
      (throw 'artanis-err 400 node-handler-get
             "Unknown command: `~a`!" cmd)))

(define (global-command-exec expr rc)
  (define (broadcast node-list config-expr)
    (for-each (lambda (node)
                (let ((handler (node-handler-get 'config)))
                  (handler node config-expr rc)))
              node-list))
  (match expr
    ((cmd node-name config-expr)
     ;; TODO: config single node
     (let ((handler (node-handler-get cmd)))
       (handler node-name config-expr rc)))
    ((cmd ('group group-name) config-expr)
     (let ((node-list (db-get-group group-name)))
       (broadcast node-list config-expr)))
    ((cmd (node-list ...) config-expr)
     (broadcast node-list config-expr))
    (else
     (throw 'artanis-err 400 global-command-exec
            "Unknown expression: `~a`!" expr))))

(define *global-command-exprs*
  `((global_config . ,global-config)
    (global_monitor . ,global-monitor)
    (global_action . ,global-action)
    (global_create . ,global-create)))

(define *status-exprs*
  `((show . ,show-status)))

(define *operation-exprs*
  `((dump . ,dump-operate)
    (refresh . ,refresh-operate)))

(define (hendle-get table expr)
  (and (pair? expr)
       (assq-ref table (car expr))))

(define (is-command-expr? expr)
  (assq-ref *command-exprs* expr))

(define (is-status-expr? expr)
  (assq-ref *status-exprs* expr))

(define (is-operation-expr? expr)
  (assq-ref *operation-exprs* expr))

(define (rule-engine-run sexp rc)
  (cond
   ((is-command-expr? sexp)
    => (lambda (handler)
         (handler sexp rc)))
   ((is-status-expr? sexp)
    => (lambda (handler)
         (handler sexp rc)))
   ((is-operation-expr? sexp)
    => (lambda (handler)
         (handler sexp rc)))
   (else
    (throw 'artanis-err rule-engine-run "Unknown expression: `~a`!" sexp))))
