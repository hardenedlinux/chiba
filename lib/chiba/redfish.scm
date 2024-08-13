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

(define-module (chiba redfish)
  #:use-module (chiba utils)
  #:use-module (chiba bmc)
  #:use-module (json)
  #:export (redfish:systems
            redfish:chassis
            redfish:managers
            redfish:sessions
            redfish:system-power-off/soft
            redfish:system-power-off/hard
            redfish:system-power-on
            redfish:system-power-reboot
            redfish:bmc-reboot
            redfish:bmc-factory-reset
            redfish:log-entry
            redfish:log-delete
            make-redfish-call
            make-redfish-call/test))

(define (redfish/v1 api)
  (format #f "redfish/v1/~a" api))

(define (gen-redfish-api method api api-call . args)
  (json-string->scm (apply (api-call method
                                     (pk 'api (redfish/v1 api)))
                           args)))

(define (redfish:systems api-call)
  (gen-redfish-api 'get "Systems" api-call))

(define (redfish:chassis api-call)
  (gen-redfish-api 'get "Chassis" api-call))

(define (redfish:managers api-call)
  (gen-redfish-api 'get "Managers" api-call))

(define (redfish:sessions api-call)
  (gen-redfish-api 'get "SessionService/Sessions" api-call))

(define (redfish:actions api-call type action data)
  (gen-redfish-api
   'post
   (format #f "Systems/~a/Actions/~a" type action)
   api-call
   (scm->json-string data)))

(define (redfish:system-power-off/soft api-call)
  (redfish:actions
   api-call
   "system"
   "ComputerSystem.Reset"
   '(("ResetType" . "GracefulShutdown"))))

(define (redfish:system-power-off/hard api-call)
  (redfish:actions
   api-call
   "system"
   "ComputerSystem.Reset"
   '(("ResetType" . "ForceOff"))))

(define (redfish:system-power-on api-call)
  (redfish:actions
   api-call
   "system"
   "ComputerSystem.Reset"
   '(("ResetType" . "On"))))

(define (redfish:system-power-reboot api-call)
  (redfish:actions
   api-call
   "system"
   "ComputerSystem.Reset"
   '(("ResetType" . "GracefulRestart"))))

(define (redfish:bmc-reboot api-call)
  (redfish:actions
   api-call
   "bmc"
   "Manager.Reset"
   '(("ResetType" . "GracefulRestart"))))

(define (redfish:bmc-factory-reset api-call)
  (redfish:actions
   api-call
   "bmc"
   "Manager.ResetToDefaults"
   '(("ResetToDefaultsType" . "ResetAll"))))

(define (redfish:log-entry api-call)
  (gen-redfish-api
   'get
   "Systems/system/Actions/LogServices/EventLog/Entries"
   api-call))

(define (redfish:log-delete api-call)
  (gen-redfish-api
   'post
   "Systems/system/LogServices/EventLog/Actions/LogService.Reset"
   api-call))

(define (redfish:firmware-apply-time/immediately api-call)
  (gen-redfish-api
   'patch
   "UpdateService"
   '(("HttpPushUriOptions"
      ("HttpPushUriApplyTime" ("ApplyTime" . "Immediate"))))))

(define (redfish:firmware-apply-time/on-reset api-call)
  (gen-redfish-api
   'patch
   "UpdateService"
   '(("HttpPushUriOptions"
      ("HttpPushUriApplyTime" ("ApplyTime" . "OnReset"))))))

(define (redfish:firmware-update api-call)
  (gen-redfish-api
   'post
   "UpdateService/Actions/UpdateService.SimpleUpdate"
   api-call))

(define (make-redfish-call addr token)
  (lambda (method api)
    (lambda args
      (case method
        ((get) (bmc/api-get addr api token))
        ((post) (apply bmc/api-post addr api token args))
        ((patch) (apply bmc/api-patch addr api token args))
        (else (throw 'artanis-error "Unknown method `~a'" method))))))

(define (make-redfish-call/test token)
  (make-redfish-call (bmc-test-addr) token))
