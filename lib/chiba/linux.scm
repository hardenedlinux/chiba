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

(define-module (chiba linux)
  #:use-module (chiba cli)
  #:use-module ((rnrs) #:select (put-bytevector))
  #:export (gunzip))

(define* (mktemp #:key (pattern "/tmp/tmp-XXXXXX") (mode "w+"))
  (let ((port (mkstemp! (string-copy pattern) mode)))
    (chmod port (logand #o666 (lognot (umask))))
    port))

(define (gunzip bv)
  (let* ((port (mktemp))
         (filename (port-filename port)))
    (put-bytevector port bv)
    (close-output-port port)
    (let ((ret (cli-run* zcat ,filename)))
      (if (zero? (<cli>-status ret))
          (<cli>-result ret)
          (error "zcat failed" ret)))))
