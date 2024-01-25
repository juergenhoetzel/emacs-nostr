;;; bech32-nostr.el --- Nostr functions -*- lexical-binding: t; -*-


;; Copyright (C) 2024  Jürgen Hötzel

;; Author: Jürgen Hötzel <juergen@hoetzel.info>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:
(require 'cl-lib)

(require 'bech32)

(defun bech32-nostr-decode (string)
  "Return alist of (key . val) of NIP19 bech32 encoded STRING."
  (cl-destructuring-bind (hrp . numbers) (bech32-decode string)
    (let ((bytes (bech32-convertbits numbers 5 8)))
      (if (member hrp '("npub" "nsec" "note")) ;bake keys and ids
	  (cons (intern hrp) bytes)
	;; TLV encoded
	(let (alist)
	  (while bytes
	    (let ((type (car bytes))
		  (len (cadr bytes)))
	      (cond
	       ((eq type 1) (push (cons 'relay (apply #'string (seq-subseq bytes 2 (+ 2 len)))) alist))
	       ((eq type 0) (cond
			     ((equal hrp "nprofile") (push (cons 'pubkey (seq-subseq bytes 2 (+ 2 len))) alist))
			     ;; FIXME: Other types
			     )))
	      (setq bytes (seq-drop bytes (+ 2 len)))))
	  (cons (intern hrp) alist))))))

(provide 'bech32-nostr)

;;; bech32-nostr.el ends here
