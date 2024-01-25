;;; bech32.el --- ELisp implementation for Bech32      -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Jürgen Hötzel

;; Author: Jürgen Hötzel <juergen@hoetzel.info>
;; Keywords: comm, nostr, data
;; Version: 0.2
;; Package-Requires: ((emacs "28.1"))

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


(require 'seq)
(require 'hexl)

(defvar bech32-chars [?q?p?z?r?y?9?x?8?g?f?2?t?v?d?w?0?s?3?j?n?5?4?k?h?c?e?6?m?u?a?7?l])

(defun bech32--base5-to-number (numbers)
  (let ((n-trail (mod (* (length numbers) 5) 8))
	(n (seq-reduce (lambda (acc x)
			 (+ (ash acc 5) x))
		       numbers 0)))
    (when (or (> n-trail 4) (> 0 (logand (1- (ash 1 n-trail)) n)))
      (error "Trailing bits MUST all be zero"))
    (ash  n (- n-trail))))


(defun bech32-hrp-expand(s)
  "Return expansion of S into values for checksum computation."
  (append (mapcar (lambda (c) (ash c -5 )) s)
	  (cons 0 (mapcar (lambda (c) (logand c 31)) s))))


(defun bech32-checksum(hrp numbers)
  "Return the checksum values given HRP and NUMBERS."
  (let* ((values (append (bech32-hrp-expand hrp) numbers))
	 (polymod (logxor (bech32-polymod (append values '(0 0 0 0 0 0 ))) 1)))
    (mapcar (lambda (i) (logand (ash polymod (* -5 (- 5 i))) 31))
	    (number-sequence 0 5))))

(defconst bech32--gen '(#x3B6A57B2 #x26508E6D #x1EA119FA #x3D4233DD #x2A1462B3))

(defun bech32-polymod (numbers)
  "Return Bech32 checksum of NUMBERS."
  (let ((chk 1))
    (dolist (n numbers)
      (let ((b (ash chk -25)))
	(setq chk (logxor (ash (logand chk  #x1FFFFFF) 5) n))
	(seq-do-indexed (lambda (g i) (setq chk (logxor chk (if (zerop (logand (ash b (- i)) 1)) 0 g)))) bech32--gen)))
    chk))

(defun bech32-valid-checksum-p (hrp numbers)
  "Return t if checksum of expaned HRP and NUMBERS is valid"
  (= (bech32-polymod (append (bech32-hrp-expand hrp) numbers)) 1))

(defun bech32-hex-decode-string (string)
  "Hex-decode STRING and return the result as a unibyte string."
  ;; Pad the string with a leading zero if its length is odd.
  (unless (zerop (logand (length string) 1))
    (setq string (concat "0" string)))
  (apply #'unibyte-string
         (seq-map (lambda (s) (hexl-htoi (aref s 0) (aref s 1)))
                  (seq-partition string 2))))

(defun bech32-decode (string &optional convertbits)
  "Decode a bech32 STRING into cons cell (HRP . DATA)

If CONVERTBITS is t, DATA is reduced from big endian 5-bit integers to
single arbitrary precision number."
  (seq-doseq (c string)
    (unless (<= 33 c 126)
      (error "Invalid bech32 char: %c" c)))
  (unless (or (equal (downcase string) string) (equal (upcase string) string))
    (error "bech32 should either have all of it's letter in uppercase or lowercase"))
  (let* ((bstring (downcase string))
	 (pos1 (string-search "1" bstring))) ;FIXME: 1 is possible value in HRP
    (unless pos1
      (error "Missing seperator char"))
    (when (zerop pos1)
      (error "Invalid seperator position 0"))
    (let ((hrp (substring bstring 0 pos1))
	  (data (seq-map (lambda (c)
			   (seq-position bech32-chars c))
			 (substring bstring (1+ pos1)))))
      (unless (bech32-valid-checksum-p hrp data)
	(error "Invalid checksum for HRP %s and data %s" hrp data))
      (cons hrp
	    (if convertbits
		(bech32--base5-to-number (butlast data 6))
	      (butlast data 6))))))

(defun bech32-encode(hrp numbers):
       "Return Bech32 string given HRP and NUMBERS.

NUMBERS is expected to be a seq of 5 bit numbers."
       (let ((combined (append numbers (bech32-checksum hrp numbers))))
	 (concat hrp "1" (concat (mapcar (lambda (x) (aref bech32-chars x)) combined)))))


(defun bech32-convertbits (numbers frombits tobits &optional pad)
  "Return numbers converted from power-of FROMBITS base to power-of TOBITS

Include trailing padding char if NOPAD is t."
  (let ((acc 0)
	(bits 0)
	(max-value (1- (ash 1 tobits)))
	(max-acc (1- (ash 1 (+ frombits tobits -1))))
	ret)
    (seq-doseq (value numbers)
      (when (or (< value 0) (> (ash (- frombits) value) 0))
	(error "number %s doesn't fit into source encoding" value))
      (setq acc (logand (logior (ash acc frombits) value) max-acc)
	    bits (+ bits frombits))
      (while (>= bits tobits)
	(setq bits (- bits tobits))
	(push (logand (ash acc (- bits)) max-value) ret)))
    (when (and pad bits)
      (push (logand (ash acc (- tobits bits)) max-value) ret))
    (nreverse ret)))

(provide 'bech32)


;;; bech32.el ends here
