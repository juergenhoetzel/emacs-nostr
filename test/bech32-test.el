;;  -*- lexical-binding: t -*-
(require 'eldev)
(require 'ert)

(require 'cl-lib)
(require 'bech32)
(require 'bech32-nostr)


(ert-deftest bech32-test-decoding ()
  (cl-destructuring-bind (hrp . n) (bech32-decode "npub10elfcs4fr0l0r8af98jlmgdh9c8tcxjvz9qkw038js35mp4dma8qzvjptg" t)
    (should (equal (format "%x" n) "7e7e9c42a91bfef19fa929e5fda1b72e0ebc1a4c1141673e2794234d86addf4e"))
    (should (equal hrp "npub")))
  (cl-destructuring-bind (hrp . n) (bech32-decode "nsec1vl029mgpspedva04g90vltkh6fvh240zqtv9k0t9af8935ke9laqsnlfe5" t)
    (should (equal (format "%x" n) "67dea2ed018072d675f5415ecfaed7d2597555e202d85b3d65ea4e58d2d92ffa"))
    (should (equal hrp "nsec"))))


(ert-deftest bech32-test-encoding ()
  (should (equal
	   (bech32-encode "npub" (bech32-convertbits (bech32-hex-decode-string "7e7e9c42a91bfef19fa929e5fda1b72e0ebc1a4c1141673e2794234d86addf4e") 8 5 t))
	   "npub10elfcs4fr0l0r8af98jlmgdh9c8tcxjvz9qkw038js35mp4dma8qzvjptg"))
  (should (equal
	   (bech32-encode "nsec" (bech32-convertbits (bech32-hex-decode-string "67dea2ed018072d675f5415ecfaed7d2597555e202d85b3d65ea4e58d2d92ffa") 8 5 t))
	   "nsec1vl029mgpspedva04g90vltkh6fvh240zqtv9k0t9af8935ke9laqsnlfe5")))

(ert-deftest bech32-nip19-decoding ()
  (should (equal
	   (bech32-nostr-decode "nprofile1qqsrhuxx8l9ex335q7he0f09aej04zpazpl0ne2cgukyawd24mayt8gpp4mhxue69uhhytnc9e3k7mgpz4mhxue69uhkg6nzv9ejuumpv34kytnrdaksjlyr9p")
	   '(nprofile
	     (relay . "wss://djbas.sadkb.com")
	     (relay . "wss://r.x.com")
	     (pubkey 59 240 198 63 203 147 70 52 7 175 151 165 229 238 100 250 136 61 16 126 249 229 88 71 44 78 185 170 174 250 69 157)))))


