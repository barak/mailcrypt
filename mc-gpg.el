;; mc-gpg.el, GPG support for Mailcrypt
;; Copyright (C) 1995  Jin Choi <jin@atype.com>
;;                     Patrick LoPresti <patl@lcs.mit.edu>
;;               1998  Brian Warner <warner@lothar.com>

;; $Id$

;;{{{ Licensing
;; This file is intended to be used with GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;}}}
(require 'mailcrypt)

; pieces to do:

; #key lookup?
; #mc-gpg-encrypt-region
;  need to deal with untrusted keys, missing keys (offer to fetch), --throw
; #mc-gpg-decrypt-region [anything not clearsigned] (a,as,ae,ase)
;  need to implement signature-key fetch, ponder --throw-keyid case
; #mc-gpg-sign-region (clearsign/notclearsign)
; #mc-gpg-verify-region [clearsigned only] (ok/badsig/missingkey/corruptmsg)
; #mc-gpg-insert-public-key (comment, altkeyring)
; #mc-gpg-snarf-keys (one, multiple, old, corrupt)
; key fetching (is there a GPG key server yet?)
; clean up use of buffers, #kill off old tmp buffers
; in verify-region, print date of signature too
;  ~maybe have bad-signature message print keyid/date? (no, sig is invalid,
;  ~ anything other than its invalidity is misleading)
; make messages shorter (get it all to fit in echo area)

; enhancements I'd like to add
;  trustdb status reporting during encryption/decryption: show the best trust
;   path to the recipient/signer?
;  completion on local id when signing (--list-secret-keys should know them)
;  algorithm preferences, possibly by destination user
;   (this is embedded in gpg)
;  extra options, possibly by destination user. Maybe for pgp5.0/pgp2.6 compat?
;  rfc2015 operation (MIME: application/pgp-signature, etc)

; mc-gpg-alternate-keyring seems dubious.. have two options, public/private?

; using a shell introduces concerns about quoting and such. If the name of a
; key used as a recipient or as a mc-gpg-user-id (a key to sign with) has a
; double quote or ! or weird stuff, things could break.

; encrypting to a nontrusted key is problematic: when not in --batch mode,
; gpg warns the user and asks if they want to use the key anyway. In --batch
; mode, it fails, even if we give --yes. Worse yet, if we encrypt to multiple
; recipients, the untrusted ones get dropped withou flagging an error (stderr
; does get a message, but it doesn't indicate which keys had a problem)

(defvar mc-gpg-user-id (user-login-name)
  "*GPG ID of your default identity.")
(defvar mc-gpg-path "gpg" "*The GPG executable.")
(defvar mc-gpg-display-snarf-output nil
  "*If t, pop up the GPG output window when snarfing keys.")
(defvar mc-gpg-alternate-keyring nil
  "*Public keyring to use instead of default.")
(defvar mc-gpg-comment
;  (format "Processed by Mailcrypt %s, an Emacs/GPG interface" mc-version)
  nil
  "*Comment field to appear in ASCII armor output.  If nil, let GPG use its 
default.")
(defconst mc-gpg-msg-begin-line "-----BEGIN PGP MESSAGE-----"
  "Text for start of GPG message delimiter.")
(defconst mc-gpg-msg-end-line "-----END PGP MESSAGE-----\n?"
  "Text for end of GPG message delimiter.")
(defconst mc-gpg-signed-begin-line "-----BEGIN PGP SIGNED MESSAGE-----"
  "Text for start of GPG signed messages.")
(defconst mc-gpg-signed-end-line "-----END PGP SIGNATURE-----\n?"
  "Text for end of GPG signed messages.")
(defconst mc-gpg-key-begin-line "^-----BEGIN PGP PUBLIC KEY BLOCK-----\r?$"
  "Text for start of GPG public key.")
(defconst mc-gpg-key-end-line "^-----END PGP PUBLIC KEY BLOCK-----\r?$"
  "Text for end of GPG public key.")
(defconst mc-gpg-error-re "^\\(ERROR:\\|WARNING:\\).*"
  "Regular expression matching an error from GPG")
(defconst mc-gpg-sigok-re "^gpg: Good signature.*"
  "Regular expression matching a GPG signature validation message")
(defconst mc-gpg-newkey-re 
  "^[^:]+:[^:]+: \\(key [0-9A-F]+\\): \\(.*\\)$"
  "Regular expression matching a GPG key snarf message")
(defconst mc-gpg-nokey-re
  "Cannot find the public key matching userid '\\(.+\\)'$"
  "Regular expression matching a GPG missing-key messsage")
(defconst mc-gpg-key-expected-re
  "gpg: Signature made .+ using .+ key ID \\(\\S +\\)\ngpg: Can't check signature: Public key not found")
(defconst mc-gpg-extra-args nil
  "Extra arguments to pass to all invocations of gpg. Used during debugging to
set --homedir, to use special test keys instead of the developer's normal
keyring.")
(defconst mc-gpg-debug-buffer nil
  "A buffer for debugging messages. If nil, no debugging messages are logged.")

; we use with-current-buffer for clarity. emacs19 doesn't have it. This
; code is cribbed from lazy-lock.el which does the same thing
(eval-when-compile
  ;; We use this for clarity and speed.  Borrowed from a future Emacs.
  (or (fboundp 'with-current-buffer)
      (defmacro with-current-buffer (buffer &rest body)
	"Execute the forms in BODY with BUFFER as the current buffer.
The value returned is the value of the last form in BODY."
	(` (save-excursion (set-buffer (, buffer)) (,@ body)))))
  )

; set this with (setq mc-gpg-debug-buffer (get-buffer-create "mc debug"))
(defun mc-gpg-debug-print (string)
  (if (and (boundp 'mc-gpg-debug-buffer) mc-gpg-debug-buffer)
      (print string mc-gpg-debug-buffer)))

;; the insert parser will return t and insert the whole of stdout if rc == 0,
;; and will error out with the stderr text if rc != 0
(defun mc-gpg-insert-parser (stdoutbuf stderrbuf statusbuf rc)
  (mc-gpg-debug-print 
   (format "(mc-gpg-generic-parser stdoutbuf=%s stderrbuf=%s rc=%s"
	   stdoutbuf stderrbuf rc))
  (if (= rc 0)
      '(t t)
    ;;(list rc nil nil)
    (error (with-current-buffer stderrbuf (buffer-string)))
))

;; the null parser returns rc and never inserts anything
(defun mc-gpg-null-parser (stdoutbuf stderrbuf statusbuf rc)
  (list t rc))

; utility function (variant of mc-process-region):
; take region in current buffer, send as stdin to a process
; maybe send in a passphrase first
; three buffers of output are collected: stdout, stderr, and --status-fd
;
; parser is called with stdoutbuf as the current buffer as
;  (parser stdoutbuf stderrbuf statusbuf rc)
; and is expected to return a list:
;  '(REPLACEP RESULT)
;
; if REPLACEP is true, the original buffer's [beg..end] will be replaced by
; the stdout data buffer's contents (all of it). Otherwise the original buffer
; is left alone. RESULT (specifically (cdr parser-return-value)) is returned
; by mc-gpg-process-region.

(defun mc-gpg-process-region (beg end passwd program args parser bufferdummy)
  (let ((obuf (current-buffer))
	(process-connection-type nil)
	(shell-file-name "/bin/sh") ;; ??? force? need sh (not tcsh) for "2>"
	; other local vars
	mybuf 
	stderr-tempfilename stderr-buf
	status-tempfilename status-buf
	proc rc status parser-result
	)
    (mc-gpg-debug-print (format 
       "(mc-gpg-process-region beg=%s end=%s passwd=%s program=%s args=%s parser=%s bufferdummy=%s)"
       beg end passwd program args parser bufferdummy))
    (setq stderr-tempfilename 
	  (make-temp-name (expand-file-name "mailcrypt-gpg-stderr-"
					    mc-temp-directory)))
    (setq status-tempfilename 
	  (make-temp-name (expand-file-name "mailcrypt-gpg-status-"
					    mc-temp-directory)))
    (unwind-protect
	(progn
	  ;; get output places ready
	  (setq mybuf (get-buffer-create " *mailcrypt stdout temp"))
	  (set-buffer mybuf)
	  (erase-buffer)
	  (set-buffer obuf)
	  (buffer-disable-undo mybuf)

	  (if passwd
	      (setq args (append '("--passphrase-fd" "0") args)))
	  (setq args (append (list (concat "2>" stderr-tempfilename)) args))
	  (setq args (append (list (concat "3>" status-tempfilename)) args))
	  (setq args (append '("--status-fd" "3") args))

	  (if mc-gpg-extra-args
	      (setq args (append mc-gpg-extra-args args)))

	  (mc-gpg-debug-print (format "prog is %s, args are %s" 
				      program 
				      (mapconcat '(lambda (x) 
						    (format "'%s'" x)) 
						 args " ")))

	  (setq proc
		(apply 'start-process-shell-command "*GPG*" mybuf 
		       program args))
	  ;; send in passwd if necessary
	  (if passwd
	      (progn
		(process-send-string proc (concat passwd "\n"))
		(or mc-passwd-timeout (mc-deactivate-passwd t))))
	  ;; send in the region
	  (process-send-region proc beg end)
	  ;; finish it off
	  (process-send-eof proc)
	  ;; wait for it to finish
	  (while (eq 'run (process-status proc))
	    (accept-process-output proc 5))
	  ;; remember result codes
	  (setq status (process-status proc))
	  (setq rc (process-exit-status proc))
	  (mc-gpg-debug-print (format "prog finished, rc=%s" rc))

	  ;; Hack to force a status_notify() in Emacs 19.29
	  (delete-process proc)

	  ;; remove the annoying "yes your process has finished" message
	  (set-buffer mybuf)
	  (goto-char (point-max))
	  (if (re-search-backward "\nProcess \\*GPG.*\n\\'" nil t)
	      (delete-region (match-beginning 0) (match-end 0)))
	  (goto-char (point-min))
	  ;; CRNL -> NL
	  (while (search-forward "\r\n" nil t)
	    (replace-match "\n"))

	  ;; ponder process death: signal, not just rc!=0
	  (if (or (eq 'stop status) (eq 'signal status))
	      ;; process died
	      (error "%s exited abnormally: '%s'" program rc) ;;is rc a string?
	    )

	  (if (= 127 rc)
	      (error "%s could not be found" program) ;; at least on my system
	    )

	  ;; fill stderr buf
	  (setq stderr-buf (get-buffer-create " *mailcrypt stderr temp"))
	  (buffer-disable-undo stderr-buf)
	  (set-buffer stderr-buf)
	  (erase-buffer)
	  (insert-file-contents stderr-tempfilename)

	  ;; fill status buf
	  (setq status-buf (get-buffer-create " *mailcrypt status temp"))
	  (buffer-disable-undo status-buf)
	  (set-buffer status-buf)
	  (erase-buffer)
	  (insert-file-contents status-tempfilename)

	  ;; feed the parser
	  (set-buffer mybuf)
	  (setq parser-result (funcall parser mybuf stderr-buf status-buf rc))
	  (mc-gpg-debug-print (format " parser returned %s" parser-result))

	  ;; what did the parser tell us?
	  (if (car parser-result)
	      ;; yes, replace region
	      (progn
		(set-buffer obuf)
		(delete-region beg end)
		(goto-char beg)
		(insert-buffer-substring mybuf)
		))

	  ;; return result
	  (cdr parser-result)
	  )
      ;; cleanup forms
      (if (and proc (eq 'run (process-status proc)))
	  ;; it is still running. kill it.
	  (interrupt-process proc))
      (set-buffer obuf)
      (delete-file stderr-tempfilename)
      (delete-file status-tempfilename)
      ;; kill off temporary buffers (which would be useful for debugging)
      (if t ;; nil for easier debugging
	  (progn
	    (if (get-buffer " *mailcrypt stdout temp")
		(kill-buffer " *mailcrypt stdout temp"))
	    (if (get-buffer " *mailcrypt stderr temp")
		(kill-buffer " *mailcrypt stderr temp"))
	    (if (get-buffer " *mailcrypt status temp")
		(kill-buffer " *mailcrypt status temp"))
	    ))
)))


; this lookup is used to turn key identifiers into names suitable for
; presentation to the user. When decrypting, the hex keyid to which the
; incoming message is encrypted is looked up to ask the user for a passphrase
; by name. When encrypting, the user's id (mc-gpg-user-id) is looked up to
; ask for a passphrase, and if mc-gpg-encrypt-to-me is true, the user's id
; is looked up to provide a full name to gpg. gpg is always given full names,
; because the hex keyids it provides might not work for both signing and
; encryption (split keys in gpg/pgp5)
;
;31:warner@zs2-pc4% gpg --list-secret-keys --with-colons --no-greeting
;/home/warner/.gnupg/secring.gpg
;-------------------------------
;sec::1024:17:1FE9CBFDC63B6750:1998-08-04:0:::Brian Warner (temporary GPG key) <warner@lothar.com>:
;ssb::1024:20:C68E8DE9F759FBDE:1998-08-04:0:::
;sec::768:17:16BD446D567E33CF:1998-08-04:0:::signature (sample signature key) <key@key>:
;sec::768:16:D514CB72B37D9AF4:1998-08-04:0:::crypt (crypt) <crypt@crypt>:
;sec::1024:17:4DBDD3258230A3E0:1998-08-04:0:::dummyy <d@d>:
;ssb::1024:20:549B0E6CBBBB43D1:1998-08-04:0:::
;
; we use the whole user id string (Brian..lothar.com>) as USER-ID, and the
; long keyid 1FE9CBFDC63B6750 for KEY-ID

(defvar mc-gpg-key-cache nil
  "Association list mapping GPG IDs to canonical \"keys\".  A \"key\"
is a pair (USER-ID . KEY-ID) which identifies the canonical IDs of the
GPG ID.")

(defun mc-gpg-lookup-key (str &optional type)
  ;; Look up the string STR in the user's secret key ring.  Return a
  ;; pair of strings (USER-ID . KEY-ID) which uniquely identifies the
  ;; matching key, or nil if no key matches.
  (let (args)
    (if (equal str "***** CONVENTIONAL *****") nil
      (let ((result (cdr-safe (assoc str mc-gpg-key-cache)))
	    (key-regexp
	     "^sec:[^:]*:[^:]*:[^:]*:\\([^:]*\\):[^:]*:[^:]*:[^:]*:[^:]*:\\([^:]*\\):$"
	     )
	    (obuf (current-buffer))
	    buffer)
	(if (null result)
	    (unwind-protect
		(progn
		  (setq buffer (generate-new-buffer " *mailcrypt temp"))
		  (setq args (list 
			      "--with-colons" 
			      "--no-greeting" "--batch" 
			      "--list-secret-keys" str 
			      ))
		  (if mc-gpg-alternate-keyring
		      (setq args (append (list "--keyring" 
					       mc-gpg-alternate-keyring) 
					 args)))
		  (if mc-gpg-extra-args
		      (setq args (append mc-gpg-extra-args args)))
		  (mc-gpg-debug-print 
		   (format "lookup: args are %s" args))
		  (apply 'call-process mc-gpg-path nil buffer nil args)
		  (set-buffer buffer)
		  (goto-char (point-min))
		  (if (re-search-forward key-regexp nil t)
		      (progn
			(setq result
			      (cons (buffer-substring-no-properties
				     (match-beginning 2) (match-end 2))
				    (concat
				     "0x"
				     (buffer-substring-no-properties
				      (match-beginning 1) (match-end 1)))))
			(setq mc-gpg-key-cache (cons (cons str result)
						     mc-gpg-key-cache)))))
					;(if buffer (kill-buffer buffer))
	      (set-buffer obuf)))
	(if (null result)
	    (error "No GPG secret key for %s" str))
	result))))

;gpg: no info to calculate a trust probability
;gpg: no valid addressees
;gpg: [stdin]: encryption failed: No such user id

(defun mc-gpg-encrypt-region (recipients start end &optional id sign)
  (let ((process-environment process-environment)
	(buffer (get-buffer-create mc-buffer-name))
	(obuf (current-buffer))
	action msg args key passwd result gpg-id)
    (mc-gpg-debug-print (format 
       "(mc-gpg-encrypt-region recipients=%s start=%s end=%s id=%s sign=%s)"
       recipients start end id sign))
    
    (setq args (list 
		"--batch" "--armor" "--textmode" "--always-trust"
		(if recipients "--encrypt" "--store")
		))
    (setq action (if recipients "Encrypting" "Armoring"))
    (setq msg (format "%s..." action))  ; May get overridden below
    (if mc-gpg-comment
	(setq args (append (list "--comment" (format "'%s'" mc-gpg-comment))
			   args)))
    (if mc-gpg-alternate-keyring
	(setq args (append (list "--keyring" mc-gpg-alternate-keyring) args)))

    (if (and (not (eq mc-pgp-always-sign 'never))
	     (or mc-pgp-always-sign sign (y-or-n-p "Sign the message? ")))
	(progn
	  (setq key (mc-gpg-lookup-key (or id mc-gpg-user-id) 'encrypt))
	  (setq passwd
		(mc-activate-passwd
		 (cdr key)
		 (format "GPG passphrase for %s (%s): " (car key) (cdr key))))
	  (setq args
		(append (list "--local-user" (cdr key)
			      "--sign" 
			      )
			args))
	  (setq msg (format "%s+signing as %s ..." action (car key)))
	  (if (not recipients)
	      ;; the --store is last in args. remove it. remove --textmode too
	      (setq args (nreverse (cddr (nreverse args)))))
	  )
      )

    ; if we're supposed to encrypt for the user too, we need to know their key
    (if (and recipients mc-encrypt-for-me)
	(setq recipients (cons (cdr (or key
					(setq key (mc-gpg-lookup-key 
						   mc-gpg-user-id 'encrypt)))
				    ) recipients)))

    ; push(@args, map {qq<-r "$_">} @recipients) if @recipients; # roughly
    (if recipients
	(setq args (append (apply 'append 
				  (mapcar '(lambda (x) 
					     (list "--remote-user" 
						   (concat "\"" x "\""))) 
					  recipients))
			   args)))

    (message "%s" msg)
    (setq result (mc-gpg-process-region start end passwd mc-gpg-path args
					'mc-gpg-insert-parser buffer))
    (if (not (car result))
	(error "%s failed: %s" msg (nth 2 result)))

    t
))



; todo: verify all possible cases

; gpg's behavior:
;  encrypted to a key we do not have: emits
;   gpg: public key decryption failed: Secret key not available
;   gpg: decryption failed: Secret key not available
;   rc == 2
;  encrypted to a key we do have but in --batch without --passphrase-fd, emits
;   gpg: fatal: Can't query password in batchmode
;   secmem usage: 1472/1472 bytes in 3/3 blocks of pool 1472/16384
;   [GNUPG:] NEED_PASSPHRASE 414F124832654831
;   rc == 2
;  encrypted to a key we do have, but our passphrase was wrong:
;   [GNUPG:] NEED_PASSPHRASE 414F124832654831
;   gpg: public key decryption failed: Bad passphrase
;   gpg: decryption failed: Secret key not available
;   rc == 2
;  encrypted to our key, passphrase ok:
;   [GNUPG:] NEED_PASSPHRASE 414F124832654831
;   <message>
;   rc == 0
;  encrypted to our key, sig from known key, passphrase ok:
;   <message>
;   gpg: Signature made Thu Aug  6 16:35:13 1998 using DSA key ID C63B6750
;   [GNUPG:] GOODSIG C63B6750 Brian Warner (temporary ...
;   gpg: Good signature from "Brian Warner (temporary GPG key) <warner@lothar.com>"
;   [GNUPG:] TRUST_ULTIMATE
;   rc == 0
; encrypted to us, sig from unknown key, passphrase ok:
;   [GNUPG:] NEED_PASSPHRASE 414F124832654831
;   <message>
;   gpg: Signature made Thu Aug  6 17:17:35 1998 using DSA key ID 567E33CF
;   [GNUPG:] ERRSIG
;   gpg: Can't check signature: Public key not found
;   rc == 2
; conventionally encrypted but we didn't give a passphrase
;   gpg: fatal: Can't query password in batchmode
;   rc == 2
; conventionally encrypted, we gave the wrong passphrase
;   gpg: decryption failed: Bad key
;   rc == 2
; conventionally encrypted, correct passphrase
;   <message>
;   rc == 0
; signed, not encrypted, not clearsigned, known key
;   <message>
;   GOODSIG
;   TRUST_ULTIMATE
;   gpg: Signature made Fri Sep 18 23:15:47 1998 PD using DSA key ID 4B75DDCF
;   gpg: Good signature from "owner1 <user@test>"

;; this parser's return convention:
;;   '( (
;;0      have-secret-key ; we are a recipient (TODO: stealth)
;;1      passphrase-ok ; t was good, nil was bad, keyid: need pw for keyid
;;2      signature: 
;;        nil: no sig
;;        keyid-hex : don't have signature key
;;        '(keyid-string t trust date) : good signature on date with trust
;;        '(keyid-string nil trust date) : bad signature on date with trust
;;       )
;;      begin end )
; todo: stealth ("--throw-keyid")?

;; cases:
;;  *not addressed to us (nil nil nil)
;;  conventionally encrypted
;;   *didn't give passphrase ('symmetric "***** CONVENTIONAL *****" nil)
;;   did give passphrase
;;    *bad passphrase ('symmetric nil nil)
;;    *good passphrase ('symmetric t nil)
;;  signed (not clearsigned), not encrypted
;;    *don't have key ('signed t keyid)
;;    do have key
;;     *good sig ('signed t (t keyid-string date trust))
;;     *bad sig ('signed t (nil keyid-string date trust))
;;  addressed to us:
;;   *didn't give passphrase (t keyid nil)
;;   gave passphrase:
;;    *bad passphrase (t nil nil)
;;    good passphrase
;;     decrypted ok
;;      *no signature (t t nil)
;;      yes signature
;;       *don't have key (offer to fetch) (t t keyid)
;;       do have key
;;        *good sig (t t (t keyid-string date trust))
;;        *bad sig (t t (nil keyid-string date trust))

; this parser's job is to find the decrypted data if any is available. The
; code in -decrypt-region will worry about reporting other status information
; like signatures

; FIXME: if the process dies (say, gpg can't be found), this replaces the
; region with void. Bad, parser, bad.

(defun mc-gpg-decrypt-parser (stdoutbuf stderrbuf statusbuf rc)
  (let (keyid sigtype symmetric sigid sigdate sigtrust)
    (set-buffer statusbuf)
    (goto-char (point-min))
    (if (re-search-forward "NEED_PASSPHRASE \\(\\S +\\)" nil t)
	(setq keyid (concat "0x" (match-string 1))))
    (goto-char (point-min))
    (if (re-search-forward "\\(\\S +SIG\\)" nil t)
	(setq sigtype (match-string 1)))
    (goto-char (point-min))
    (if (re-search-forward "\\(TRUST_\\S +\\)$" nil t)
	(setq sigtrust (match-string 1)))
    (set-buffer stderrbuf)
    (goto-char (point-min))
    (if (and (not keyid)
	     (re-search-forward 
	      "^gpg: public key decryption failed: Secret key not available$" 
	      nil t))
	;; encrypted to a key we do not have. Bail now.
	(list nil nil nil nil)
      (progn
	(goto-char (point-min))
	(if (re-search-forward
	     "^gpg: Signature made \\(.*\\) using" nil t)
	    (setq sigdate (match-string 1)))
	(goto-char (point-min))
	(if (equal sigtype "ERRSIG")
	    (if (re-search-forward
		 "^gpg: Signature made .* key ID \\(.*\\)$" nil t)
		(setq sigid (concat "0x" (match-string 1))))
	  (if (re-search-forward 
	       "^gpg: \\S + signature from \"\\(.*\\)\"$" nil t)
	      (setq sigid (match-string 1))))
	(goto-char (point-min))
	(cond
	 ((re-search-forward 
	   "^gpg: fatal: Can't query password in batchmode$" nil t)
	    ;; didn't give a password.
	  (if keyid
	      ;; public key encryption
	      (list nil t keyid nil)
	    ;; symmetric encryption
	    (list nil 'symmetric "***** CONVENTIONAL *****" nil)))
	 ;; did give a password
	 ((re-search-forward
	   "^gpg: public key decryption failed: Bad passphrase$" nil t)
	    ;; bad passphrase
	  (list nil t nil nil) ; pke
	  )
	 ((re-search-forward
	   "^gpg: decryption failed: Bad key$" nil t)
	  (list nil 'symmetric nil nil))
	 ;; password/passphrase was good (we were able to decrypt the message)
	 ;; or the message was just signed
	 ;; figure out signatureness
	 ((not keyid)
	  ;; not PKE: symmetric or just signed
	  (if sigtype
	      ;; signed only
	      (cond
	       ((equal sigtype "ERRSIG")
		(list t 'signed t sigid))
	       ((equal sigtype "GOODSIG")
		(list t 'signed t (list t sigid sigtrust sigdate)))
	       (t
		(list t 'signed t (list nil sigid sigtrust sigdate)))
	       )
	    ;; symmetric. Don't bother with sig
	    (list t 'symmetric t nil)))
	 ((not sigtype)
	  ;; no signature
	  (list t t t nil))
	 ((equal sigtype "ERRSIG")
	  ;; missing key
	  (list t t t sigid))
	 ((equal sigtype "GOODSIG")
	  (list t t t (list t sigid sigtrust sigdate)))
	 (t
	  (list t t t (list nil sigid sigtrust sigdate)))
	 )))
    ))

;; decrypt-region is first called without ID. This means we'll try to decrypt
;; without a passphrase, almost guaranteed to fail, but it will tell us which
;; key is necessary. We then call decrypt-region again, this time with ID
;; set. This second time will lookup ID and ask the user for the passphrase.

(defun mc-gpg-decrypt-region (start end &optional id)
  ;; returns a pair (SUCCEEDED . VERIFIED) where SUCCEEDED is t if
  ;; the decryption succeeded and verified is t if there was a valid signature
  (let ((process-environment process-environment)
	(buffer (get-buffer-create mc-buffer-name))
	(obuf (current-buffer))
	args key new-key passwd result gpg-id)
    (mc-gpg-debug-print (format "(mc-gpg-decrypt-region start=%s end=%s id=%s)"
				start end id))
    (undo-boundary)
    (if id
	;; second time through, now we know who the message is for.
	;; id is either a hex keyid of the (first?) secret key that is in
	;; the message's recipient list, or "**..CONVENTIONAL.."
	(progn
	  (setq key (mc-gpg-lookup-key id 'encrypt))
	  ;; key is nil if CONVENTIONAL, (string . hexid) otherwise
	  (setq passwd
		(if key
		    (mc-activate-passwd (car key)
					(format 
					 "GPG passphrase for %s (%s): "
					 (car key) (cdr key)))
		  (mc-activate-passwd 
		   id "GPG passphrase for conventional decryption: ")))))
    (setq args '("--batch"))
    (if mc-gpg-alternate-keyring
	(setq args (append args (list "--keyring" mc-gpg-alternate-keyring))))
    (setq args (append args '("--decrypt"))) ; this wants to be last
    (message "Decrypting...")
    (setq result
	  (mc-gpg-process-region
	   start end passwd mc-gpg-path args 'mc-gpg-decrypt-parser buffer))
    ;(message "Decrypting... Done.")
    (cond
     ((not (nth 0 result)) ;; we were not a recipient
      (error "This message is not addressed to you"))
     ((not (nth 1 result)) ;; passphrase-ok is nil: bad passphrase
      (mc-deactivate-passwd t)
      (error "That passphrase was wrong"))
     ((not (equal (nth 1 result) t)) ;; passphrase-ok is keyid: need passphrase
      ;; get passphrase for (nth 1 result), try again
      (mc-gpg-decrypt-region start end (nth 1 result))
      )
     ;; passphrase was ok, were able to decrypt
     ((nth 2 result) ;; there was a signature
      (let ((sig (nth 2 result)))
	(cond
	 ((atom sig) ;; don't have the signature key
	  (progn
	    ;; offer to fetch the key, then what? run again? must we undo 1st?
	    (message (format "cannot check signature from keyid %s" sig))
	    (if (and (not (eq mc-gpg-always-fetch 'never))
		     (or mc-gpg-always-fetch
			 (y-or-n-p
			  (format "Key %s not found; attempt to fetch? " sig)))
		     (mc-gpg-fetch-key (cons nil sig)))
		(progn
		  (undo-start)
		  (undo-more 1)
		  (mc-gpg-decrypt-region start end id))
	      '(t . nil))
	    ))
	 ((nth 0 sig) ;; good signature
	  (progn
	    ;; message about who made the signature. This is a bit wide..
	    ;; the date can easily run off the echo area. Consider replacing
	    ;; 'Good signature' with 'good sig', but keep it consistent with
	    ;; everything else.
	    (message (format "Good signature from '%s' %s made %s"
			     (nth 1 sig) (nth 2 sig) (nth 3 sig)))
	    '(t . t)
	    ))
	 (t ;; bad signature
	  (progn
	    ;; message about who made the bad signature?? misleading?
	    (message (format "BAD SIGNATURE from '%s' %s made %s"
			     (nth 1 sig) (nth 2 sig) (nth 3 sig)))
	    '(t . nil)
	    ))
       )))
     (t ;; no signature
      (message "Decrypting... Done.")
      '(t . nil)
      ))
    ))

(defun mc-gpg-sign-region (start end &optional id unclear)
  (let ((process-environment process-environment)
	(buffer (get-buffer-create mc-buffer-name))
	passwd args key result)
    (setq key (mc-gpg-lookup-key (or id mc-gpg-user-id) 'sign))
    (setq passwd
	  (mc-activate-passwd
	   (car key)
	   (format "GPG passphrase for %s (%s): " (car key) (cdr key))))
    (setq args
	  (list
	   "--batch" "--armor"
	   "--local-user" (cdr key)
	   (if unclear "--sign" "--clearsign")
	   ))
    (if mc-gpg-comment
	(setq args (append (list "--comment" (format "'%s'" mc-gpg-comment))
			   args)))
    (if mc-gpg-extra-args
	(setq args (append mc-gpg-extra-args args)))
    (message "Signing as %s ..." (car key))
    (setq result (mc-gpg-process-region start end passwd mc-gpg-path args
					'mc-gpg-insert-parser buffer))
    (if (car result)
	(message "Signing as %s ... Done." (car key))
      (progn
	(mc-deactivate-passwd t)
	(error "Signature failed: %s" (nth 2 result))
	))
    (car result)
))

; our convention for this parser: return '((STATUS MESSAGE) nil nil). 
;  STATUS= 'good: MESSAGE is displayed in the echo area (name of signator,
;                 timestamp of signature, trust level
;  STATUS= 'bad:  MESSAGE is displayed with a beep. (just dump stderr)
;  STATUS= 'needkey: MESSAGE is the hex keyid required

; the way GPG works:

;  GOOD sig from a trusted key (gpgm --check-trustdb):
;   rc=0, 
;   stderr:
;gpg: Signature made Mon Sep  7 01:19:29 1998 using DSA key ID FE8E94E9
;gpg: Good signature from "trusted <trusted@test>"
;   status:
;[GNUPG:] GOODSIG
;[GNUPG:] TRUST_FULLY

;  GOOD sig from an untrusted key
;   rc=0
;gpg: Signature made Mon Sep  7 01:19:29 1998 using DSA key ID DA4E030E
;gpg: Good signature from "untrusted <untrusted@test>"
;gpg: WARNING: This key is not certified with a trusted signature!
;gpg:          There is no indication that the signature belongs to the owner.
;[GNUPG:] GOODSIG
;[GNUPG:] TRUST_UNDEFINED

;  BAD sig from a trusted key:
;   rc=1
;   stderr:
;gpg: Signature made Mon Sep  7 01:19:28 1998 using DSA key ID 4B75DDCF
;gpg: BAD signature from "owner1 <user@test>"
;   status:
;[GNUPG:] BADSIG

;  sig from a missing key:
;   rc=2
;   stderr:
;gpg: Signature made Mon Sep  7 01:19:29 1998 using DSA key ID 2B09EB69
;gpg: Can't check signature: Public key not found
;   status:
;[GNUPG:] ERRSIG

(defun mc-gpg-verify-parser (stdoutbuf stderrbuf statusbuf rc)
  (let (
	(status (let (msg)
		  (set-buffer statusbuf)
		  (goto-char (point-min))
		  (while (re-search-forward "^\\[GNUPG:\\]\\s +\\(\\S +\\)"
					    nil t)
		    (setq msg (append msg (list (match-string 1)))))
		  (mapconcat 'identity msg " ")))
	(needkey (progn
		   (if (and (progn
			      (set-buffer statusbuf)
			      (goto-char (point-min))
			      (re-search-forward "^\\[GNUPG:\\]\\s +ERRSIG$" 
						 nil t))
			    (progn
			      (set-buffer stderrbuf)
			      (goto-char (point-min))
			      (re-search-forward 
			       "gpg: Can't check signature: Public key not found$"
			       nil t))
			    )
		       (progn
			 (set-buffer stderrbuf)
			 (goto-char (point-min))
			 (re-search-forward
			  "key ID \\(\\S +\\)$" nil t)
			 (match-string 1)))))
	)
    (mc-gpg-debug-print (format " status is %s" status))
    (cond 
     ((= rc 0)
      ;; good signature. stderr has info about the signature
      ;; status has GOODSIG and a keyword with trust info
      ;; return GOODSIG TRUST_FULLY from "keyid"
      (list nil 
	    'good 
	    (progn
	      (set-buffer stderrbuf)
	      (goto-char (point-min))
	      (if (re-search-forward "^gpg: Good signature \\(from.*\\)$"
				     nil t)
		  (setq status (concat status " " (match-string 1))))
	      status)))
     ((= rc 1)
      ;; bad signature
      (list nil 'bad status))
     (needkey
      (list nil 'needkey needkey))
     (t ;corrupted message?
      (error (with-current-buffer stderrbuf (buffer-string))))
     )
    ))


;gpg: Signature made Wed Aug  5 17:47:07 1998 using DSA key ID C63B6750
;gpg: Good signature from "Brian Warner (temporary GPG key) <warner@lothar.com>"

(defun mc-gpg-verify-region (start end &optional no-fetch)
  (let ((buffer (get-buffer-create mc-buffer-name))
	(obuf (current-buffer))
	args gpg-id result)
    (setq args '("--batch" "--verify"))
    (if mc-gpg-alternate-keyring
	(setq args (append "--keyring" mc-gpg-alternate-keyring args)))
    (message "Verifying...")
    (setq result (mc-gpg-process-region
		  start end nil mc-gpg-path args 'mc-gpg-verify-parser buffer))
    (mc-gpg-debug-print (format "process-region returned %s" result))
    (cond 
     ((eq (nth 0 result) 'good)
      (message (nth 1 result)))
     ((eq (nth 0 result) 'bad)
      (error (nth 1 result)))
     ((eq (nth 0 result) 'needkey)
      (if (and
	   (not no-fetch)
	   (setq gpg-id
		 (concat "0x" (nth 1 result)))
	   (not (eq mc-gpg-always-fetch 'never))
	   (or mc-gpg-always-fetch
	       (y-or-n-p
		(format "Key %s not found; attempt to fetch? " gpg-id)))
	   (mc-gpg-fetch-key (cons nil gpg-id))
	   (set-buffer obuf))
	  (mc-gpg-verify-region start end t)
	(error "Can't check signature: Public key 0x%s not found" (nth 1 result))))
     (t
      (error (nth 1 result))))
    ))


(defun mc-gpg-insert-public-key (&optional id)
  (let ((buffer (get-buffer-create mc-buffer-name))
	args result)
    (setq id (or id mc-gpg-user-id))
    (setq args (list "--export" "--armor" "--batch" (concat "\"" id "\"")))
    (if mc-gpg-comment
	(setq args (append (list "--comment" (format "'%s'" mc-gpg-comment))
			   args)))
    (if mc-gpg-alternate-keyring
	(setq args (append (list "--keyring" mc-gpg-alternate-keyring) args)))

    (setq result (mc-gpg-process-region (point) (point) nil mc-gpg-path
					args 'mc-gpg-insert-parser buffer))
    (if (car result)
	(message (format "Key for user ID: %s" id))
      (message "failed: %s" (nth 2 result)))
    (car result)
))

;; return convention: '(newkeys oldkeys weirdos). error with stderr if rc != 0
(defun mc-gpg-snarf-parser (stdoutbuf stderrbuf statusbuf rc)
  (if (eq rc 0)
      (let ((newkeys 0) (oldkeys 0) (weirdos 0) tmpstr)
	(save-excursion
	  (set-buffer stderrbuf)
	  (goto-char (point-min))
	  (while (re-search-forward mc-gpg-newkey-re nil t)
	    (progn
	      (setq tmpstr (buffer-substring-no-properties
			    (match-beginning 2) (match-end 2)))
	      (cond ((equal tmpstr "public key imported") 
		     (setq newkeys (1+ newkeys)))
		    ((equal tmpstr "not changed")
		     (setq oldkeys (1+ oldkeys)))
		    (t
		     (setq weirdos (1+ weirdos))))))
	  (list nil newkeys oldkeys weirdos)
	))
    (error (with-current-buffer stderrbuf (buffer-string))))
)

(defun mc-gpg-snarf-keys (start end)
  ;; Returns number of keys found.
  (let ((buffer (get-buffer-create mc-buffer-name))
	results args)
    (setq args '("--import" "--batch"))
    (if mc-gpg-alternate-keyring
	(setq args (append args (list "--keyring" mc-gpg-alternate-keyring))))
    (message "Snarfing...")
    (setq results (mc-gpg-process-region start end nil mc-gpg-path args
					 'mc-gpg-snarf-parser buffer))
    (message (format "%d new keys, %d old, %d weird" 
		     (nth 0 results) (nth 1 results) (nth 2 results)))
    ;; might need to do a 'gpgm --check-trustdb' now
    (nth 0 results) ;(+ news olds weirds)
    ))

(defun mc-scheme-gpg ()
  (list
   (cons 'encryption-func 		'mc-gpg-encrypt-region)
   (cons 'decryption-func		'mc-gpg-decrypt-region)
   (cons 'signing-func			'mc-gpg-sign-region)
   (cons 'verification-func 		'mc-gpg-verify-region)
   (cons 'key-insertion-func 		'mc-gpg-insert-public-key)
   (cons 'snarf-func			'mc-gpg-snarf-keys)
   (cons 'msg-begin-line 		mc-gpg-msg-begin-line)
   (cons 'msg-end-line 			mc-gpg-msg-end-line)
   (cons 'signed-begin-line 		mc-gpg-signed-begin-line)
   (cons 'signed-end-line 		mc-gpg-signed-end-line)
   (cons 'key-begin-line 		mc-gpg-key-begin-line)
   (cons 'key-end-line 			mc-gpg-key-end-line)
   (cons 'user-id			mc-gpg-user-id)))

;;{{{ Key fetching

(defvar mc-gpg-always-fetch 'never
  "*If t, always attempt to fetch missing keys, or never fetch if
'never.")

(defun mc-gpg-fetch-key (&optional id)
  "Attempt to fetch a key for addition to GPG keyring.  Interactively,
prompt for string matching key to fetch.

This function is not yet implemented. The GPG documentation suggests a simple
keyserver protocol, but as far as I know it has not yet been implemented
anywhere."

  (error "Key fetching not yet implemented"))

;;}}}
