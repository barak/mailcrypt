;; mc-pgp5.el, Support for PGP version 2.6.* in Mailcrypt
;;           (C) 1998  Len Budney <lbudney@pobox.com>

;; These functions have been added to Mailcrypt version 3.4, to
;; support PGP version 5.0.  They depend on the "expect.el" functions
;; kindly provided by Lars Magne Ingebrigtsen <[22]lmi@gnus.org> to
;; interact with PGP, since batchmode support is sketchy or broken.

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


;; {{{ Modified mc-process-region uses "expect.el" to interact with PGP.
;; This is necessary because PGP5.0 is fairly broken in its batch
;; mode, and so *MUST* interact with the user under various error
;; conditions.
;;
;; More particularly, the old mailcrypt ran PGP in batch mode, then
;; exposed the results to a regexp-based parser.  This is not possible
;; with PGP5.0 or PGP5.0i, since pgp* does not exit in the case of
;; errors, but rather reprompts the user.  Our solution is to hand the
;; PROCESS off to a parser, which subsumes some of the functionality
;; which used to be in mc-process-region.
;;
(require 'expect)

(defun mc-pgp5-process-region 
  (beg end passwd program args parser &optional buffer)
  (let ((obuf (current-buffer))
	(process-connection-type nil)
	mybuf result rgn proc results)
    (unwind-protect
	(progn
	  (setq mybuf (or buffer (generate-new-buffer " *mailcrypt temp")))
	  (set-buffer mybuf)
	  (erase-buffer)
	  (set-buffer obuf)
	  (buffer-disable-undo mybuf)
	  (setq proc
		(apply 'start-process "*PGP*" mybuf program args))
	  (if passwd
	      (or mc-passwd-timeout (mc-deactivate-passwd t)))

	  ;; Now hand the process to the parser, which returns the exit
	  ;; status of the dead process and the limits of the region
	  ;; containing the PGP results.
	  (setq results (funcall parser proc obuf beg end mybuf passwd))
	  (setq result  (car results))
	  (setq rgn     (cdr results))

	  ;; Hack to force a status_notify() in Emacs 19.29
	  (set-buffer mybuf)

	  ;; Hurm.  FIXME; must get better result codes.
	  (if (stringp result)
	      (error "%s exited with message: '%s'" program result)

	    ;; If the parser found something, migrate it to the old
	    ;; buffer.  In particular, the parser's job is to return
	    ;; a cons of the form ( beg . end ) delimited the result
	    ;; of PGP in the new buffer.
	    (if (consp rgn)
		(progn
		  (set-buffer obuf)
		  (delete-region beg end)
		  (goto-char beg)
		  (insert-buffer-substring mybuf (car rgn) (cdr rgn))
		  (set-buffer mybuf)
		  (delete-region (car rgn) (cdr rgn))
		  )))
	  ;; Return nil on failure and exit code on success
	  (if rgn result))
      ;; Cleanup even on nonlocal exit
      (if (and proc (eq 'run (process-status proc)))
	  (interrupt-process proc))
      (set-buffer obuf)
      (or buffer (null mybuf) (kill-buffer mybuf))
      )))

;;; }

(defun mc-pgp5-decrypt-parser (proc oldbuf start end newbuf passwd)
  (setenv "PGPPASSFD" 0)
  (set-buffer newbuf)
  (goto-char (point-max))
  (progn
    (unwind-protect
	(with-expect proc
	  (message "Sending passphrase...")
	  (expect-send (concat passwd "\n"))
	  (expect "No files specified.  Using stdin."
	    (message "Passphrase sent.  Decrypting...")
	    (set-buffer oldbuf)
	    (process-send-region proc start end)
	    (set-buffer newbuf)
	    (process-send-eof proc)

	    ;; Test output of the program, looking for
	    ;; errors.
	    (expect-cond

	     ;; OPTION 1:  Here comes the message!
	     ("Opening file \"stdout\" type .*\.\n"

	      ;; Record this point!
	      (setq rgn (point))
	      (message "Decryption complete.")

	      ;; Now wait out the process.
	      (while (eq 'run (process-status proc))
		(accept-process-output proc 5))
	      (setq result (process-exit-status proc))
	      (delete-process proc)

	      ;; Check whether there was a good signature made.
	      (goto-char (point-max))
	      (if
		  (re-search-backward 
		   "\n\nGood signature made.*by key:\n" nil t)
		  (setq rgn (cons rgn (match-beginning 0)))

		;; Note that we don't check for a BAD signature.  The
		;; effect is that the "BAD signature" message is
		;; embedded in the message itself.  Is it impolite to
		;; stick stuff into people's messages?  Well, forgery
		;; is impolite, too!
		(setq rgn (cons rgn (point-max))))

	      ;; Return the exit status, with the region
	      ;; limits!
	      (setq results (cons result rgn)))
			
	     ;; OPTION 2:  Awww...bad passphrase!
	     ("Enter pass phrase:" 
	      (mc-deactivate-passwd)
	      (interrupt-process proc)
	      (delete-process proc)

	      ;; Return the bad news.
	      (setq results 
		    '("Incorrect passphrase or wrong key" nil)))

	     ;; OPTION 3:  Not encrypted to us!
	     ("Cannot decrypt message.  It can only be decrypted by:"
	      (interrupt-process proc)
	      (delete-process proc)

	      ;; Return the bad news.
	      (setq results '("Message corrupt or not encrypted to you" nil)))

	     ;; OPTION 4:  The program exits.
	     (exit
	      (setq results (list 
			     (process-exit-status proc) nil))))))

      (pop-to-buffer obuf))
    results))

(defun mc-pgp5-verify-parser (proc oldbuf start end newbuf passwd)
  (setenv "PGPPASSFD")			; Delete this to exit batchmode!
  (set-buffer newbuf)
  (goto-char (point-max))
  (progn
    (unwind-protect
	(with-expect proc
	  (expect "No files specified.  Using stdin."
	    (message "Password sent.  Signing...")
	    (set-buffer oldbuf)
	    (process-send-region proc start end)
	    (set-buffer newbuf)
	    (process-send-eof proc)

	    ;; Test output of the program, looking for
	    ;; errors.
	    (expect-cond

	     ;; OPTION 1:  Great!  The signature is approved!
	     ("Good signature made.*by key:\n"

	      ;; Catch the exit status.
	      (delete-process proc)

	      ;; Return the good news!
	      (setq results '("Signature is valid." nil)))

	     ;; OPTION 2:  Shucks!  The signature is invalid!
	     ("BAD signature made.*by key:\n"

	      ;; Catch the exit status.
	      (delete-process proc)

	      ;; Return the good news!
	      (setq results 
		    '("Signature is *NOT* valid!  You've been had!" nil)))

	     ;; OPTION 2.5:  We don't have the right public key!
	     ("\nSignature by unknown keyid: 0x.*\n"

	      ;; Catch the exit status.
	      (delete-process proc)

	      ;; Return the good news!
	      (setq results 
		    '("You don't have this person's public key!" nil)))

	     ;; OPTION 3:  Awww...This isn't clearsigned, it's encrypted!
	     ("Enter pass phrase:" 
	      (interrupt-process proc)
	      (delete-process proc)

	      ;; Return the bad news.
	      (setq results '("Decrypt the message; that will verify it" nil)))

	     ;; OPTION 4:  The program exits.
	     (exit
	      (setq results (list 
			     (process-exit-status proc) nil))))))

      (pop-to-buffer obuf))
    results))

(defun mc-pgp5-encrypt-parser (proc oldbuf start end newbuf passwd)
  (set-buffer newbuf)
  (goto-char (point-max))
  (progn
    (unwind-protect
	(with-expect proc
	  (message "Encrypting message...")
	  (set-buffer oldbuf)
	  (process-send-region proc start end)
	  (set-buffer newbuf)
	  (process-send-eof proc)

	  ;; Test output of the program, looking for
	  ;; errors.
	  (expect-cond

	   ;; OPTION 1:  Great!  The data is now encrypted!
	   ("-----END PGP MESSAGE-----"

	    ;; Catch the exit status.
	    (setq result (process-exit-status proc))
	    (delete-process proc)
	    (message "Encryption complete.")

	    ;; Delete everything preceding the signed data.
	    (goto-char (point-max))
	    (re-search-backward 
	     "-----BEGIN PGP MESSAGE-----" nil t)
	    (delete-region (point-min) (match-beginning 0))
	    (setq rgn (point-min))

	    ;; Convert out CR/NL -> NL
	    (goto-char (point-min))
	    (while (search-forward "\r\n" nil t)
	      (replace-match "\n"))

	    ;; Delete everything after the signature.
	    (goto-char (point-min))
	    (re-search-forward
	     "-----END PGP MESSAGE-----" nil t)
	    (delete-region (match-end 0) (point-max))
			 
	    ;; Return the exit status, with the region
	    ;; limits!
	    (setq rgn (cons rgn (point-max)))
	    (setq results (cons result rgn)))

	   ;; OPTION 2:  Something is wrong!  We cannot sign and
	   ;; encrypt messages, since the batch mode in PGP5.0 is
	   ;; broken.  Holler!
	   ("Enter pass phrase:" 
	      (interrupt-process proc)
	      (delete-process proc)
	    ;; This should never happen.
	    (setq results '("Cannot sign and encrypt" nil)))

	   ;; OPTION 3:  There are keys missing.  Just bug out 
	   ;; of the whole thing, for now.
	   ("\nNo encryption keys found for:"
	      (interrupt-process proc)
	      (delete-process proc)
	    (setq results '("One or more public keys are missing" nil)))

	   ;; OPTION 4:  The program exits.
	   (exit
	    (setq results (list 
			   (process-exit-status proc) nil)))))
      (set-buffer obuf))
    results))

(defun mc-pgp5-sign-parser (proc oldbuf start end newbuf passwd)
  (setenv "PGPPASSFD" 0)
  (set-buffer newbuf)
  (goto-char (point-max))
  (progn
    (unwind-protect
	(with-expect proc
	  (message "Sending passphrase...")
	  (expect-send (concat passwd "\n"))
	  (expect "No files specified.  Using stdin."
	    (message "Passphrase sent.  Signing...")
	    (set-buffer oldbuf)
	    (process-send-region proc start end)
	    (set-buffer newbuf)
	    (process-send-eof proc)

	    ;; Test output of the program, looking for
	    ;; errors.
	    (expect-cond

	     ;; OPTION 1:  Great!  The data is now signed!
	     ("-----END PGP SIGNATURE-----"

	      ;; Catch the exit status.
	      (setq result (process-exit-status proc))
	      (delete-process proc)
	      (message "Signing complete.")

	      ;; Delete everything preceding the signed data.
	      (goto-char (point-max))
	      (re-search-backward 
	       "-----BEGIN PGP SIGNED MESSAGE-----" nil t)
	      (delete-region (point-min) (match-beginning 0))
	      (setq rgn (point-min))

	      ;; Convert out CR/NL -> NL
	      (goto-char (point-min))
	      (while (search-forward "\r\n" nil t)
		(replace-match "\n"))

	      ;; Delete everything after the signature.
	      (goto-char (point-min))
	      (re-search-forward
	       "-----END PGP SIGNATURE-----" nil t)
	      (delete-region (match-end 0) (point-max))
			 
	      ;; Return the exit status, with the region
	      ;; limits!
	      (setq rgn (cons rgn (point-max)))
	      (setq results (cons result rgn)))
			

	     ;; OPTION 2:  Awww...bad passphrase!
	     ("Enter pass phrase:" 
	      (mc-deactivate-passwd)
	      (interrupt-process proc)
	      (delete-process proc)

	      ;; Return the bad news.
	      (setq results '("Incorrect passphrase" nil)))

	     ;; OPTION 3:  The program exits.
	     (exit
	      (setq results (list 
			     (process-exit-status proc) nil))))))

      (pop-to-buffer obuf))
    results))
