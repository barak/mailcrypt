;; mc-pgp2.el, Support for PGP version 2.6.* in Mailcrypt
;; Copyright (C) 1995  Jin Choi <jin@atype.com>
;;                     Patrick LoPresti <patl@lcs.mit.edu>
;;           (C) 1998  Len Budney <lbudney@pobox.com>

;; These functions are derived from Mailcrypt version 3.4, and haven't
;; been touched at all by me (hardly).  They have been renamed from
;; their originals by substituting "pgp2" for "pgp" everywhere.

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
(defun mc-pgp2-process-region 
  (beg end passwd program args parser &optional buffer)
  (let ((obuf (current-buffer))
	(process-connection-type nil)
	mybuf result rgn proc)
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
	      (progn
		(process-send-string proc (concat passwd "\n"))
		(or mc-passwd-timeout (mc-deactivate-passwd t))))
	  (process-send-region proc beg end)
	  (process-send-eof proc)
	  (while (eq 'run (process-status proc))
	    (accept-process-output proc 5))
	  (setq result (process-exit-status proc))
	  ;; Hack to force a status_notify() in Emacs 19.29
	  (delete-process proc)
	  (set-buffer mybuf)
	  (goto-char (point-max))
	  (if (re-search-backward "\nProcess \\*PGP.*\n\\'" nil t)
	      (delete-region (match-beginning 0) (match-end 0)))
	  (goto-char (point-min))
	  ;; CRNL -> NL
	  (while (search-forward "\r\n" nil t)
	    (replace-match "\n"))
	  ;; Hurm.  FIXME; must get better result codes.
	  (if (stringp result)
	      (error "%s exited abnormally: '%s'" program result)
	    (setq rgn (funcall parser result))
	    ;; If the parser found something, migrate it
	    (if (consp rgn)
		(progn
		  (set-buffer obuf)
		  (delete-region beg end)
		  (goto-char beg)
		  (insert-buffer-substring mybuf (car rgn) (cdr rgn))
		  (set-buffer mybuf)
		  (delete-region (car rgn) (cdr rgn)))))
	  ;; Return nil on failure and exit code on success
	  (if rgn result))
      ;; Cleanup even on nonlocal exit
      (if (and proc (eq 'run (process-status proc)))
	  (interrupt-process proc))
      (set-buffer obuf)
      (or buffer (null mybuf) (kill-buffer mybuf)))))

;;}}}

(defun mc-pgp2-generic-parser (result)
  (let (start)
    (goto-char (point-min))
    (cond ((not (eq result 0))
	   (prog1
	       nil
	     (if (mc-message "^\aError: +Bad pass phrase\\.$" (current-buffer))
		 (mc-deactivate-passwd t)
	       (mc-message mc-pgp-error-re (current-buffer)
			   (format "PGP exited with status %d" result)))))
	  ((re-search-forward mc-pgp-nokey-re nil t)
	   nil)
	  (t
	   (and
	    (goto-char (point-min))
	    (re-search-forward "-----BEGIN PGP.*-----$" nil t)
	    (setq start (match-beginning 0))
	    (goto-char (point-max))
	    (re-search-backward "^-----END PGP.*-----\n" nil t)
	    (cons start (match-end 0)))))))

(defun mc-pgp2-decrypt-parser (result)
  (goto-char (point-min))
  (cond ((eq result 0)
	 ;; Valid signature
	 (re-search-forward "^Signature made.*\n")
	 (if (looking-at
	      "\a\nWARNING:  Because this public key.*\n.*\n.*\n")
	     (goto-char (match-end 0)))
	 (cons (point) (point-max)))
	((eq result 1)
	 (re-search-forward
	  "\\(\\(^File is conven.*\\)?Just a moment\\.+\\)\\|\\(^\\.\\)")
	 (if (eq (match-beginning 2) (match-end 2))
	     (if (looking-at
		  "\nFile has signature.*\\(\n\a.*\n\\)*\nWARNING:.*\n")
		 (goto-char (match-end 0)))
	   (if (looking-at "Pass phrase appears good\\. \\.")
	       (goto-char (match-end 0))))
	 (cons (point) (point-max)))
	(t nil)))

(defun mc-pgp2-verify-parser (result)
  (cond ((eq result 0)
	 (mc-message mc-pgp-sigok-re (current-buffer) "Good signature")
	 t)
	((eq result 1)
	 (mc-message mc-pgp-error-re (current-buffer) "Bad signature")
	 nil)
	(t
	 (mc-message mc-pgp-error-re (current-buffer)
		     (format "PGP exited with status %d" result))
	 nil)))

(defun mc-pgp2-snarf-parser (result)
  (eq result 0))
