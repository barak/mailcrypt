;; mc-versions.el, Support for multiple versions of PGP.
;; Copyright (C) 1998  Len Budney <lbudney@pobox.com>

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

(defun mc-setversion (&optional version)
  "Reset path and argument information for the selected version of PGP.
Possible values of VERSION are 2.6 and 5.0."
  (interactive)

  (let ((called-interactively nil))
  (if (null version)
      (progn
	(setq called-interactively t)
	(setq version (read-string "Select PGP version: "))))

  (if (string-equal version "5.0")
      (progn
	(setq mc-pgp-version-five t)
	(setq mc-pgp-path-encrypt "pgpe.sh")
	(setq mc-pgp-path-decrypt "pgpv.sh")
	(setq mc-pgp-path-sign    "pgps.sh")
	(setq mc-pgp-path-verify  "pgpv.sh")
	(setq mc-pgp-path-getkeys "mc-pgpk")
	(setq mc-pgp-key-cache nil)

	;; Arguments to PGP for various actions.
	(setq mc-pgp-encrypt-args
	      (list "+NoBatchInvalidKeys=off" "-fat" "+batchmode=1"))
	(setq mc-pgp-sign-arg "-fat")

	;; Aliases to call the correct functions for this version.
	(defalias 'mc-process-region     'mc-pgp5-process-region)
	(defalias 'mc-pgp-decrypt-parser 'mc-pgp5-decrypt-parser)
	(defalias 'mc-pgp-encrypt-parser 'mc-pgp5-encrypt-parser)
	(defalias 'mc-pgp-verify-parser  'mc-pgp5-verify-parser )
	(defalias 'mc-pgp-sign-parser    'mc-pgp5-sign-parser   )
	(message "PGP version set to 5.0."))
    (if (string-equal version "2.6")
	(progn
	  (setq mc-pgp-version-five nil)
	  (setq mc-pgp-path-encrypt (concat mc-pgp2-path "pgp"))
	  (setq mc-pgp-path-decrypt (concat mc-pgp2-path "pgp"))
	  (setq mc-pgp-path-sign    (concat mc-pgp2-path "pgp"))
	  (setq mc-pgp-path-verify  (concat mc-pgp2-path "pgp"))
	  (setq mc-pgp-path-getkeys (concat mc-pgp2-path "pgp"))
	  (setq mc-pgp-path         (concat mc-pgp2-path "pgp"))
	  (setq mc-pgp-key-cache nil)

	  ;; Arguments to PGP for various actions.
	  (setq mc-pgp-encrypt-args
		(list "+encrypttoself=off +verbose=1" "+batchmode"
		      "+language=en" "-fat"))
	  (setq mc-pgp-sign-arg "-fast")

	  ;; Aliases to call the correct functions for this version.
	  (defalias 'mc-process-region     'mc-pgp2-process-region)
	  (defalias 'mc-pgp-decrypt-parser 'mc-pgp2-decrypt-parser)
	  (defalias 'mc-pgp-encrypt-parser 'mc-pgp2-generic-parser)
	  (defalias 'mc-pgp-verify-parser  'mc-pgp2-verify-parser )
	  (defalias 'mc-pgp-sign-parser    'mc-pgp2-generic-parser)
	  (defalias 'mc-pgp-generic-parser 'mc-pgp2-generic-parser)
	  (message "PGP version set to 2.6."))
      (if called-interactively
	  (message "You must specify a version; 5.0 or 2.6"))))))
