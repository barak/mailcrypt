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
	(setq version 
	      (completing-read 
	       "Select PGP version: " mc-schemes nil t))))

  (if (string-equal version "pgp50")
      (progn
	(setq mc-default-scheme 'mc-scheme-pgp50)
	(message "PGP version set to 5.0."))
    (if (string-equal version "pgp")
	(progn
	  (setq mc-default-scheme 'mc-scheme-pgp)
	  (message "PGP version set to 2.6."))
      (if called-interactively
	  (message "You must specify a version; 5.0 or 2.6"))))
))
