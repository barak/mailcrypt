;-*-emacs-lisp-*-

;; mailcrypt-init.el, perform mailcrypt initialization
;; Copyright (C) 1996 Joe Reinhardt <joe-reinhardt@uiowa.edu>
;; Copyright © Davide G. M. Salvetti <salve@debian.org>, 1998, 1999, 2002.

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

;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
;;}}}

(provide 'mailcrypt-init)

;;; Configuration

;; Autoloads as per README
(autoload 'mc-install-write-mode "mailcrypt" "Part of MailCrypt" t)
(autoload 'mc-install-read-mode "mailcrypt" "Part of MailCrypt" t)
(autoload 'mc-setversion "mailcrypt" "Part of MailCrypt" t)

;; mail-mode hook
(add-hook 'mail-mode-hook 'mc-install-write-mode)

;; Rmail hooks
(if (and (boundp 'emacs-version)
	 (>= emacs-major-version 20)
	 (>= emacs-minor-version 3))
    (add-hook 'rmail-show-message-hook 'mc-install-read-mode)
  (add-hook 'rmail-mode-hook 'mc-install-read-mode))
(add-hook 'rmail-summary-mode-hook 'mc-install-read-mode)

;; VM hooks
(add-hook 'vm-mode-hook 'mc-install-read-mode)
(add-hook 'vm-summary-mode-hook 'mc-install-read-mode)
(add-hook 'vm-virtual-mode-hook 'mc-install-read-mode)
(add-hook 'vm-mail-mode-hook 'mc-install-write-mode)

;; MH-E hooks
(add-hook 'mh-folder-mode-hook 'mc-install-read-mode)
(add-hook 'mh-letter-mode-hook 'mc-install-write-mode)

;; Gnus hooks
(add-hook 'gnus-summary-mode-hook 'mc-install-read-mode)
(add-hook 'message-setup-hook 'mc-install-write-mode)
(add-hook 'news-reply-mode-hook 'mc-install-write-mode)

;; Mew hooks
(add-hook 'mew-message-mode-hook 'mc-install-read-mode)
(add-hook 'mew-summary-mode-hook 'mc-install-read-mode)
(add-hook 'mew-draft-mode-hook 'mc-install-write-mode)

;; load path
(setq load-path (cons (concat "/usr/share/"
			      (symbol-name debian-emacs-flavor)
			      "/site-lisp/mailcrypt") load-path))

;; GNU Privacy Guard is our default.
(setq mc-default-scheme 'mc-scheme-gpg)

;;; End
