
(setq load-path (append '("..") load-path))
(load-library "mailcrypt")
(load-library "mc-toplev")
(load-library "mc-gpg")
(load-library "mc-remail")
(setq mc-gpg-extra-args '("--homedir" "remkeys"))
(setq mc-levien-file-name "remkeys/rlist.txt")
(setq mc-remailer-user-chains '( ("123" ["rem1" "rem2" "rem3"])))
(setq mc-test-remailer-unwind-program "unwind.py")

(defvar mc-test-verbose nil)

(defun mc-test-generate-plaintext (long)
  (if long
      "This is a test message."
    "This is a long test message."
    ))

;; to run an automated test of the remailer, we need to do the following:
;;  generate a plaintext message, to a given recipient, in a Message buffer
;;  pick a remailer chain
;;  encrypt the message to that chain
;;  hand the plaintext, recipient, chain, and crypttext to ./unwind.py
;;  verify that unwind.py exited successfully
;; The remailer chain can pick from three fake remailers: rem[123]@test.test


(defun mc-test-encrypt-remailer (chain)
  (let* ((b (get-buffer-create "mc plaintext"))
         (recipients '("user@test.test"))
         (scheme 'mc-scheme-gpg)
         (mc-pgp-always-sign 'never)
         (mc-gpg-extra-args '("--homedir" "remkeys"))
         (mc-levien-file-name "remkeys/rlist.txt")
         (mc-remailer-user-chains '( ("123" ["rem1" "rem2" "rem3"])))
         chains chain start end
	)
    (set-buffer b)
    (erase-buffer)
    (insert "To: user@test.test\n")
    (insert "Subject: test subject\n")
    (insert "--text follows this line--\n")
    (insert (mc-test-generate-plaintext nil))
    (mail-mode)

    (mc-reread-levien-file)
    (setq chains (mc-remailer-make-chains-alist))
    (setq chain (mc-remailer-canonicalize-chain
                 (cdr (assoc "123" chains)) chains))

    ;(mc-remailer-encrypt-for-chain)
    (mc-rewrite-for-chain chain)
    ;; crypttext is now in "mc plaintext" buffer, after --text follows etc--
    ;; chain is '("<rem1@test.test>" "<rem2@test.test>" "<rem3@test.test>").
    ;; recip is user@test.test.
    (message "chain is %s" (mc-unparse-chain chain))

    ;; find message body
    (goto-char (point-min))
    (re-search-forward
     (concat "^" (regexp-quote mail-header-separator) "\n"))
    (setq start (point))
    (setq end (point-max))

    ;; todo: spawn unwind.py, pipe crypttext to stdin
    (setq rc 
          (call-process-region start end shell-file-name 
                               nil  ; DELETE: don't delete text as it is sent
                               nil  ; DESTINATION: throw out process' stdout
                               nil  ; DISPLAY: don't update display
                               ;; args
                               "-c"
                               mc-test-remailer-unwind-program
                               "user@test.test" ; recipient
                               "rem3@test.test,rem2@test.test,rem1@test.test,rem1@test.test"         ; chainstring
                               (shell-quote-argument "test subject") ; subject
                               ))
    (message "rc is %d" rc)
    )
  )

(defun run-one-test ()
  ; it would be nice to take the test name from argv. see (command-line-args)
  (setq mc-test-verbose t)
  (mc-test-encrypt-remailer "123")
)

(defun run-all-tests ()
  (let (cases)
        
    (setq cases (append '("123") cases))
    (dolist (onecase cases)
      (mc-test-encrypt-remailer onecase))
))
