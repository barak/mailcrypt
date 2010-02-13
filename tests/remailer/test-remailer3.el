;;
;; brief automated tests of type2/3 remailer functions


(setq load-path (append '("../..") load-path))
(load-library "mailcrypt")
(load-library "mc-toplev")
(load-library "mc-remail2")

(defun remailer-test-1-mixmaster ()
  (let (
        (mc-mixmaster29-path (expand-file-name "./fakemix.sh"))
        (mc-mixmaster29-ask nil)
        (mc-mixminion-path "/bin/false")
        (mc-default-remailer-scheme 'mc-remailer-scheme-mixmaster)
        (plaintext "This is an anonymous message\n")
        b crypttext)
    (setq b (get-buffer-create "mc plaintext"))
    (set-buffer b)
    (erase-buffer)

    (insert plaintext)
    (setq crypttext (concat "fakemix --mail --to bob@anonocorp\n"
                            plaintext
                            "\nMessage sent to 'bob@anonocorp' through the Mixmaster network\n"))

    ;; do it
    (mc-remail-generic "bob@anonocorp")

    ;; the fakemix.sh script just echoes the arguments on one line and then
    ;; copies stdin to stdout. Therefore the encrypted region should wind up
    ;; with with the arguments line prepended to it

    (when (not (equal (buffer-string) crypttext))
        (message "expected '%s'" crypttext)
        (message "got '%s'" (buffer-string))
        (error "remailer-test-1-mixmaster failed"))

    (message "test-1-mixmaster passed")
))

(defun remailer-test-2-mixminion ()
  (let (
        (mc-mixminion-path (expand-file-name "./fakemix.sh"))
        (mc-mixminion-ask nil)
        (mc-mixmaster29-path "/bin/false")
        (mc-default-remailer-scheme 'mc-remailer-scheme-mixminion)
        (plaintext "This is an anonymous message\n")
        b crypttext)
    (setq b (get-buffer-create "mc plaintext"))
    (set-buffer b)
    (erase-buffer)

    (insert plaintext)
    (setq crypttext (concat "Running mixminion as follows:\n"
                            (format " %s (send --to bob@anonocorp)\n"
                                    mc-mixminion-path)
                            "\n"
                            "--- Command output begins ---\n"
                            "fakemix send --to bob@anonocorp\n"
                            plaintext
                            "--- Command output ends ---\n"
                            "\n\n"
                            "Message sent to 'bob@anonocorp' through the Mixminion network\n"
                            "This buffer may now be deleted.\n"
                            ))

    ;; do it
    (mc-remail-generic "bob@anonocorp")

    (when (not (equal (buffer-string) crypttext))
        (message "expected '%s'" crypttext)
        (message "got '%s'" (buffer-string))
        (error "remailer-test-2-mixminion failed"))

    (message "test-2-mixminion passed")
))


(defun run-all-tests ()
  ;;(remailer-test-1-mixmaster)
  (remailer-test-2-mixminion)
)
