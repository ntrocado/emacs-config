(use-package notmuch
  :ensure t
  :pin manual
  :load-path "/usr/share/emacs/site-lisp/"
  :init (setq notmuch-search-oldest-first nil
	      sendmail-program "~/.cargo/bin/mujmap"
	      message-send-mail-function #'message-send-mail-with-sendmail
	      message-sendmail-extra-arguments '("-C" "home/trocado/Mail/account.fastmail" "send")
	      notmuch-fcc-dirs nil
	      notmuch-show-logo nil))

(use-package consult-notmuch
  :after (consult notmuch)
  :ensure t
  :bind ("C-c C-n" . consult-notmuch))

(use-package ol-notmuch
  :after (org notmuch)
  :ensure t
  :bind ("C-c l" . org-store-link))


;;; notmuch-hello refresh status message

(defvar notmuch-hello-refresh-count 0)

(defun notmuch-hello-refresh-status-message ()
  (let* ((new-count
          (string-to-number
           (car (process-lines notmuch-command "count"))))
         (diff-count (- new-count notmuch-hello-refresh-count)))
    (cond
     ((= notmuch-hello-refresh-count 0)
      (message "You have %s messages."
               (notmuch-hello-nice-number new-count)))
     ((> diff-count 0)
      (message "You have %s more messages since last refresh."
               (notmuch-hello-nice-number diff-count)))
     ((< diff-count 0)
      (message "You have %s fewer messages since last refresh."
               (notmuch-hello-nice-number (- diff-count)))))
    (setq notmuch-hello-refresh-count new-count)))

(add-hook 'notmuch-hello-refresh-hook 'notmuch-hello-refresh-status-message)

;;; Attach files from dired
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)
