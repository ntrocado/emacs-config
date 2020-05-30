(use-package bbdb
  :ensure t)

(use-package notmuch
  :ensure t
  :config (setq notmuch-search-oldest-first nil))

; configure outgoing SMTP
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-auth-credentials (expand-file-name "~/.authinfo")
      smtpmail-debug-info t)

(setq message-kill-buffer-on-exit t
      notmuch-fcc-dirs nil)

;;; Send e-mail without inserting newlines
(add-hook 'message-mode-hook
	  (lambda ()
	    (turn-off-auto-fill)
	    (visual-line-mode)
	    (setq mml-enable-flowed nil)))

;;; Keys to archive and delete

(define-key notmuch-show-mode-map "a"
      (lambda ()
        "archive messages by removing the inbox tag"
        (interactive)
        (notmuch-show-tag (list "-inbox"))))

(define-key notmuch-search-mode-map "a"
      (lambda (&optional beg end)
        "archive thread by removing the inbox tag"
        (interactive)
        (notmuch-search-tag (list "-inbox") beg end)))

(define-key notmuch-show-mode-map "d"
      (lambda ()
        "mark messages for deletion by adding the trash tag"
        (interactive)
        (notmuch-show-tag (list "+trash" "-inbox"))))

(define-key notmuch-search-mode-map "d"
      (lambda (&optional beg end)
        "mark thread for deletion by adding the trash tag"
        (interactive)
        (notmuch-search-tag (list "+trash" "-inbox") beg end)))

;;; notmuch-hello refresh status message

(defvar notmuch-hello-refresh-count 0)

(defun notmuch-hello-refresh-status-message ()
  (unless no-display
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
      (setq notmuch-hello-refresh-count new-count))))

(add-hook 'notmuch-hello-refresh-hook 'notmuch-hello-refresh-status-message)
