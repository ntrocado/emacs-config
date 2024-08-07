(use-package notmuch
  :ensure t
  :pin manual
  :load-path "/usr/share/emacs/site-lisp/"
  :init (setq notmuch-search-oldest-first nil
	      sendmail-program "~/.cargo/bin/mujmap"
	      message-send-mail-function #'message-send-mail-with-sendmail
	      message-sendmail-extra-arguments '("-C" "home/trocado/Mail/account.fastmail" "send")
	      notmuch-fcc-dirs nil))

;;; Keys to archive and delete

(define-key notmuch-show-mode-map "h"
	    (lambda ()
              "Archive messages by removing the inbox tag"
              (interactive)
              (notmuch-show-tag (list "-inbox"))))

(define-key notmuch-search-mode-map "h"
	    (lambda (&optional beg end)
              "Archive thread by removing the inbox tag"
              (interactive)
              (notmuch-search-tag (list "-inbox") beg end)))

(define-key notmuch-show-mode-map "d"
	    (lambda ()
              "mark messages for deletion by adding the trash tag"
              (interactive)
              (notmuch-show-tag (list "+deleted" "-inbox"))))

(define-key notmuch-search-mode-map "d"
	    (lambda (&optional beg end)
              "mark thread for deletion by adding the trash tag"
              (interactive)
              (notmuch-search-tag (list "+deleted" "-inbox") beg end)))

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
