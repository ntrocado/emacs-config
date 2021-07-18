(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(defun generic-path (path)
  (expand-file-name path (cond
			  ((eql system-type 'windows-nt) "c:/")
			  ((eql system-type 'gnu/linux) "/mnt/c/"))))

(defun lab-path (file)
  (expand-file-name file (generic-path "Users/trocado/OneDrive/lab/")))

(package-initialize)

(mapc (lambda (x)
	(add-to-list 'package-archives x))
      (list '("melpa" . "https://melpa.org/packages/")
	    '("org" . "https://orgmode.org/elpa/")))

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;;; Window size and position at startup
;; FIX: doesn't work all the time 
(when (display-graphic-p)
  (setq frame-resize-pixelwise t)
  (set-frame-position (selected-frame) 5 50)
  (set-frame-size (selected-frame) 100 40)
 ;(set-face-attribute 'default (selected-frame) :height 200)
)

;;; General defaults
(prefer-coding-system 'utf-8)
(tool-bar-mode 0)
(show-paren-mode 1)
(delete-selection-mode 1)
(global-visual-line-mode t)
(setq ring-bell-function 'ignore
      column-number-mode t
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      inhibit-startup-message t
      sentence-end-double-space nil
      enable-recursive-minibuffers t
      recentf-max-saved-items 50)

;;; Backups
(let ((backup-dir (concat user-emacs-directory "backups")))
  (setq backup-directory-alist (list (cons ".*" backup-dir))
	auto-save-file-name-transforms (list (list ".*" backup-dir t))))

;;; Global key bindings
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))
(global-set-key (kbd "<apps>") #'other-window)
(global-set-key (kbd "<menu>") #'other-window)

;;; Spell checking
(setq ispell-program-name "hunspell")


;;; Packages

(use-package counsel
  :ensure t
  :config
  (defun ivy-call-number (n)
    (interactive
     (list (let* ((type (event-basic-type last-command-event))
		  (char (if (characterp type)
			    ;; Number on the main row.
			    type
			  ;; Keypad number, if bound directly.
			  (car (last (string-to-list (symbol-name type))))))
		  (n (- char ?0)))
	     (if (zerop n) 10 n))))
    (ivy-set-index (1- n))
    (ivy--exhibit)
    (ivy-done))
  
  (dotimes (i 10)
    (define-key ivy-minibuffer-map (read-kbd-macro (format "M-%d" i)) 'ivy-call-number))
  
  (setq ivy-use-virtual-buffers t
	ivy-count-format "(%d/%d) ")
  (ivy-mode 1)
  (counsel-mode 1))

(use-package ivy-bibtex
  :ensure t
  :after ivy
  :config
  (setq bibtex-completion-bibliography (lab-path "master.bib")
	bibtex-completion-library-path (lab-path "pdf/")
	bibtex-completion-notes-path (lab-path "notes.org")
	bibtex-completion-display-formats '((t . "${author:36} ${title:*} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:7} ${keywords:15}"))
	bibtex-completion-pdf-symbol "#"
	bibtex-completion-notes-symbol "n")
  
  (setq bibtex-completion-pdf-open-function
	(if (eq system-type 'windows-nt)
	    (lambda (fpath)
	      (w32-shell-execute "open" fpath))
	  'org-open-file)))

;; (use-package ido
;;   :ensure t
;;   :config
;;   (setq ido-enable-flex-matching t
;; 	ido-everywhere t)
;;   (ido-mode t))

;; (use-package smex
;;   :ensure t
;;   :config (smex-initialize)
;;   :bind (("M-x" . smex)
;; 	 ("M-X" . smex-major-mode-commands)
;; 	 ("C-c C-c M-x" . execute-extended-command)))

(use-package sly
  :ensure t
  :config (setq sly-autodoc-mode t
		sly-lisp-implementations '((sbcl ("sbcl"))
					   (ccl ("~/ccl-dev/ccl/lx86cl64")))))

(use-package paredit
  :ensure t
  :config (enable-paredit-mode)
  :hook ((emacs-lisp-mode
	  eval-expression-minibuffer-setup
	  lisp-mode
	  lisp-interaction-mode
	  scheme-mode
	  sly-mrepl-mode)
	 . enable-paredit-mode))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config (setq company-dabbrev-downcase nil))

(use-package magit
  :ensure t)

(use-package org
  :ensure org-plus-contrib
  :pin org
  :custom (org-startup-indented t))

(use-package org-roam
  :ensure t
  :config
  (org-roam-setup)
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "/mnt/c/Users/trocado/OneDrive/Roam")
  (org-roam-completion-system 'ivy)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)))

(use-package org-ref
  :ensure t
  :preface (setq org-ref-completion-library 'org-ref-ivy-cite)
  :config
  (setq org-ref-bibliography-notes (lab-path "notes.org")
	org-ref-default-bibliography (list (lab-path "master.bib"))
	org-ref-pdf-directory (lab-path "pdf/")))

(use-package org-roam-bibtex
  :ensure t
  :after (org-roam org-ref ivy-bibtex)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :bind (:map org-mode-map
         (("C-c n a" . orb-note-actions))))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))

(add-hook 'LilyPond-mode-hook
	  #'(lambda ()
	      (hack-local-variables)))

;;; Other config files
(load-file (expand-file-name "notmuch-config.el" user-emacs-directory))

;;; Custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
