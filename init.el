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
      sentence-end-double-space nil)

;;; Backups
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

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
		sly-lisp-implementations '((sbcl ("sbcl")))))

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
  :hook (after-init . global-company-mode))

(use-package magit
  :ensure t)

(use-package org
  :ensure org-plus-contrib
  :pin org
  :custom (org-startup-indented t))

(use-package org-roam
  :ensure t
  :hook
  (after-init . org-roam-mode)
  :config
  ;; open org-roam buffer after org-roam-find-file
  (add-hook 'find-file-hook
    (defun +org-roam-open-buffer-maybe-h ()
      (and (memq 'org-roam-buffer--update-maybe post-command-hook)
           (not (window-parameter nil 'window-side)) ; don't proc for popups
           (not (eq 'visible (org-roam-buffer--visibility)))
           (with-current-buffer (window-buffer)
             (org-roam-buffer--get-create)))))
  :custom
  (org-roam-directory "/mnt/c/Users/trocado/OneDrive/Roam")
  (org-roam-completion-system 'ivy)
  :bind (:map org-roam-mode-map
	      (("C-c n l" . org-roam)
	       ("C-c n f" . org-roam-find-file)
	       ("C-c n g" . org-roam-show-graph))
	      :map org-mode-map
	      (("C-c n i" . org-roam-insert))))

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

;;; Other config files
(load-file (expand-file-name "notmuch-config.el" user-emacs-directory))

;;; Custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
