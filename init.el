;;; GENERAL DEFAULTS

(prefer-coding-system 'utf-8)
(tool-bar-mode 0)
(show-paren-mode 1)
(delete-selection-mode 1)
(global-visual-line-mode t)
(save-place-mode 1)
(pixel-scroll-precision-mode 1)
(setq ring-bell-function 'ignore
      column-number-mode t
      inhibit-startup-message t
      sentence-end-double-space nil
      enable-recursive-minibuffers t
      recentf-max-saved-items 50
      time-stamp-format "%Y-%02m-%02d %02H:%02M:%02S"
      large-file-warning-threshold 50000000)


;;; BACKUPS AND AUTO-SAVES

(let ((backup-dir (concat user-emacs-directory "backups")))
  (setq backup-directory-alist (list (cons ".*" backup-dir))
	delete-old-versions t
	kept-new-versions 6
	kept-old-versions 2
	version-control t
	backup-by-copying t))

(let ((save-files-directory
       (file-name-concat user-emacs-directory
                         "auto-save/")))
  (make-directory save-files-directory :parents)
  (setq auto-save-file-name-transforms
	`((".*" ,save-files-directory t))))


;;; GLOBAL KEY BINDINGS

(define-key key-translation-map (kbd "ESC") (kbd "C-g"))
(global-set-key (kbd "<apps>") #'other-window)
(global-set-key (kbd "<menu>") #'other-window)


;;; Set default font and hide scroll-bar

(defun font-exists-p (font) "check if font exists"
  (if (null (x-list-fonts font)) nil t))

(defun my/setup-frame (&optional frame)
  "Configure look of FRAME.
If FRAME is nil, configure current frame. If non-nil, make FRAME
current."
  (when frame (select-frame frame))
  (when window-system
    (cond ((font-exists-p "InputMono")
	   (set-face-attribute 'default nil :font "InputMono-11")
	   (set-face-attribute 'fixed-pitch nil :family "InputMono"))
	  ((font-exists-p "Roboto")
	   (set-face-attribute 'default nil :font "Roboto Mono")
	   (set-face-attribute 'variable-pitch nil :font "Roboto")
	   (set-face-attribute 'fixed-pitch nil :font "Roboto Mono")))
    (cond ((font-exists-p "-*-Source Sans 3-*")
	   (set-face-attribute 'variable-pitch nil :font "-outline-Source Sans 3-normal-normal-normal-sans-32-*-*-*-p-*-iso10646-1"))
	  ((font-exists-p "Noto Sans")
	   (set-face-attribute 'variable-pitch nil :family "Noto Sans")))
    (toggle-scroll-bar -1)))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'my/setup-frame)
  (my/setup-frame))

(add-hook 'text-mode-hook
               (lambda ()
                (variable-pitch-mode 1)))


;;; CUSTOM FILE

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


;;; CONVENIENCE FUNCTIONS

(defun my/remove-newlines-and-hyphens (begin end)
  (interactive "r")
  (let ((text (buffer-substring-no-properties begin end)))
    (kill-region begin end)
    (insert
     (replace-regexp-in-string "\n" " "
			       (replace-regexp-in-string "-\n" "" text)))))

(defun my/insert-time-stamp ()
  "Insert a time-stamp at point."
  (interactive)
  (when (org-check-for-org-mode) ; org files get a commented-out time-stamp
    (insert "# "))
  (insert "Time-stamp: <>\n"))


;;; MELPA

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


;;; PACKAGES

(use-package emacs
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend)
	modus-themes-headings
	'((1 . (rainbow overline background 1.1))
          (2 . (rainbow background 1))
          (3 . (rainbow bold 1))
          (t . (regular 1))))

  ;; Vertico recommended configurations
  
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  :config
  ;; Load the theme of your choice:
  (load-theme 'modus-operandi) ;; OR (load-theme 'modus-vivendi)
  :bind ("<f5>" . modus-themes-toggle)

  ;; Update timestamp before saving
  :hook (before-save . time-stamp))

(use-package diminish
  :ensure t
  :config
  (mapcar #'diminish '(eldoc-mode visual-line-mode)))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (vertico-indexed-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package recentf
  :init
  (recentf-mode))

(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; Example configuration for Consult
(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)	;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)		;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop) ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)	 ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("C-c M-s d" . consult-find) ;; Alternative: consult-fd
         ("C-c M-s c" . consult-locate)
         ("C-c M-s g" . consult-grep)
         ("C-c M-s G" . consult-git-grep)
         ("C-c M-s r" . consult-ripgrep)
         ("C-c M-s l" . consult-line)
         ("C-c M-s L" . consult-line-multi)
         ("C-c M-s k" . consult-keep-lines)
         ("C-c M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("C-c M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)	;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history) ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  (setq consult-preview-key '(:debounce 0.2 any))
  ;; (setq consult-preview-key "M-q")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file)

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
)

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; OR (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  ;(setq embark-prompter #'embark-completing-read-prompter)
  )

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (text-mode-ispell-word-completion nil)
  :init (global-corfu-mode)
  :config (keymap-unset corfu-map "RET"))

(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

(use-package ediff
  :config
  (setq ediff-split-window-function 'split-window-horizontally
	ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package citar
  :ensure t
  :init
  (setq org-cite-global-bibliography '("~/OneDrive/lab/master.bib"))
  :config
  (setq citar-library-paths '("~/OneDrive/lab/pdf/")
	citar-notes-paths '("~/OneDrive/lab/notes/"))
  (setf (alist-get 'note citar-templates) "${author}. (${year date:4}). ${title}")

  ;; adapted version of the default function citar-org-format-note-default
  (defun my/citar-org-format-note (key entry)
    "Format a note from KEY and ENTRY."
    (let* ((template (citar--get-template 'note))
           (note-meta (when template
			(citar-format--entry template entry)))
           (filepath (expand-file-name
                      (concat key ".org")
                      (car citar-notes-paths)))
           (buffer (find-file filepath)))
      (with-current-buffer buffer
	;; This just overrides other template insertion.
	(erase-buffer)
	(citar-org-roam-make-preamble key)
	(insert "#+title: ")
	(when template (insert note-meta))
	(insert "\n# Time-stamp: <>\n\n"))))
  (setq citar-note-format-function #'my/citar-org-format-note)
  
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (org-cite-csl-locales-dir "~/OneDrive/lab/csl")
  (citar-bibliography org-cite-global-bibliography)
  ;; optional: org-cite-insert is also bound to C-c C-x C-@
  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert)))

(use-package bibtex
  :ensure nil
  :config
  (setq bibtex-autokey-name-year-separator "_"
	bibtex-autokey-year-title-separator ""
	bibtex-autokey-year-length 4
	bibtex-autokey-titleword-length 0
	bibtex-autokey-titlewords 0
	bibtex-autokey-name-case-convert-function #'upcase-initials))

(use-package citar-embark
  :ensure t
  :after citar embark
  :diminish citar-embark-mode
  :config (citar-embark-mode))

(use-package abbrev
  :ensure nil
  :init
  (setq-default abbrev-mode t)
  :config
  (read-abbrev-file (expand-file-name "abbrev_defs" user-emacs-directory))
  (setq save-abbrevs 'silently))

(use-package dired
  :ensure nil
  :custom (dired-listing-switches "-lAhvtu")
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq ls-lisp-use-insert-directory-program nil
	dired-dwim-target t
	delete-by-moving-to-trash t))

(use-package async
  :ensure t
  :config (dired-async-mode 1))

(use-package titlecase
  :ensure t
  :custom (titlecase-style 'apa))

(use-package sly
  :ensure t
  :config
  (setq sly-autodoc-mode t
	sly-autodoc-use-multiline-p t
	inferior-lisp-program "sbcl"))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :hook ((emacs-lisp-mode
	  eval-expression-minibuffer-setup
	  lisp-mode
	  lisp-interaction-mode
	  scheme-mode
	  sly-mrepl-mode)
	 . enable-paredit-mode)
  :hook (sly-mrepl-mode . (lambda ()
			    (let ((oldmap (cdr (assoc 'paredit-mode minor-mode-map-alist)))
				  (newmap (make-sparse-keymap)))
			      (set-keymap-parent newmap oldmap)
			      (define-key newmap ["RET"] nil)
			      (make-local-variable 'minor-mode-overriding-map-alist)
			      (push `(paredit-mode . ,newmap) minor-mode-overriding-map-alist)))))

(use-package sly ; cl-collider and cl-patterns
  :config
  (defun sly-stop-sc ()
    (interactive)
    (sly-interactive-eval "(sc:stop)"))

  (defun sly-stop-patterns ()
    (interactive)
    (sly-interactive-eval "(cl-patterns:stop t)"))

  (defun cl-patterns-helpers-load ()
    (interactive)
    (sly-eval-async '(cl:let ((system (asdf:find-system "cl-patterns" nil)))
                       (cl:when system (cl:namestring (asdf:system-source-directory system))))
      (lambda (path)
        (load (concat path "res/emacs/cl-patterns-helpers") nil nil nil t)
        (load (concat path "res/emacs/cl-patterns-skeletons") nil nil nil t)))
    (define-key sly-mode-map (kbd "C-c p") 'cl-patterns-play-or-end-context-or-select-pdef)
    (define-key sly-mode-map (kbd "C-c P") 'cl-patterns-play-or-stop-context-or-select-pdef)
    (define-key sly-mode-map (kbd "C-c s") 'cl-patterns-stop-all)
    (define-key sly-doc-map (kbd "s") 'cl-patterns-supercollider-documentation))

  :bind (:map sly-mode-map
	      ("C-." . sly-stop-sc)
	      ("C-," . sly-stop-patterns))
  :hook
  (sly-connected . cl-patterns-helpers-load))


(use-package magit
  :ensure t)

(use-package circe
  :ensure t
  :init
  (defun my-fetch-password (&rest params)
    (require 'auth-source)
    (let ((match (car (apply 'auth-source-search params))))
      (if match
          (let ((secret (plist-get match :secret)))
            (if (functionp secret)
		(funcall secret)
              secret))
	(error "Password not found for %S" params))))

  (defun my-nickserv-password (server)
    (my-fetch-password :login "trocado" :host "irc.libera.chat"))
  
  :config
  (setq circe-network-options
      '(("irc.libera.chat"
	 :port 7000
         :tls t
         :nick "trocado"
	 :user "trocado"
	 :realname "trocado"
	 :sasl-username "trocado"
	 :sasl-password my-nickserv-password
         :channels (:after-auth "#lisp" "#commonlisp" "#dataflow"
				"#lispgames" "#supercollider" "#org-mode"
				"#clschool" "#emacs-circe" "#lilypond"
				"##running" "#org-roam" "##latin" "##latinitas")
	 :reduce-lurker-spam t))
      lui-scroll-behavior 'post-output)
  (require 'circe-color-nicks)
  (enable-circe-color-nicks)
  (setq circe-color-nicks-everywhere t))

(use-package powerthesaurus
  :ensure t
  :config
  ;; https://github.com/SavchenkoValeriy/emacs-powerthesaurus/issues/16
  (setq powerthesaurus-request-headers
	(delete '("Accept-Encoding" . "gzip, deflate, br")
		powerthesaurus-request-headers)))

(use-package pt
  :ensure t)

(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :config (golden-ratio-mode))

(use-package org
  :ensure t
  :pin gnu
  :custom
  (org-startup-indented t)
  (org-footnote-auto-adjust t)
  (org-src-preserve-indentation t)
  (org-edit-src-content-indentation 0)
  (calendar-week-start-day 1)
  (org-hide-emphasis-markers t)
  (org-list-allow-alphabetical t)
  (org-clock-mode-line-total 'current)
  :init
  (setq org-emphasis-regexp-components '("-[:space:]('\"{—"
					 "-[:space:].,:!?;'\")}\\[—"
					 "[:space:]"
					 "."
					 1))
  :config
  ;; https://list.orgmode.org/CAKPXLbtS=y_8LaT43zpkZeNrU7n4JNgYPKnws=0nPoDom1TroA@mail.gmail.com/
  (require 'ol-docview)

  (add-to-list 'org-link-frame-setup '(file . find-file)) ; open links in the same window
  (set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-document-title nil :height 1.5)
  
  ;; (setq org-priority-faces '((?A . (:foreground "red" :weight 'bold))
  ;;                            (?B . (:foreground "yellow"))
  ;;                            (?C . (:foreground "gray"))))

  ;; Latex export
  (setq org-latex-pdf-process (list "latexmk -pdfxe -f %f"))

  (setq org-latex-listings 'listings)
  (setq org-latex-listings-options
	'(("frame" "lines")
          ("basicstyle" "\\ttfamily\\scriptsize")
          ("numbers" "left")
          ("numberstyle" "\\tiny")))

  (setq org-latex-prefer-user-labels t)

  (setq org-latex-classes
	'(("article" "\\documentclass{scrartcl}"
	   ("\\section{%s}" . "\\section*{%s}")
	   ("\\subsection{%s}" . "\\subsection*{%s}")
	   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	   ("\\paragraph{%s}" . "\\paragraph*{%s}")
	   ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
	  ("report" "\\documentclass[11pt]{report}"
	   ("\\part{%s}" . "\\part*{%s}")
	   ("\\chapter{%s}" . "\\chapter*{%s}")
	   ("\\section{%s}" . "\\section*{%s}")
	   ("\\subsection{%s}" . "\\subsection*{%s}")
	   ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
	  ("book" "\\documentclass[11pt]{book}"
	   ("\\part{%s}" . "\\part*{%s}")
	   ("\\chapter{%s}" . "\\chapter*{%s}")
	   ("\\section{%s}" . "\\section*{%s}")
	   ("\\subsection{%s}" . "\\subsection*{%s}")
	   ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

  (setq org-export-with-smart-quotes t)

  ;; Blog export
  (setq org-html-htmlize-output-type 'css)

  (setq org-publish-project-alist
	'(("org-trocado"
           ;; Path to your org files.
           :base-directory "c:/Users/trocado/OneDrive/Documents/Practice-log/"
           :base-extension "org"

           ;; Path to your Jekyll project.
           :publishing-directory "c:/Users/trocado/Documents/ntrocado.github.io/"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           :html-extension "html"
           :body-only t) ;; Only export section between <body> </body>

	  ("org-static-trocado"
           :base-directory "c:/Users/trocado/OneDrive/Documents/Practice-log/"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php\\|svg"
           :publishing-directory "c:/Users/trocado/Documents/ntrocado.github.io/"
           :recursive t
           :publishing-function org-publish-attachment)

	  ("blog" :components ("org-trocado" "org-static-trocado"))))

  (defun org-custom-link-img-follow (path)
    (org-open-file-with-emacs
     (format "../assets/%s" path)))

  (defun org-custom-link-img-export (path desc format)
    (cond
     ((eq format 'html)
      (format "<img src=\"/assets/%s\" alt=\"%s\"/>" path desc))))

  ;; org-cite
  (defun my/org-ref-to-org-cite ()
  "Convert org-ref citations to org-cite format."
  (interactive)
  (let ((conversions '(("citet" . "/text")
		       ("nocite" . "/nocite")
		       ("citeyear" . "/noauthor")))) ;TODO: add others
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (rx "[["
				    (group (*? nonl) "cite" (*? nonl))
				    ":"
				    (group (+? nonl)) ;reference(s)
				    (? (group space (+? nonl))) ;post
				    "]]")
				nil t)
	(replace-match (format "[%s:%s%s]"
			       (concat "cite" (assoc-default (match-string 1) conversions))
			       (replace-regexp-in-string "&" "@" (match-string 2))
			       (or (match-string 3) ""))
		       nil nil))))))

(use-package org-roam
  :after org
  :preface (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/OneDrive/Roam")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode)

  (add-to-list 'display-buffer-alist
	       '("\\*org-roam\\*"
		 (display-buffer-in-direction)
		 (direction . right)
		 (window-width . 0.33)
		 (window-height . fit-window-to-buffer)))

  (setq org-roam-capture-templates
	'(("d" "default" plain "%?" :target
	   (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
		      "#+title: ${title}\n# Time-stamp: <>\n")
	   :unnarrowed t))))

;;; agenda
(use-package org
  :config
  (defun %heading-format ()
    (concat "[ " (org-format-outline-path (org-get-outline-path)) " ] "))
  
  (setq org-agenda-files '("~/OneDrive/Escritorio/notas.org"
			   "~/OneDrive/Documents/tarefas.org")
	org-log-done 'time
	org-agenda-prefix-format '((agenda . " %i %s %(%heading-format)")
				   (timeline . "  % s")
				   (todo .
					 " %i %-12:c %(%heading-format)")
				   (tags .
					 " %i %-12:c %(%heading-format)")
				   (search . " %i %-12:c"))
	org-agenda-skip-scheduled-if-deadline-is-shown t
	org-deadline-warning-days 90)

  :bind (("C-c a" . org-agenda)
	 :map org-mode-map
	 ("<M-S-left>" . nil)
	 ("<M-S-right>" . nil)
	 ("<M-left>" . nil)
	 ("<M-right>" . nil)
	 ("<C-S-right>" . 'org-shiftmetaright)
	 ("<C-S-left>" . 'org-shiftmetaleft)
	 ("<C-right>" . 'org-metaright)
	 ("<C-left>" . 'org-metaleft)))

(use-package org-roam-ui
  :ensure t
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package ox-typst
  :ensure t
  :after org
  :custom (org-typst-process (when (eql system-type 'windows-nt)
			       "typst c --root /Users/trocado/OneDrive \"%s\"")))

(use-package ispell
  :config
  (setq ispell-program-name "hunspell"
	ispell-dictionary-alist
	'(("pt_PT"
	   "[aerisontcdmlupvgbfzáhçqjíxãóéêâúõACMPSBTELGRIFVDkHJONôywUKXZWQÁYÍÉàÓèÂÚ]"
	   "[^aerisontcdmlupvgbfzáhçqjíxãóéêâúõACMPSBTELGRIFVDkHJONôywUKXZWQÁYÍÉàÓèÂÚ]"
	   "" t
	   ("-d" "pt_PT-preao")
	   nil utf-8)
	  ("en_US" "[A-Za-z]" "[^A-Za-z]" "[']" nil
	   ("-d" "en_US")
	   nil iso-8859-1)
	  ("la"	   "[iastnokreuldvmgpjbyczfhwxqāâăáàãēĕèéêëæǣǽīĭìíĩïōŏóòôœūŭúùũýŷÿöIASTNOKREULDVMGPJBCZFHWXQÂÀÁĀĂÃÉÈÊËĒĔÆǢÍÌĪÎÏÒÓŌŎÔÕŒÙÚŪŬÛŨÝŶŸ]" "[^iastnokreuldvmgpjbyczfhwxqāâăáàãēĕèéêëæǣǽīĭìíĩïōŏóòôœūŭúùũýŷÿöIASTNOKREULDVMGPJBCZFHWXQÂÀÁĀĂÃÉÈÊËĒĔÆǢÍÌĪÎÏÒÓŌŎÔÕŒÙÚŪŬÛŨÝŶŸ]"
	   "" nil
	   ("-d" "la")
	   nil utf-8))
	ispell-hunspell-dictionary-alist ispell-dictionary-alist
	ispell-dictionary "en_US"
	ispell-alternate-dictionary "~/hunspell_en_US")

  (defun my/switch-dictionary ()
    (interactive)
    (if (string= ispell-current-dictionary "en_US")
	(progn (abbrev-mode 0)
	       (ispell-change-dictionary "pt_PT")
	       (setq ispell-alternate-dictionary "~/hunspell_pt_PT-preao"))
      (progn (abbrev-mode 1)
	     (ispell-change-dictionary "en_US")
	     (setq ispell-alternate-dictionary "~/hunspell_en_US"))))

  :bind
  ("<f8>" . my/switch-dictionary))

(use-package flyspell
  :config 
  (setq flyspell-issue-message-flag nil)
  :hook 
  (text-mode . turn-on-flyspell))

(use-package eww
  :config
  (setq shr-width 100)
  (setq shr-color-visible-luminance-min 90))

;;; Notmuch
(when (eql system-type 'gnu/linux)
  (load-file (expand-file-name "notmuch-config.el" user-emacs-directory)))

(use-package gnus
  :config
  (setq user-full-name "Nuno Trocado"
	user-mail-address "nuno@nunotrocado.com"
	send-mail-function 'smtpmail-send-it
	smtpmail-smtp-server "smtp.fastmail.com"
	smtpmail-stream-type 'ssl
	smtpmail-smtp-service 465
	smtpmail-servers-requiring-authorization "fastmail"
	gnus-message-archive-group "nnimap+Mail:Sent"
	gnus-select-method
	'(nnimap "fastmail"
		 (nnimap-address "imap.fastmail.com")
		 (nnimap-server-port 993)
		 (nnimap-stream ssl))
	gnus-large-newsgroup 1000
	gnus-user-date-format-alist '((t . "%Y-%m-%d"))
	gnus-summary-line-format "%U%R%I  %&user-date;  %(%[ %-23,23f %]%) %s\\n"
	gnus-always-read-dribble-file t))

;;; Send e-mail without inserting newlines
(use-package message
  :hook (message-mode . (lambda ()
			  (turn-off-auto-fill)
			  (visual-line-mode)
			  (setq mml-enable-flowed nil)))
  :config (setq message-kill-buffer-on-exit t))

(use-package message-attachment-reminder
  ;; TODO config message-attachment-reminder-regexp to include pt expressions
  :ensure t)

(use-package bbdb
  :ensure t
  :config
  (bbdb-initialize 'message 'gnus)
  (bbdb-mua-auto-update-init 'message 'gnus)
  (setf bbdb-mua-action 'create
	bbdb-mua-pop-up nil
	bbdb-message-all-addresses t
	bbdb-ignore-message-alist '(("From" . "reply\\|daemon\\|server"))))

(use-package org-mime ;; Org-mode → email
  :ensure t
  :config
  (setq org-mime-export-options '(:with-latex dvipng
                                :section-numbers nil
                                :with-author nil
                                :with-toc nil)))

(use-package lilypond
  :defer t
  :config
  (if (eql system-type 'windows-nt) (push "c:/Program Files (x86)/LilyPond/usr/share/emacs/site-lisp"
					  load-path)))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  :custom
  (pdf-misc-print-program-executable "/usr/bin/lpr"))

(use-package pdf-view
  :hook (pdf-view-mode . (lambda ()
			   (setq-local mode-line-position
				       '(" P " (:eval (number-to-string (pdf-view-current-page)))
					 ;; Avoid errors during redisplay.
					 ":" (:eval (or (ignore-errors
							  (number-to-string (pdf-cache-number-of-pages)))
							"???"))
					 "  PL " (:eval (pdf-view-current-pagelabel)))))))

(use-package saveplace-pdf-view
  :ensure t
  :config
  (save-place-mode 1))

(use-package nov
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package scratch
  :ensure t
  :bind ("C-c s" . (lambda () (interactive) (scratch 'org-mode))))

(use-package gptel
  :ensure t
  :custom
  (gptel-default-mode 'org-mode)
  :config
  (require 'auth-source)
  (let ((gemini-api-key (funcall (plist-get (car (auth-source-search :host "gemini"))
					    :secret))))
    (when gemini-api-key
      (setq gptel-backend (gptel-make-gemini "Gemini" :key gemini-api-key :stream t)
	    gptel-model 'gemini-2.5-pro-exp-03-25))))

(use-package gptel-quick
  :after gptel embark
  :vc (:url "https://github.com/karthink/gptel-quick" :rev :newest)
  :ensure t
  :config
  (keymap-set embark-general-map "?" #'gptel-quick)
  (setq gptel-quick-model 'gemini-2.0-flash-exp
	gptel-quick-backend gptel-backend))

(use-package which-key
  :config
  (which-key-mode 1))
