;;; STRAIGHT

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;;; USE-PACKAGE

(straight-use-package 'use-package)


;;; GENERAL DEFAULTS

(prefer-coding-system 'utf-8)
(tool-bar-mode 0)
(show-paren-mode 1)
(delete-selection-mode 1)
(global-visual-line-mode t)
(save-place-mode 1)
(setq ring-bell-function 'ignore
      column-number-mode t
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      inhibit-startup-message t
      sentence-end-double-space nil
      enable-recursive-minibuffers t
      recentf-max-saved-items 50)

;;; BACKUPS

(let ((backup-dir (concat user-emacs-directory "backups")))
  (setq backup-directory-alist (list (cons ".*" backup-dir))
	delete-old-versions t
	kept-new-versions 6
	kept-old-versions 2
	version-control t
	backup-by-copying t))


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
  :bind ("<f5>" . modus-themes-toggle))

(use-package diminish
  :straight t
  :config
  (mapcar #'diminish '(eldoc-mode visual-line-mode)))

(use-package vertico
  :straight (:files (:defaults "extensions/*"))
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
  :straight t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :straight t
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
  :straight t
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
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

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
  :straight t

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
  :straight t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package citar
  :straight t
  :init
  (setq org-cite-global-bibliography '("~/OneDrive/lab/master.bib"))
  :config
  (setq citar-library-paths '("~/OneDrive/lab/pdf/")
	citar-notes-paths '("~/OneDrive/lab/notes/"))
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
  :straight t
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
  :straight t
  :config (dired-async-mode 1))

(use-package sly
  :straight t
  :config
  (load (expand-file-name "~/.roswell/helper.el"))
  (setq sly-autodoc-mode t
	sly-autodoc-use-multiline-p t
	inferior-lisp-program "ros dynamic-space-size=3000 -Q run"))

(use-package paredit
  :straight t
  :diminish paredit-mode
  :hook ((emacs-lisp-mode
	  eval-expression-minibuffer-setup
	  lisp-mode
	  lisp-interaction-mode
	  scheme-mode)
	 . enable-paredit-mode)
  :hook (sly-mrepl-mode . (lambda ()
			    (let ((oldmap (cdr (assoc 'paredit-mode minor-mode-map-alist)))
				  (newmap (make-sparse-keymap)))
			      (set-keymap-parent newmap oldmap)
			      (define-key newmap ["RET"] nil)
			      (make-local-variable 'minor-mode-overriding-map-alist)
			      (push `(paredit-mode . ,newmap) minor-mode-overriding-map-alist)))))

(use-package company
  :straight t
  :diminish company-mode
  :hook (after-init . global-company-mode)
  :config (setq company-dabbrev-downcase nil
		company-show-quick-access t))

(use-package company-posframe
  :straight t
  :after company
  :diminish company-posframe-mode
  :config (company-posframe-mode 1))

(use-package magit
  :straight t)

(use-package circe
  :straight t
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
    (my-fetch-password :login "trocado" :machine "irc.libera.chat"))
  
  :config
  (setq circe-network-options
      '(("irc.libera.chat"
	 :port 7000
         :use-tls t
         :nick "trocado"
	 :user "trocado"
	 :realname "trocado"
	 :sasl-username "trocado"
	 :sasl-password my-nickserv-password
         :channels (:after-auth "#lisp" "#commonlisp" "#dataflow"
				"#lispgames" "#supercollider" "#org-mode"
				"#clschool" "#emacs-circe" "#lilypond"
				"##running" "#org-roam" "##latin" "##latinitas")
	 :reduce-lurker-spam t)))
  (require 'circe-color-nicks)
  (enable-circe-color-nicks)
  (setq circe-color-nicks-everywhere t))

(use-package powerthesaurus
  :straight t
  :config
  ;; https://github.com/SavchenkoValeriy/emacs-powerthesaurus/issues/16
  (setq powerthesaurus-request-headers
	(delete '("Accept-Encoding" . "gzip, deflate, br")
		powerthesaurus-request-headers)))

(use-package pt
  :straight t)

(use-package golden-ratio
  :straight t
  :diminish golden-ratio-mode
  :config (golden-ratio-mode))


;;; ORG

(use-package org
  :straight t
  :custom
  (org-startup-indented t)
  (org-footnote-auto-adjust t)
  (org-src-preserve-indentation t)
  (org-edit-src-content-indentation 0)
  (calendar-week-start-day 1)
  (org-hide-emphasis-markers t)
  (org-list-allow-alphabetical t)
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

  ;; agenda
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

;; (setq org-priority-faces '((?A . (:foreground "red" :weight 'bold))
;;                            (?B . (:foreground "yellow"))
;;                            (?C . (:foreground "gray"))))

;;; ORG LATEX EXPORT

;; (setq org-latex-pdf-process '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "biber %f" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq org-latex-pdf-process (list
   "latexmk -pdfxe -f %f"))

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

;; (org-add-link-type
;;  "latex" nil
;;  (lambda (path desc format)
;;    (cond
;;     ((eq format 'html)
;;      (format "<span class=\"%s\">%s</span>" path desc))
;;     ((eq format 'latex)
;;      (format "\\%s{%s}" path desc)))))

;;; ORG BLOG EXPORT

(setq org-html-htmlize-output-type 'css)

(setq org-publish-project-alist
      '(

  ("org-trocado"
          ;; Path to your org files.
          :base-directory "c:/Users/trocado/OneDrive/Documents/Practice-log/"
          :base-extension "org"

          ;; Path to your Jekyll project.
          :publishing-directory "c:/Users/trocado/Documents/ntrocado.github.io/"
          :recursive t
          :publishing-function org-html-publish-to-html
          :headline-levels 4
          :html-extension "html"
          :body-only t ;; Only export section between <body> </body>
	  )


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

;; (org-link-set-parameters "img" 'org-custom-link-img-follow 'org-custom-link-img-export)

;;; ORG-REF and IVY-BIBTEX


;;   (defun my/sci-hub ()
;;     "Opens a browser for Sci-hub with the bibtex entry at point."
;;     (interactive)
;;     (browse-url (concat "https://sci-hub.se/"
;; 			(replace-regexp-in-string
;; 			 "https?://\\(dx.\\)?.doi.org/" ""
;; 			 (bibtex-autokey-get-field "doi"))))))

(use-package org-roam
  :straight (:host github :repo "org-roam/org-roam" :branch "main"
		   :files (:defaults "extensions/*"))
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
		 (window-height . fit-window-to-buffer))))

(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
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

;; (use-package org-ref
;;   :straight t
;;   :after org
;;   :init
;;   (define-key org-mode-map (kbd "C-c c") 'org-ref-insert-link)
;;   (setq org-ref-default-citation-link "autocite")
;;   (require 'org-ref-ivy)
;;   :config
;;   ;; Count references: https://github.com/jkitchin/org-ref/issues/1034
;;   (defun my/count-refs ()
;;     (interactive)
;;     (let* ((cites (org-element-map (org-element-parse-buffer) 'link
;; 		    (lambda (lnk)
;; 		      (when (member (org-element-property :type lnk)
;; 				    (mapcar 'car org-ref-cite-types))
;; 			(cl-loop for ref in (plist-get (org-ref-parse-cite-path
;; 							(org-element-property :path lnk))
;; 						       :references)
;; 				 collect (plist-get ref :key))))))
;; 	   (all-cites (-flatten cites))
;; 	   (uniq-cites (-uniq all-cites)))
;;       (message "%d references" (length uniq-cites)))))


;;; SPELLING
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
	ispell-dictionary "en_US")

  (defun my/switch-dictionary ()
    (interactive)
    (if (string= ispell-current-dictionary "en_US")
	(progn (abbrev-mode 0)
	       (ispell-change-dictionary "pt_PT"))
      (progn (abbrev-mode 1)
	     (ispell-change-dictionary "en_US"))))

  :bind
  ("<f8>" . my/switch-dictionary))

(use-package flyspell
  :config 
  (setq flyspell-issue-message-flag nil)
  :hook 
  (text-mode . turn-on-flyspell))

;;; Eww

(setq shr-width 100)
(setq shr-color-visible-luminance-min 90)

;;; Cl-collider

(defun slime-documentation-supercollider (ugen)
  (interactive (list
                (completing-read "Class: " (slime-supercollider-get-ugens-list))))
  (browse-url (concat "http://doc.sccode.org/Classes/" ugen ".html")))

(defvar slime-supercollider-ugens-list nil)

(defun slime-supercollider-get-ugens-list ()
  (if (null slime-supercollider-ugens-list)
      (progn
        (with-temp-file "c:\\Users\\trocado\\supercollider-get-ugens-list.scd"
          (insert "\"-----\".postln;Object.allSubclasses.do(_.postcs);\"-----\".postln;0.exit;"))
        (with-temp-buffer
          (call-process-shell-command "\"c:\\Program Files\\SuperCollider-3.11.0\\sclang.exe\"" nil t nil "\"c:\\Users\\trocado\\supercollider-get-ugens-list.scd\"")
          (goto-char (point-min))
          (search-forward "\n-----\n")
          (setf slime-supercollider-ugens-list
                (sort (split-string (buffer-substring-no-properties (point) (- (save-excursion (search-forward "\n-----\n") (point)) 6)) "\n" t) #'string<))))
    slime-supercollider-ugens-list))

(defun sly-stop-sc ()
  (interactive)
  (sly-interactive-eval "(sc:stop)"))

(defun sly-stop-patterns ()
  (interactive)
  (sly-interactive-eval "(cl-patterns:stop t)"))

(defun cl-patterns-helpers-load ()
  (interactive)
  (sly-eval-async '(cl:namestring (asdf:system-source-directory (asdf:find-system 'cl-patterns)))
    (lambda (path)
      (load (concat path "res/emacs/cl-patterns-helpers") nil nil nil t)
      (load (concat path "res/emacs/cl-patterns-skeletons") nil nil nil t)))
  (define-key sly-mode-map (kbd "C-c p") 'cl-patterns-play-or-end-context-or-select-pdef)
  (define-key sly-mode-map (kbd "C-c P") 'cl-patterns-play-or-stop-context-or-select-pdef)
  (define-key sly-mode-map (kbd "C-c s") 'cl-patterns-stop-all)
  (define-key sly-doc-map (kbd "s") 'cl-patterns-supercollider-documentation))

(add-hook 'sly-connected-hook 'cl-patterns-helpers-load)

(add-hook 'sly-mode-hook 'my-sly-mode-hook)
(defun my-sly-mode-hook ()
  (define-key sly-mode-map (kbd "C-c C-d s") 'slime-documentation-supercollider)
  (define-key sly-mode-map (kbd "C-.") 'sly-stop-sc)
  (define-key sly-mode-map (kbd "C-,") 'sly-stop-patterns))

;;; Ag

(setq ag-executable "c:/Program Files/ag/ag.exe")

;;; Multiple cursors

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;; Notmuch

;; (when (eql system-type 'gnu/linux)
;;   (load-file (expand-file-name "notmuch-config.el" user-emacs-directory)))

;;; Gnus

(use-package gnus
  :config
  (setq user-full-name "Nuno Trocado"
	user-mail-address "nuno@nunotrocado.com"
	send-mail-function 'smtpmail-send-it
	smtpmail-smtp-server "smtp.fastmail.com"
	smtpmail-stream-type 'starttls
	smtpmail-smtp-service 587
	gnus-select-method
	'(nnimap "fastmail"
		 (nnimap-address "imap.fastmail.com")
		 (nnimap-server-port 993)
		 (nnimap-stream ssl)
		 ;; (nnir-search-engine imap)
		 ;; (nnmail-expiry-wait-function
		 ;;  (lambda (group)
		 ;;    (if (string-match-p "INBOX" group)
		 ;; 	'immediate
		 ;;      'never))))
		 )
	gnus-large-newsgroup 1000
	gnus-user-date-format-alist '((t . "%Y-%m-%d"))
	gnus-summary-line-format "%U%R%I  %&user-date;  %(%[ %-23,23f %]%) %s\\n"
	gnus-always-read-dribble-file t))
  ;; (add-to-list 'nnir-imap-search-arguments '("gmail" . "X-GM-RAW"))
  ;; (setq nnir-imap-default-search-key "gmail"))

;;; Send e-mail without inserting newlines
(use-package message
  :hook (message-mode . (lambda ()
			  (turn-off-auto-fill)
			  (visual-line-mode)
			  (setq mml-enable-flowed nil))))

(use-package message-attachment-reminder
  ;; TODO config message-attachment-reminder-regexp to include pt expressions
  :straight t)

;;; Contacts
(use-package bbdb
  :straight t
  :ensure t
  :config
  (bbdb-initialize 'message 'gnus)
  (bbdb-mua-auto-update-init 'message 'gnus)
  (setf bbdb-mua-action 'create
	bbdb-mua-pop-up nil
	bbdb-message-all-addresses t
	bbdb-ignore-message-alist '(("From" . "reply\\|daemon\\|server"))))


;;; Lilypond
(if (eql system-type 'windows-nt) (push "c:/Program Files (x86)/LilyPond/usr/share/emacs/site-lisp"
					load-path))


;;; PDF-Tools
(use-package pdf-tools
  :straight t
  :config
  (pdf-tools-install)
  :custom
  (pdf-misc-print-program-executable "/usr/bin/lpr"))

(use-package saveplace-pdf-view
  :straight t
  :config
  (save-place-mode 1))

;;; Epub
(use-package nov
  :straight t)
