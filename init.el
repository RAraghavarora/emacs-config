;; Basic configuration

;; (set-face-attribute 'default nil :height 120) ; Set font height
(when (display-graphic-p)
  ;; Replace '12' with your desired font size
  (set-frame-font "SpaceMonoNerdFont-14" nil t))
;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; Make esc quit prompts
(setq inhibit-startup-message t) ; Don't show the start screen

;; Turn off UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode 0)

(column-number-mode)
(global-display-line-numbers-mode t)

(recentf-mode 1); Recently viewed files

;; Save history of minibuffer commands
(setq history-length 25)
(savehist-mode 1)

(save-place-mode 1) ; Open files at the same place where you left it

;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Revert buffers when underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Open init.el file with a keybinding
(defun my-edit-configuration ()
  (interactive)
  (find-file user-init-file))

(global-set-key (kbd "C-c i") 'my-edit-configuration)

;; Set up package managers
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t)
(setq use-package-verbose t)

;; Set theme
(use-package ef-themes)
(load-theme 'ef-rosa t)

(use-package no-littering)

;; Download icons
(use-package all-the-icons
  :if (display-graphic-p))
(use-package nerd-icons
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "SpaceMono Nerd Font Mono")
  )
;; Split vertically?
(setq split-height-threshold nil)
(setq split-width-threshold 0)

(use-package compat)

;; Load the setup-cygwin.el file
;; (add-to-list 'load-path "C:/Users/raghav/AppData/Roaming/.emacs.d/cygwin")
;; (load "use_cygwin.el")
;; (load "setup-cygwin.el")

;; Disable Line numbers for some modes

(defun ra/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections"
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'ra/display-startup-time)


(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
	    eshell-mode-hook
        treemacs))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Defining personal namespaces 
(use-package general
  :after evil
  :config
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (rune/leader-keys
    "t" '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "tm" '(set-mark-command :which-key "set mark"))
  )

;; Disable keymapping C-z for suspending frame
(put 'suspend-frame 'disabled t)
;; (global-unset-key (kbd "C-z"))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-C-w-in-emacs-state t) 
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  )

(setq evil-toggle-key "C-z")
(evil-mode 1)

;; Make Evil Normal State the Initial State Always
(setq evil-normal-state-modes
      (append evil-emacs-state-modes
              evil-insert-state-modes
              evil-normal-state-modes
              evil-motion-state-modes))

(use-package evil-escape
  :init
  (setq-default evil-escape-key-sequence "jk")
  :config
  (evil-escape-mode 1))
(global-set-key (kbd "C-c C-g") 'evil-escape)
;; (add-hook 'focus-out-hook 'evil-normal-state)


(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ; Don't start searches with ^

;; To show better results in ivy based on previous use
(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  (ivy-prescient-mode 1)
  (prescient-persist-mode 1) 
  )

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))


(use-package ein
  :hook (ein:notebooklist-mode . auto-revert-mode)
  :init
  (setq ein:worksheet-enable-undo t)
  )
(setq debug-on-error t)

(defun force-debug (func &rest args)
  (condition-case e
      (apply func args)
    ((debug error) (signal (car e) (cdr e)))))

;; (advice-add #'corfu--post-command :around #'force-debug)
;; (require 'git)
;; (setq ein:jupyter-default-server-command "~/miniconda3/envs/default/Scripts/jupyter")


(windmove-default-keybindings)

(use-package smartparens
  :config
  (add-hook 'prog-mode-hook 'smartparens-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Better help buffer for emacs
(use-package helpful
  :commands (helpful-callable helpful-variable)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package tex
:ensure auctex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
;; (setq-default TeX-engine 'xetex)


(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(rune/leader-keys
 "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "C:/raghav/")
    (setq projectile-project-search-path '("C:/raghav/")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package ripgrep)

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package magit
  :commands (magit-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(define-key global-map (kbd "C-c g") 'magit-status)

(use-package forge
  :after magit)

(defun ra/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun ra/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Arial" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :pin org
  :hook (org-mode . ra/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)
  (setq org-agenda-files
	'("C:/raghav/Kaggle-LLM-Detection/tasks.org"
	"C:/raghav/Habits.org"))
  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  
  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)
       ("agenda" . ?a)
       ("planning" . ?p)
       ("publish" . ?P)
       ("batch" . ?b)
       ("note" . ?n)
       ("idea" . ?i)))
  
  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work-email")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))


  ;; (setq org-capture-templates
  ;;   `(("t" "Tasks / Projects")
  ;;     ("tt" "Task" entry (file+olp "C:/raghav/Tasks.org" "Inbox")
  ;;          "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

  ;;     ("j" "Journal Entries")
  ;;     ("jj" "Journal" entry
  ;;          (file+olp+datetree "C:/raghav/Journal.org")
  ;;          "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
  ;;          ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
  ;;          :clock-in :clock-resume
  ;;          :empty-lines 1)
  ;;     ("jm" "Meeting" entry
  ;;          (file+olp+datetree "C:/raghav/Journal.org")
  ;;          "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
  ;;          :clock-in :clock-resume
  ;;          :empty-lines 1)

  ;;     ("w" "Workflows")
  ;;     ("we" "Checking Email" entry (file+olp+datetree "C:/raghav/Journal.org")
  ;;          "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

  ;;     ("m" "Metrics Capture")
  ;;     ("mw" "Weight" table-line (file+headline "C:/raghav/Metrics.org" "Weight")
  ;;      "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  ;; (define-key global-map (kbd "C-c j")
  ;; 	      (lambda () (interactive) (org-capture nil "jj")))
  ;; (define-key global-map (kbd "C-c t")
  ;; 	      (lambda () (interactive) (org-capture nil "tt")))
  
  (ra/org-font-setup)
  )


(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(setq tramp-verbose 10)

(use-package conda
  :after python
  :ensure t
  :config (progn
            (conda-env-initialize-interactive-shells)
            (conda-env-initialize-eshell)
            (conda-env-autoactivate-mode t)
            (setq conda-env-home-directory (expand-file-name "~/miniconda3"))
            (setq-default mode-line-format (cons mode-line-format '(:exec conda-env-current-name)))
            ))
(setq-default mode-line-format (cons '(:exec conda-env-current-name) mode-line-format))
(setq tab-always-indent 'complete) ; Try to indent the current line, else call completion-at-point
(setq-default indent-tabs-mode nil
              tab-width 4)
(setq completion-styles '(flex basic partial-completion emacs22))
(put 'scroll-left 'disabled nil)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  :hook
  (python-mode . lsp-deferred)
  (html-mode . lsp-deferred)
  :custom
  (python-shell-interpreter "~/miniconda3/default/bin/python")
  )

(use-package dap-mode
  :commands dap-debug
  :after lsp-mode
  :config
  (require 'dap-python)
  (dap-mode t)
  (dap-ui-mode t)
  (dap-auto-configure-mode)
  (general-define-key
   :keymaps 'python-mode-map
   :prefix lsp-keymap-prefix
   "d" '(dap-hydra t :wk "debugger"))
  )

(setq dap-python-debugger 'debugpy)

;; (use-package lsp-pyright
;;   :after lsp-mode
;;   :ensure t
;;   :config
;;   (setq lsp-clients-python-library-directories '("~/miniconda3/pkgs"))
;;   (setq lsp-pyright-disable-language-service nil
;; 	lsp-pyright-disable-organize-imports nil
;; 	lsp-pyright-auto-import-completions t
;; 	lsp-pyright-use-library-code-for-types t
;; 	lsp-pyright-venv-path "~/miniconda3/envs")

;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-pyright)
;;                           (lsp)
;;                           (add-hook 'after-save-hook 'lsp-pyright-organize-imports nil t)))

;;   )

(use-package yasnippet
  :after lsp-mode)

;; (use-package py-isort
;;   :after python
;;   :hook ((python-mode . pyvenv-mode)
;;          (before-save . py-isort-before-save)))

(defun my-isort ()
  "When in Python Mode, call isort on save"
  (when (eq major-mode 'python-mode)
    (shell-command-to-string (format "isort %s" buffer-file-name))))

;; Run on file save
;; (add-hook 'after-save-hook 'my-isort)

(use-package treemacs
  :hook (treemacs-mode . variable-pitch-mode)
  :ensure t
  :defer t
  :config
  (setq
   ;; treemacs-no-png-images t
        treemacs-width-increment 1
        treemacs-width 34
        treemacs-follow-mode -1
        treemacs-fringe-indicator-mode t
        treemacs-git-mode 'deferred
        treemacs-filewatch-mode t
        ;; treemacs-resize-icons 16
        )
  (treemacs-indent-guide-mode 'line)
  (when treemacs-python-executable
    (treemacs-git-commit-diff-mode t))
  :bind
  (("C-c t" . treemacs-display-current-project-exclusively)
   ("C-c T" . treemacs)))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ui
  :ensure t
  :defer t
  :config
  (setq lsp-ui-sideline-enable nil
	    lsp-ui-doc-delay 2)
  :hook
  (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
	          ("C-c i" . lsp-ui-imenu)))

(use-package lsp-ivy
  :ensure t
  :after lsp)

(use-package blacken
  :after python
  :delight
  :config
  (add-hook 'python-mode-hook 'blacken-mode))


;; (add-to-list 'safe-local-variable-values
;;              '(Conda- . "lualatex -shell-escape"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package python
  :after lsp-mode
  :delight "π "
  :bind (("M-[" . python-nav-backward-block)
         ("M-]" . python-nav-forward-block))
  :preface
  (defun python-remove-unused-imports()
    "Removes unused imports and unused variables with autoflake."
    (interactive)
    (if (executable-find "autoflake")
        (progn
          (shell-command (format "autoflake --remove-all-unused-imports -i %s"
                                 (shell-quote-argument (buffer-file-name))))
          (revert-buffer t t t))
      (warn "python-mode: Cannot find autoflake executable.")))
  :ensure t
  :config
  (require 'dap-python)
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil)
  ;; Use IPython when available or fall back to regular Python 
  (cond
   ((executable-find "ipython")
    (progn
      (setq python-shell-buffer-name "IPython")
      (setq python-shell-interpreter "ipython")
      (setq python-shell-interpreter-args "-i --simple-prompt")))
   ((executable-find "python3")
    (setq python-shell-interpreter "~/miniconda3/envs/default/bin/python"))
   ((executable-find "python2")
    (setq python-shell-interpreter "python2"))
   (t
    (setq python-shell-interpreter "python")))

  :custom
  (dap-python-debugger 'debugpy)
  )

(use-package pyvenv
  :after python
  :ensure t
  :defer t
  :config
  ;; Setting work on to easily switch between environments
  (setenv "WORKON_HOME" (expand-file-name "~/miniconda3/envs/"))
  ;; Display virtual envs in the menu bar
  (setq pyvenv-menu t)
  ;; Restart the python process when switching environments
  (add-hook 'pyvenv-post-activate-hooks (lambda ()
					  (pyvenv-restart-python)))
  :hook (python-mode . pyvenv-mode))

(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'editorconfig)
(editorconfig-mode 1)
;; These 3 are needed by codeium
;; (use-package s)
;; (use-package dash)
;; (use-package editorconfig)

(add-to-list 'load-path  "~/.emacs.d/copilot.el")
;; (add-to-list 'load-path  "~/.emacs.d/editorconfig-emacs")
;; (add-to-list 'load-path "~/.emacs.d/codeium.el")
(defun load-copilot-in-prog-mode ()
  (require 'copilot))

(add-hook 'prog-mode-hook 'load-copilot-in-prog-mode)

(defun ra/copilot-setup ()
  "Setup Copilot in prog-mode."
  (require 'copilot)
  (setq-local copilot--indent-warning-printed-p t)
  (add-hook 'prog-mode-hook 'copilot-mode)

  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
)

(add-hook 'prog-mode-hook 'ra/copilot-setup)

(set-language-environment "UTF-8")

(with-eval-after-load 'org
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((emacs-lisp . t)
         (python . t)))

      (require 'org-tempo)
      (add-to-list 'org-structure-template-alist '("py" . "src python :results output :session"))
      )


;; Do not prompt to confirm evaluation
(setq org-confirm-babel-evaluate nil)
;; (use-package company
;;   :ensure t
;;   :config
;;   (setq company-idle-delay 0.1
;; 	company-minimum-prefix-length 1)
;;   )
;; (add-hook 'after-init-hook 'global-company-mode)
(use-package corfu
  :hook lsp-mode
  :pin elpa
  :ensure t
  ;; Optional customizations
  :custom
  (completion--cycle-threshold t) ;; Always show candidates in menu
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary t)   ;; Never quit at completion boundary
  (corfu-quit-no-match t)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect-first nil)    ;; Disable candidate preselection
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode)
  :bind (:map corfu-map
                   ("<escape>". corfu-quit)
                   ("<return>" . corfu-insert)
                   ("C-h" . corfu-show-documentation)
                   ("M-l" . 'corfu-show-location)
                   ("TAB" . corfu-next)
                   ([tab] . corfu-next)
                   ("S-TAB" . corfu-previous)
                   ([backtab] . corfu-previous)))

;; (use-package highlight-indent-guides
;;   :ensure t
;;   :hook
;;   (python-mode . highlight-indent-guides-mode)
;;   :config
;;   (set-face-foreground 'highlight-indent-guides-character-face "green")
;;   (setq highlight-indent-guides-mode 'character))

;; Syntax checking for GNU Emacs

(use-package flycheck
  :diminish flycheck-mode
  :ensure t
  :defer t
  :custom
  (flycheck-check-syntax-automatically '(mode-enabled save)) ; Check on save instead of running constantly
  :hook ((prog-mode-hook text-mode-hook) . flycheck-mode))


(use-package flymake
  :ensure t
  :defer t)

(use-package markdown-mode
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown"))


(put 'ein:jupyter-server-command 'safe-local-variable (lambda (_) t))
(setq lazy-highlight-cleanup t)
(setq request-curl-options '("--noproxy" "127.0.0.1"))
(put 'downcase-region 'disabled nil)
(setq projectile-enable-caching t)


(general-override-mode)
;; Define a key binding for entering Evil normal state with <escape>
(general-def 'insert
  [escape] 'evil-normal-state)
(general-def 'override
  [escape] 'keyboard-escape-quit)
;; Adapt `evil-collection-corfu' to the new `corfu--setup' signature (see github.com/minad/corfu/issues/403)
(with-eval-after-load 'evil-collection-corfu
  (advice-remove 'corfu--setup #'evil-normalize-keymaps)
  (advice-add 'corfu--setup :after (lambda (&rest _) (evil-normalize-keymaps))))

(defun todo ()
  (interactive)
  (find-file "C:/raghav/todo.org")
)

(use-package web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(use-package copy-as-format)

;; (use-package gptel)
;; (gptel-make-gemini "Gemini"
;;   :key "AIzaSyCh6kE0nLZx-H5xzGqoHB4KfmjOOh-ZJbI"
;;   :stream t)
;; (setq-default gptel-model "gemini-pro")
;; (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)


(use-package spaceline)

(use-package spaceline-all-the-icons 
  :after spaceline
  :config (spaceline-all-the-icons-theme))

;; (use-package obsidian
;;   :ensure t
;;   :demand t
;;   :config
;;   (obsidian-specify-path "~/Obsidian/")
;;   (global-obsidian-mode t)
;;   :custom
;;   ;; This directory will be used for `obsidian-capture' if set.
;;   (obsidian-inbox-directory "Inbox")
;;   ;; Create missing files in inbox? - when clicking on a wiki link
;;   ;; t: in inbox, nil: next to the file with the link
;;   ;; default: t
;;   ;(obsidian-wiki-link-create-file-in-inbox nil)
;;   ;; The directory for daily notes (file name is YYYY-MM-DD.md)
;;   (obsidian-daily-notes-directory "Daily Notes")
;;   ;; Directory of note templates, unset (nil) by default
;;   ;(obsidian-templates-directory "Templates")
;;   ;; Daily Note template name - requires a template directory. Default: Daily Note Template.md
;;   ;(setq obsidian-daily-note-template "Daily Note Template.md")
;;   :bind (:map obsidian-mode-map
;;   ;; Replace C-c C-o with Obsidian.el's implementation. It's ok to use another key binding.
;;   ("C-c C-o" . obsidian-follow-link-at-point)
;;   ;; Jump to backlinks
;;   ("C-c C-b" . obsidian-backlink-jump)
;;   ;; If you prefer you can use `obsidian-insert-link'
;;   ("C-c C-l" . obsidian-insert-wikilink)))

;; (use-package auctex
;;   :straight t
;;   :defer t
;;   :mode
;;   ("\\.tex\\'" . latex-mode)

;;   ("\\.ltx\\'" . latex-mode)

;;   :commands
;;   (latex-mode
;;    LaTeX-mode
;;    TeX-mode)
;;   :hook (LaTeX-mode .
;;                     (lambda ()
;;                         (turn-on-reftex)
;;                         (LaTeX-preview-setup)
;;                         (flyspell-mode)
;;                         (outline-minor-mode)
;;                         (hs-minor-mode)
;;                       )
;;                     )
;;   :init
;;   (setq-default TeX-master nil)
;;   ;; :config
;;   :custom
;;   (TeX-auto-save t)
;;   (TeX-parse-self t)
;;   (TeX-save-query nil)
;;   (TeX-PDF-mode t)
;;   (LaTeX-beamer-item-overlay-flag nil)
;;   (TeX-PDF-mode t)
;;   (TeX-quote-after-quote nil)
;;   (TeX-open-quote "\"")
;;   (TeX-close-quote "\"")
;;   (TeX-insert-macro-default-style 'mandatory-args-only)
;;   )

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))
  
(use-package sqlite3)
(use-package notmuch
  :commands notmuch-hello
  :bind (("C-c m" . notmuch-hello)))
(use-package exec-path-from-shell)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(setq x-select-enable-clipboard-manager nil)

(use-package mu4e
  :ensure nil
  :commands mu4e
  :config
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/Mail")
  (setq mu4e-sent-folder "/[Gmail].Sent Mail")
  (setq mu4e-dratfs-folder "/[Gmail].Drafts")
  (setq mu4e-trash-folder "/[Gmail].Trash")
  (setq mu4e-refile-folder "/[Gmail].All Mail")

  (setq mu4e-maildir-shortcuts
        '(("/INBOX" . ?i)
          ("/[Gmail].Sent Mail" . ?s)
          ("/[Gmail].Trash" . ?t)
          ("/[Gmail].Drafts" . ?d)
          ("/[Gmail].All Mail" . ?a)))
  
  )

