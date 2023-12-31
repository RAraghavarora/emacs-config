(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e8ab68ce371f48623dab9658d7910458e98affec3e09585a39552dbd3fd1ecda" "d9c038dc91688c433de8e83709449563ec6475b900a21d7016856035ae4dcd32" "0d12b08dec64641c5df1a13d2c52ad678f6235a9b1c86041ea457fc1a71651dc" "2d09bd884d697b48b380b48117ccaebd8e99fe1cb242e31675dcec5724c603f6" "9b64a681308383067359cf06bfa6a1bc4fa75c5b68182e4d6ba4d1816277d70e" "64204b9e3ad01000654d5524d2904fc8fa28aafc168f48660897ddfe36a2bfd5" "8e08bb8da358e2cf92e10e4bac47b025ccbcf4c70788cdbd67dc4ed11f786194" "82f0f47ac811eeb45fbcfc5fee48989e4d0bca11f74653b838c29fab9a20aee7" "f7b6b207d7a6318ea5d33ca2dea51483350d0c26beb986f008d63258b9c112ab" "ed6e47baf355da248c4de8953058234b82c8f838fc85f570f9fe1700e47b9426" "d31c3706f7c1b0520405c796b33f515bc481d2062cbc964f3c36925665999a6d" "454e92bc5f22f690afce91cb6f92a3ccb638c57a89e84c55759fb16dfb2444e4" "cc0ea29f3e80c5c622cea5fd83686dd36963a9abcde5b21dfe6cee653b67d72f" "5a409d7844bfbc12bf6e6cf7a6a6cb9f79945a82dee315ed1778632b5686b6ec" "6c01b5d4faa0f143055e63c9fba8e23e9160f181e54b14b46d56410811edbc9e" default))
 '(package-selected-packages
   '(orgmode projectile hydra telega evil-collection evil general auctex helpful ivy-rich which-key rainbow-delimiters doom-modeline swiper-helm counsel command-log-mode git ag ivy ef-themes ein)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(set-face-attribute 'default nil :height 120)

;; Make esc quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))


;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))


(require 'use-package)
(setq use-package-always-ensure t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable Line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
	      eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


(use-package command-log-mode)

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

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ; Don't start searches with ^

(setq python-shell-interpreter "C:/Users/raghav/miniconda3/envs/default/python.exe")
(setq python-shell-interpreter-args "-i")

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))


(require 'ein)
(require 'git)
(setq ein:jupyter-default-server-command "C:/Users/raghav/miniconda3/envs/default/Scripts/jupyter.exe")

(windmove-default-keybindings)

(load-theme 'ef-elea-dark t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package helpful
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
(setq-default TeX-engine 'xetex)

;; Defining personal namespaces 
(use-package general
  :config
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (rune/leader-keys
   "t" '(:ignore t :which-key "toggles")
   "tt" '(counsel-load-theme :which-key "choose theme"))
)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package hydra)

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
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "C:/raghav/")
    (setq projectile-project-search-path '("C:/raghav/")))
  (setq projectile-switch-project-action #'projectile-dired))

