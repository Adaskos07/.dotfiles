(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
        (package-install 'use-package))
(require 'use-package)

        
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-M-u") 'universal-argument)

(defun as/evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  circe-server-mode
                  circe-chat-mode
                  circe-query-mode
                  sauron-mode
                  term-mode))
   (add-to-list 'evil-emacs-state-modes mode)))

(use-package undo-tree
  :diminish
  :init
  (global-undo-tree-mode 1))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree)
  :config
  (add-hook 'evil-mode-hook 'as/evil-hook)
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  ;; (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  ;; (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :ensure t
  :after evil
  :init
  (setq evil-collection-company-use-tng nil)
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (setq evil-collection-mode-list
        (remove 'lispy evil-collection-mode-list))
  (evil-collection-init)
  :diminish evil-collection-unimpaired-mode)

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package vertico
  :ensure t
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-f" . vertico-exit))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package savehist
  :ensure t
  :config
  (setq history-length 25)
  (savehist-mode 1))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file) (styles . partial-completions))))
 
(use-package marginalia
  :ensure t
  :after vertico
  ;; :custom
  ;; (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package helpful
  :ensure t
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package general
  :ensure t
  :config
  (general-evil-setup t)
  (general-create-definer as/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (general-create-definer as/ctrl-c-keys
    :prefix "C-c"))
 
(use-package all-the-icons)
(use-package all-the-icons-dired
  :config
  :hook (dired-mode . (lambda ()
                        (interactive)
                        (unless (file-remote-p default-directory)
                          (all-the-icons-dired-mode)))))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :config
  (setq dired-listing-switches "-agho --group-directories-first"
        dired-omit-files "^\\.[^.].*"
        dired-omit-verbose nil
        dired-hide-details-hide-symlink-targets nil
        delete-by-moving-to-trash t)
  (autoload 'dired-omit-mode "dired-x")
  (add-hook 'dired-load-hook
            (lambda ()
              (interactive)
              (dired-collapse)))
  (add-hook 'dired-mode-hook
            (lambda ()
              (interactive)
              (dired-omit-mode 1)
              (hl-line-mode 1)))
  (use-package dired-single
    :defer t)
  (use-package dired-collapse
    :defer t)

  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "H" 'dired-omit-mode
    "l" 'dired-single-buffer))
   
(use-package openwith
  :config
  (setq openwith-associations
        (list
          (list (openwith-make-extension-regexp
                 '("mpg" "mpeg" "mp3" "mp4"
                   "avi" "wmv" "wav" "mov" "flv"
                   "ogm" "ogg" "mkv"))
                "mpv"
                '(file))
          (list (openwith-make-extension-regexp
                 '("xbm" "pbm" "pgm" "ppm" "pnm"
                   "png" "gif" "bmp" "tif" "jpeg" "jpg"))
                "sxiv"
                '(file))
          (list (openwith-make-extension-regexp
                 '("pdf"))
                "zathura"
                '(file)))))

(use-package parinfer-rust-mode
  :diminish
  :hook clojure-mode
  :hook emacs-lisp-mode
  (setq parinfer-rust-auto-download t))

(use-package magit
  :commands (magit-status magit-get-current-branch))

(use-package diminish)

;; cider settings
(use-package cider
  :init
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (setq cider-eldoc-display-for-symbol-at-point t)
  (setq cider-eldoc-display-context-dependent-info t)
  :config
  (evil-collection-cider-setup))

(use-package eldoc
  :diminish eldoc-mode)

(defun as/org-mode-setup ()
  (display-line-numbers-mode -1)
  ;; (org-indent-mode)
  (electric-indent-mode 0)
  (auto-fill-mode 0)
  (setq evil-auto-indent nil)
  (diminish org-indent-mode))

(use-package org
  :defer t
  :hook (org-mode . as/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (set-face-underline 'org-ellipsis nil)
  (setq org-modules
        '(org-crypt
          org-habit
          org-bookmark
          org-eshell
          org-irc)))
 
(use-package vterm
  :ensure t
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

(use-package elfeed
  :commands elfeed
  :config
  (as/leader-key-def
    "v" 'elfeed-view-mpv)
  (setq elfeed-feeds
    '(("https://lukesmith.xyz/rss.xml" primary blog)
      ("https://lukesmith.xyz/videos" primary videos)
      "https://hnrss.org/frontpage" secondary)))
      
(defun elfeed-v-mpv (url)
 "Watch a video from URL in MPV" 
 (async-shell-command (format "mpv '%s'" url)))

(defun elfeed-view-mpv (&optional use-generic-p)
  "Youtube-feed link"
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
     do (elfeed-untag entry 'unread)
     when (elfeed-entry-link entry) 
     do (elfeed-v-mpv it)) 
   (mapc #'elfeed-search-update-entry entries) 
   (unless (use-region-p) (forward-line)))) 

(use-package mpv)
;; no emacs welcome screen
(setq inhibit-startup-screen t)

;; disable toolbars and scrollbars
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; relative numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; use spaces instead of tabs for indentation
(setq-default indent-tabs-mode)

;; change font
(set-face-attribute 'default nil
      :family "JetBrainsMonoMedium Nerd Font"
      :height 117)

(set-face-attribute 'fixed-pitch nil
                    :font "JetBrainsMonoMedium Nerd Font")

(set-face-attribute 'variable-pitch nil
                    :font "Linux Libertine"
                    :height 150)

;; make steps for font changes smaller
(setq text-scale-mode-step 1.05)

(use-package doom-themes
  :defer t)
;; t prevents prompt on entry
(load-theme 'doom-gruvbox t)

;; defauly encoding
(set-default-coding-systems 'utf-8)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(doom-themes elfeed mpv vterm orderless helpful marginalia vertico openwith dired-collapse all-the-icons-dired dired-single undo-tree use-package cider clojure-mode magit gruber-darker-theme)))
(custom-set-faces)
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
