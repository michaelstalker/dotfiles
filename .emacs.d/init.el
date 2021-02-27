(require 'package)

(add-to-list 'package-archives
	           '("melpa-stable" . "https://stable.melpa.org/packages/")
             t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")
             t)
(package-initialize)

(setq quelpa-update-melpa-p nil)
(unless (package-installed-p 'quelpa)
    (with-temp-buffer
      (url-insert-file-contents "https://github.com/quelpa/quelpa/raw/master/quelpa.el")
      (eval-buffer)
      (quelpa-self-upgrade)))

;; Install use-package and the quelpa handler
(quelpa '(use-package :fetcher github :repo "jwiegley/use-package" :stable t))
(quelpa '(quelpa-use-package :fetcher github :repo "quelpa/quelpa-use-package"))
(require 'quelpa-use-package)
(setq use-package-always-ensure t)
(quelpa-use-package-activate-advice)

(use-package alchemist
  :ensure t)

(use-package auto-complete
  :ensure t

  :config
  (ac-config-default))

(use-package company
  :ensure t

  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package ac-alchemist
  :ensure t)

(use-package cider
  :ensure t)

(use-package elixir-mode
  :ensure t)

(use-package magit
  :ensure t)

(use-package neotree
  :ensure t

  :config
  (add-to-list 'load-path "/some/path/neotree")
  (require 'neotree)
  (global-set-key [f8] 'neotree-toggle))

(use-package smartparens
  :ensure t

  :config
  (sp-use-smartparens-bindings)
  (show-smartparens-global-mode)
  (smartparens-global-strict-mode)
  (require 'smartparens-config)
  (set-face-background 'sp-show-pair-match-face "deep sky blue")
  (set-face-foreground 'sp-show-pair-match-face "white"))

;; Project search
(use-package projectile
  :ensure t

  :config
  (projectile-global-mode)
  ;; (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package rainbow-delimiters
  :ensure t

  :config
  (require 'rainbow-delimiters)
  (show-paren-mode 1))

(use-package ujelly-theme
  :ensure t

  :config
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
  (load-theme 'ujelly t)
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(rainbow-delimiters-depth-1-face ((t (:foreground "red1"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "yellow1"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "green1"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "cyan1"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "magenta1"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "red1"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "yellow1"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "green1"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "cyan1"))))))

(use-package web-mode
  :ensure t

  :config
  (require 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode)))

;; Tmux
(defadvice terminal-init-screen
  ;; The advice is named `tmux', and is run before `terminal-init-screen' runs.
  (before tmux activate)
  ;; Docstring.  This describes the advice and is made available inside emacs;
  ;; for example when doing C-h f terminal-init-screen RET
  "Apply xterm keymap, allowing use of keys passed through tmux."
  ;; This is the elisp code that is run before `terminal-init-screen'.
  (if (getenv "TMUX")
      (let ((map (copy-keymap xterm-function-map)))
	(set-keymap-parent map (keymap-parent input-decode-map))
	(set-keymap-parent input-decode-map map))))

;; Emacs backup behavior
(setq
 backup-by-copying t ; don't clobber symlinks
 backup-directory-alist `(("." . "~/tmp")) ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t ; use versioned backups
 auto-save-file-name-transforms `((".*" "~/tmp" t)))

;; IDO
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ; enable fuzzy matching

;; Share Mac OS X clipboard
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; Emacs window appearance
;; Remove menu bar
(menu-bar-mode -1)
(line-number-mode)
(column-number-mode)
(global-linum-mode)

;; Indentation
;;; Use spaces instead of tabs when auto-indenting
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq js-indent-level 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq css-indent-offset 2)

;; Blueprint mode
(autoload 'apib-mode "apib-mode"
  "Major mode for editing API Blueprint files" t)
(add-to-list 'auto-mode-alist '("\\.apib\\'" . apib-mode))

;; Clojure
(defun customize-clojure-mode ()
  (turn-on-smartparens-strict-mode)
  (rainbow-delimiters-mode))

(add-hook 'clojure-mode-hook 'customize-clojure-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(eval-after-load 'clojure-mode
  '(define-clojure-indent
     (defroutes 'defun)
     (context 'defun)
     (GET 'defun)
     (POST 'defun)
     (PUT 'defun)
     (DELETE 'defun)
     (HEAD 'defun)
     (ANY 'defun)))

;; Figwheel
(setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")

;; Elixir
(add-hook 'elixir-mode-hook (lambda () (alchemist-mode) (rainbow-delimiters-mode)))
;; (eval-after-load 'flycheck
;;   '(flycheck-credo-setup))
;; (add-hook 'elixir-mode-hook 'flycheck-mode)

;; Scheme
;; (setq slime-contribs '(slime-fancy)) ; Try this if you install SLIME
(add-hook 'scheme-mode-hook (lambda () (rainbow-delimiters-mode)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3a9f65e0004068ecf4cf31f4e68ba49af56993c20258f3a49e06638c825fbfb6" default))
 '(package-selected-packages
   '(web-mode ujelly-theme smartparens rainbow-delimiters quelpa-use-package projectile neotree magit cider ac-alchemist)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "red1"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "yellow1"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "green1"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "cyan1"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "magenta1"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "red1"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "yellow1"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "green1"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "cyan1")))))
