(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/")
	     t)

(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

(setq quelpa-update-melpa-p nil)
(unless (require 'quelpa nil t)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

;; install use-package and the quelpa handler
(quelpa '(use-package :fetcher github :repo "jwiegley/use-package" :stable t))
(quelpa '(quelpa-use-package :fetcher github :repo "quelpa/quelpa-use-package"))
(require 'quelpa-use-package)
(setq use-package-always-ensure t)
(quelpa-use-package-activate-advice)

;; Parens
(use-package smartparens
  :config
  (sp-use-smartparens-bindings)
  (show-smartparens-global-mode)
  (smartparens-global-strict-mode)
  (require 'smartparens-config)
  (set-face-background 'sp-show-pair-match-face "deep sky blue")
  (set-face-foreground 'sp-show-pair-match-face "white"))

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

(require 'rainbow-delimiters)
(show-paren-mode 1)

(defun customize-clojure-mode ()
  (turn-on-smartparens-strict-mode)
  (rainbow-delimiters-mode))

(add-hook 'clojure-mode-hook 'customize-clojure-mode)

;; Projectile project search
(projectile-global-mode)

;; Strip whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("b7ba8bd70d2c954e326144c5bf11eecffd55683dfa76aa16bc53572a6184bc1d" "5c6d40ef6e7bbe9e83dc0e32db794c7e9a6a0d9eb7d6a874aaf9744c053842b4" "47d9be69b3f83450d9e55f08ba84a1199348ccc7f7eb0c11c56f3626e7dc9afd" default))))

;; Color themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'ujelly)
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

;; Visual settings
(use-package visual-settings
	     :ensure nil
	     :init
	     (line-number-mode)
	     (column-number-mode)
	     (global-linum-mode))

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

(setq slime-contribs '(slime-fancy))
(setq geiser-active-implementations '(racket))

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

;; NeoTree
(add-to-list 'load-path "/some/path/neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

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

;; Remove menu bar
(menu-bar-mode -1)

(require 'slim-mode)

;; auto-complete
(ac-config-default)

(setq js-indent-level 2)
(setq web-mode-css-indent-offset 2)

;; Blueprint mode
(autoload 'apib-mode "apib-mode"
  "Major mode for editing API Blueprint files" t)
(add-to-list 'auto-mode-alist '("\\.apib\\'" . apib-mode))

;; Figwheel
(setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
