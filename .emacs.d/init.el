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

;(use-package smartparens
;  :config
;  (sp-use-smartparens-bindings)
;  (show-smartparens-global-mode)
;  (smartparens-global-strict-mode)
;  (require 'smartparens-config)
;  (set-face-background 'sp-show-pair-match-face "deep sky blue")
;  (set-face-foreground 'sp-show-pair-match-face "white"))

;; Projectilef project search
(projectile-global-mode)

;; Strip whitespace
(setq whitespace-action '(auto-cleanup))

;; Tab 2 spaces in CoffeeScript files
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("47d9be69b3f83450d9e55f08ba84a1199348ccc7f7eb0c11c56f3626e7dc9afd" default))))

;; Color themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'ujelly)

;; Visual settings
(use-package visual-settings
	     :ensure nil
	     :init
	     (line-number-mode)
	     (column-number-mode)
	     (global-linum-mode))

;; Parens
(require 'rainbow-delimiters)
(require 'paredit)

(defun customize-clojure-mode ()
  (paredit-mode 1)
  (local-set-key "\M-[1;2d" 'paredit-backward-slurp-sexp)
  (local-set-key "\M-[\ c" 'paredit-forward-slurp-sexp))

(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook 'customize-clojure-mode)
(show-paren-mode 1)
;(define-key paredit-mode-map (kbd "M-[") nil)

;; Share OS X clipboard
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-cnnection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

(setq slime-contribs '(slime-fancy))
(setq geiser-active-implementations '(racket))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
