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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
