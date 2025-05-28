;;; init.el --- My Emacs configuraion
;;; Commentary:

;;; Code:
(message "Hello, Emacs!")

;; User-defined Lisp code
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Appearance setup
;; ====================================
;;
;; show column mode
(column-number-mode)
;; set line numbers
(global-linum-mode t)
;; y-or-n
(fset 'yes-or-no-p 'y-or-n-p)
;; highlight tabs
(setq-default highligh-tabs t)
;; start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; highlight parentheshis
(show-paren-mode t)
;; pair-mode
(electric-pair-mode t)
;; move between windows using S-arrows
(windmove-default-keybindings)
;; hide toolbars
(tool-bar-mode -1)
;; enable winner-mode (history of window composition)
(winner-mode 1)
;; display buffer in tabs
(setq display-buffer-base-action '(display-buffer-in-tab))
;; fix emoji
(set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend)

;; Comment region using C-x /
(defun toggle-comment-region-or-line ()
  "Toggle comment for region or line."
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-line 1)))

(global-set-key (kbd "C-x /") 'toggle-comment-region-or-line)

(set-face-background 'show-paren-match (face-background 'highlight))
(set-face-foreground 'show-paren-match (face-foreground 'highlight))
(set-face-background 'show-paren-mismatch (face-background 'error))
(set-face-foreground 'show-paren-mismatch (face-foreground 'error))

;; Install MELPA repository
;; ====================================
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Install doom-themes
;; ====================================
(use-package doom-themes
	     :ensure t
	     :config
	     (load-theme 'doom-one t))

;; Instal useful packages
;; ====================================

;; magit
;; (use-package magit
;;   :ensure t
;;   :bind ("C-x g" . magit-status))

;; restart-emacs
(use-package restart-emacs
  :ensure t)

;; request
(use-package request
  :ensure t)

;; yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  :bind ("C-c y" . yas-expand))

;; reverse-im (fix the keyboard layout for Emacs hotkeys)
(use-package reverse-im
  :ensure t
  :demand t
  :config
  (reverse-im-activate "russian-computer")
  (setq reverse-im-char-fold t))

;; vterm
(use-package vterm
  :ensure t
  :config
  (setq vterm-always-compile-module t)
  (add-to-list 'display-buffer-alist
	       '((derived-mode . vterm-mode)
		 (display-buffer-in-direction)
		 (direction . bottom)
		 (window-height . 0.2))))

;; (defun launch-vterm ()
;;   "Open vterm."
;;   (interactive)
;;   (split-window-below)
;;   (other-window 1)
;;   (vterm))
;; (global-set-key (kbd "C-c t") 'launch-vterm)

;; Projectile
;; TODO move to separate file
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
  (global-set-key (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(require 'projectile)
(projectile-register-project-type 'alire '("alire.toml")
				  :project-file "alire.toml"
				  :compile "alr build"
				  :test "alr test"
				  :run "alr run")

;; CMake project
;; ====================================
(use-package cmake-mode
  :ensure t)

;; Treemacs config
(use-package treemacs
  :ensure t
  :defer nil
  :bind (("C-x t" . treemacs))
  :config
  (progn
    (setq treemacs-collapse-dirs 3
	  treemacs-display-in-side-window t
	  )
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)))
;; (treemacs-start-on-boot)

(require 'treemacs)
(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile)
  :config
  (add-hook 'projectile-after-switch-project-hook #'treemacs-add-and-display-current-project-exclusively))

(defun my/open-treemacs-if-dir ()
  "Asd."
  (when (and (file-directory-p default-directory)
             (delete-other-windows)
	     (treemacs)
	     (other-window 1))
    (treemacs-add-and-display-current-project-exclusively)))

(add-hook 'emacs-startup-hook #'my/open-treemacs-if-dir)
(add-hook 'projectile-after-switch-project-hook #'treemacs-add-and-display-current-project-exclusively)

;; Ada configuration
;; ====================================
;;(use-package ada-mode
;;	     :ensure t
;;	     :mode "\\.ads\\'" "\\.adb\\'")

;; Syntax checker
(use-package flycheck
  :ensure t
  :config (global-flycheck-mode))

;; Autocompletion
(use-package company
  :ensure t
  :config (global-company-mode))

;; Usefull functions (move to separate el file later)
;; ====================================
(use-package google-translate
  :ensure t)

(defun google-translate-english-to-russian-xxx (text)
  "Translate TEXT from English to Russian using Google Translate."
  (interactive)
  ;; (google-translate-translate "en" "ru" text))
  (let ((result (google-translate-translate "en" "ru" text)))
    (if (stringp (car result))
	(car result)
      (error "Translation error: %S" result))))
  
(defun org-translate-en-to-ru ()
  "Translate."
  (interactive)
  (unless (org-at-table-p)
    (user-error "Not in table!"))

  (let* ((english-word (string-trim (org-table-get-field 1)))
	 (translation (if (string-empty-p english-word)
			  ""
			(google-translate-english-to-russian english-word))))
    (org-table-next-field)
    (org-table-put-field nil translation)
    (org-table-previous-field)))

;; (load-file "~/.emacs.d/ada-project-setup.el")
(load-file "~/.emacs.d/org-config.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(ispell-dictionary nil)
 '(org-directory "~/Documents/org/")
 '(package-selected-packages '(request doom-themes use-package cmake-mode))
 '(safe-local-variable-values
   '((eval when
	   (featurep 'projectile)
	   (unless
	       (get-buffer "*project-term*")
	     (let
		 ((default-directory
		    (projectile-project-root)))
	       (split-window-below)
	       (other-window 1)
	       (shrink-window 10)
	       (projectile-run-term)
	       (rename-buffer "*project-term*")
	       (term-send-raw-string "cd $PWD && clear
")
	       (other-window -1)))
	   (add-hook 'focus-in-hook 'save-some-buffers)
	   (global-auto-revert-mode 1))
     (eval when
	   (featurep 'projectile)
	   (unless
	       (get-buffer "*project-term*")
	     (let
		 ((default-directory
		    (projectile-project-root)))
	       (split-window-below)
	       (shrink-window 10)
	       (other-window 1)
	       (projectile-run-term)
	       (rename-buffer "*project-term*")
	       (term-send-raw-string "cd $PWD && clear
")
	       (other-window -1)))
	   (add-hook 'focus-in-hook 'save-some-buffers)
	   (global-auto-revert-mode 1)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
