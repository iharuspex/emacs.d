;;; init-packages.el --- Packages setup
;;; Commentary:
;;;=============================================================================
;;; Code:

(require 'package)
(add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; install use-package if needed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; install packages
;;=============================================================================
;; helm
(use-package helm
             :ensure t
             :config
             (progn
               (helm-mode 1)
               (use-package helm-config
                            :config
                            (progn
                              (global-set-key (kbd "M-x") 'helm-M-x)
                              (global-set-key (kbd "C-x C-f") 'helm-find-files)
                              (define-key helm-map (kbd "<tab>")
                                'helm-execute-persistent-action)
                              ))))

(use-package magit
             :ensure t
             :defer t
             :bind ("C-x g" . magit-status))

(use-package company
  :ensure t
  :config
  (setq company-tooltip-limit 20)
  (setq company-idle-delay .3)
  (setq company-minimum-prefix-length 1)
  (setq company-minimum-delay 0)
  (setq company-begin-commands '(self-insert-command))
  (global-company-mode t))

(use-package flycheck
  :ensure t
  :config
  (progn (global-flycheck-mode)))

;; TODO: move the package settings for C/C++ to separate file!

(use-package irony
  :ensure t
  :config
  (progn
    ;; if irony server was never installed - install it
    (unless (irony--find-server-executable) (call-interactively
                                             #'irony-install-server))
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)

    ;; use compilation database first, clang_complete as fallback
    (setq-default irony-cdb-compilation-databases '(irony-cdb-libclang
                                                    irony-cdb-clang-complete))
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    ))

(use-package company-irony
  :ensure t
 ;; :require company irony
  :config
  (progn
    (eval-after-load 'company '(add-to-list 'company-backends 'company-irony))))

(use-package flycheck-irony
  :ensure t
  ;;:require flycheck irony
  :config
  (progn
    (eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook
                                          #'flycheck-irony-setup))))

(use-package irony-eldoc
  :ensure t
 ;; :require eldoc irony
  :config
  (progn
    (add-hook 'irony-mode-hook #'irony-eldoc)))

(use-package projectile
  :ensure t
  :config
  (progn (projectile-mode)))

(use-package sr-speedbar
  :ensure t
  :defer t
  :init
  (setq sr-speedbar-right-side nil)
  (setq speedbar-show-unknown-files t)
  (setq sr-speedbar-width 35)
  (setq sr-speedbar-max-width 35)
  (setq speedbar-use-images t)
  (setq speedbar-initial-expansion-list-name "quick buffers")
  (setq speedbar-initial-expansion-list-name "files"))

(use-package neotree
  :bind ([f8] . neotree-toggle)
  :init (setq neo-window-width 35)
  :config (setq neo-smart-open nil))


(provide 'init-packages)

;;; init-packages.el ends here
