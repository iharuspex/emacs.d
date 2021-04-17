;; Packages setup
;;=============================================================================

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
             
(provide 'init-packages)
