;;; package --- org-config.el
;;; Commentary:
;;; Code:
;; -*- lexical-binding: t; -*-
(custom-set-variables
 '(org-directory "~/Documents/org/"))

(require 'org-install)
(require 'org)
(require 'use-package)
(require 'treemacs)
(require 'org-clockify)

;; Additional packages
(use-package adaptive-wrap
  :ensure t
  :config
  (setq adaptive-wrap-extra-indent 2)
  (add-hook 'org-mode-hook 'adaptive-wrap-prefix-mode))

(setq-default fill-column 120)
(setq-default display-fill-column-indicator-column 120)
(add-hook 'org-mode-hook 'display-fill-column-indicator-mode)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Visualize habit progress
;;(setq org-habit-show-habits 1)

;; Enable the soft indents
(add-hook 'org-mode-hook 'org-indent-mode)
;; Enable visual wrap
(add-hook 'org-mode-hook 'visual-line-mode)

(defun open-org-directory ()
  "Open ~/Documents/org in Dired mode."
  (interactive)
  ;; (dired "~/Documents/org/"))
  (unless (treemacs-current-visibility)
    (treemacs))
  (treemacs-select-window))

(global-set-key (kbd "C-c o") 'open-org-directory)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; Agenda config
(setq org-agenda-files (file-expand-wildcards "~/Documents/org/4-tasks/*.org"))

;; Clockify integration
(setq org-clockify-api-key (getenv "CLOCKIFY_API_KEY"))

;; Insert header to org-files
(auto-insert-mode 1)

(define-auto-insert
  '("\\.org\\'" . "Org-mode file template")
  (lambda ()
       (unless (string-match-p "^*" (buffer-name))
	 (insert
	   "#+TITLE: " (read-string "Title: ") "\n"
	   "#+AUTHOR: " user-full-name "\n"
	   "#+DATE: " (format-time-string "%d-%m-%Y") "\n"
	   "#+STARTUP: overview\n"
	   "\n* "))))

(provide 'org-config)
;;; org-config.el ends here
