;;; package --- org-config.el
;;; Commentary:
;;; Code:
;; -*- lexical-binding: t; -*-
(custom-set-variables
 '(org-directory "~/Documents/org/"))

(require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; (auto-insert-mode t)

;; (define-auto-insert
;;   '("\\.org\\'" . "Org-mode file")
;;   '("Title: "
;;     "#+TITLE: " (read-string "Title: ") "\n"
;;     "#+AUTHOR: " user-full-name "\n"
;;     "#+DATE: " (format-time-string "%d-%m-%Y") "\n"
;;     "#+STARTUP: overview\n"
;;     "\n* Note starts here:\n\n"))

(provide 'org-config)
;;; org-config.el ends here
