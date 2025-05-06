;;; package --- org-config.el
;;; Commentary:
;;; Code:
;; -*- lexical-binding: t; -*-
(custom-set-variables
 '(org-directory "~/Documents/org/"))

(require 'org-install)
(require 'org)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(defun open-org-directory ()
  "Open ~/Documents/org in Dired mode."
  (interactive)
  (dired "~/Documents/org/"))

(global-set-key (kbd "C-c o") 'open-org-directory)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; Agenda config
(setq org-agenda-files (file-expand-wildcards "~/Documents/org/agenda/*.org"))

;; Insert header to org-files
(auto-insert-mode t)

(define-auto-insert
  '("\\.org\\'" . "Org-mode file")
  `(,(lambda ()
       (unless (string-match-p "^*" (buffer-name))
	 '("Title: "
	   "#+TITLE: " (read-string "Title: ") "\n"
	   "#+AUTHOR: " user-full-name "\n"
	   "#+DATE: " (format-time-string "%d-%m-%Y") "\n"
	   "#+STARTUP: overview\n"
	   "\n* ")))))

(provide 'org-config)
;;; org-config.el ends here
