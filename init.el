;; init.el

;; Check version
;;=============================================================================
(let ((minver "25.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "26.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

;; Packages
;;=============================================================================
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))

;; Appearance setup
;;=============================================================================
;; set theme
(load-theme 'wombat)
;; set frame name, just because =)
(setq frame-title-format "GNU Emacs")
;; interactive mode
(ido-mode)
;; show column number
(column-number-mode)
;; set line numbers
(global-linum-mode t)
;; y-or-n
(fset 'yes-or-no-p 'y-or-n-p)
;; highlight corresponding parentheses when cursor is on one
(show-paren-mode t)
;; insert parenthesis by pair
(electric-pair-mode 1)
;; highlight tabs
(setq-default highlight-tabs t)
;; disable toolbars
(tool-bar-mode -1)
;;(menu-bar-mode -1)
;; disable scrollbar
(scroll-bar-mode -1)

;; display time, date and battery status
(setq display-time-day-and-date t 
      display-time-24hr-format t 
      display-time-interval 10
      display-time-default-load-average nil)
(display-time)
(display-battery-mode 0)

;; Indentation setup
;;=============================================================================
(setq-default indent-tabs-mode nil) ; never use tab characters for indentation
(setq tab-width 4)

;; For C
;; TODO: replace to separate files!
(defun my-c-mode-hook ()
  (setq c-basic-offset 4
	c-indent-level 4
	c-default-style "stroustrup"))
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

;; Enable auto-fill
;;=============================================================================
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook
          '(lambda() (set-fill-column 80)))
(add-hook 'prog-mode-hook 'turn-on-auto-fill)
(add-hook 'prog-mode-hook
          '(lambda() (set-fill-column 80)))

;; Load external scripts
;;=============================================================================
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-benchmarking) ;; Measure startup time
(require 'init-key-bindings) ;; Key bindings

