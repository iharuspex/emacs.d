;; init-key-bindings.el
;;-------------------------------------------------------
;; Key bindings
;;-------------------------------------------------------


(global-set-key (kbd "C-x /") 'shell-command) ;; run shell
;; применять клавишу super как meta
;; (setq x-super-keysym 'meta) ;; set meta key

;; перемещение между окнами с помощью S-arrows
(windmove-default-keybindings)

(provide 'init-key-bindings)
