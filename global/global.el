;; *************************************************
;; * 
;; * Global declarations of stufe
;; *
;; *************************************************



;; Definition of the selection for this Windows
(custom-set-variables
 '(pc-select-meta-moves-sexps t)
 '(pc-select-selection-keys-only t)
 '(pc-selection-mode t nil (pc-select)))


;; No beep 
(setq visible-bell t)



;; Highlight for Search, replace and mouse selection
(setq search-highlight t)
(setq query-replace-highlight t)
(setq mouse-sel-retain-highlight t)

;; Highlight matching parenthesis
(show-paren-mode t)

;; Define the wheel mouse key
(define-key global-map [mouse-4]
  '(lambda () (interactive)
     (save-selected-window 
       (select-window (stufe-get-window-under-cursor))
       (scroll-down stufe-mouse-scroll-step))))
(define-key global-map [mouse-5]
  '(lambda () (interactive)
     (save-selected-window 
       (select-window (stufe-get-window-under-cursor))
       (scroll-up stufe-mouse-scroll-step))))

;; Handy bindings for the editor
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line) 
(global-set-key [(control tab)] 'other-window)
(global-set-key [(f4)] 'next-error)
(global-set-key [(control z)] 'undo)
(global-set-key [(f9)] 'compile)
(global-set-key [(control-l)] 'stufe-center-on-line)
(setq Man-notify-method 'newframe)
(global-set-key [(f1)] (lambda () (interactive) (man (current-word))))


;; Items for useful bindings
(stufe-add-menu-item (stufe-get-stufe-context)
		       "Reload stufe config" 
		       'load-config)
(stufe-add-menu-item (stufe-get-stufe-context)
		       "Run command..." 
		       'compile)
(stufe-add-menu-item (stufe-get-stufe-context)
		       "Center on line" 
		       'stufe-center-on-line)













