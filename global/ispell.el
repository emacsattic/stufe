;; *************************************************
;; * 
;; * Few bindings for ispell
;; *
;; *************************************************


;; Use the french dictionary by default
;; (ispell-change-dictionary "francais")


;; Bindings keys on handy functions
(global-set-key [(f3)] 'ispell-word)
(global-set-key [(meta f3)] 'ispell-buffer)


;; Items menu
(defvar stufe-menu-spell-check-context
  nil
  "Context of the spell check menu")

(setq stufe-menu-spell-check-context
      (stufe-add-menu-item-group "Spell check"))

(stufe-add-menu-item stufe-menu-spell-check-context 
		       "Buffer spell check" 
		       'ispell-buffer)
(stufe-add-menu-item stufe-menu-spell-check-context 
		       "Word spell check" 
		       'ispell-word)

