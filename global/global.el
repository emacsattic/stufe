;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copyright (C) 2000-2002 the Stufe project

;; This file is part of Stufe.

;; Stufe is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; Stufe is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(setq Man-notify-method 'newframe)
(global-set-key [(f1)] (lambda () (interactive) (man (current-word))))


;; Items for useful bindings
(stufe-add-menu-item (stufe-get-stufe-context)
		       "Run command..." 
		       'compile)













