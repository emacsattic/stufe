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
;; * Functions to be able to control Xmms from 
;; * Emacs (cool !!!)
;; *
;; *************************************************



;; Variable to be able to decide if Xmms is running
;; or not. We assume it is running when we run Emacs
(setq stufe-music-running t)


(defun stufe-xmms-next ()
  "Next music in the play list"
  (interactive)
  (shell-command "xmms -f" nil nil))


(defun stufe-xmms-last ()
  "Last music in the play list"
  (interactive)
  (shell-command "xmms -r" nil nil))


(defun stufe-xmms-pause ()
  "Pause the current music"
  (interactive)
  (shell-command "xmms -u" nil nil)
  (setq stufe-music-running nil))


(defun stufe-xmms-play ()
  "Play the current music"
  (interactive)
  (shell-command "xmms -p" nil nil)
  (setq stufe-music-running t))


(defun stufe-xmms-change ()
  "Play/stop the current music"
  (interactive)
  (if stufe-music-running
      (stufe-xmms-pause)
    (stufe-xmms-pause)))
    

;; Define global key to bind with this functions
(global-set-key [(control shift j)] `stufe-xmms-last)
(global-set-key [(control shift k)] `stufe-xmms-change)
(global-set-key [(control shift l)] `stufe-xmms-next)


;; Add the items in the stufe menu
(defvar stufe-menu-xmms-context
  nil
  "Context of the xmms menu")


(setq stufe-menu-xmms-context
      (stufe-add-menu-item-group "Xmms"))
(stufe-add-menu-item stufe-menu-xmms-context
		       "Last music" 
		       'stufe-xmms-last)
(stufe-add-menu-item stufe-menu-xmms-context
		       "Next music" 
		       'stufe-xmms-next)
(stufe-add-menu-item stufe-menu-xmms-context
		       "Play/Stop music" 
		       'stufe-xmms-change)


