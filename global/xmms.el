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


(defvar stufe-xmms-command 
  "xmms"
  "Command used to run the mp3 player")

(defvar stufe-xmms-process
  "*Xmms-Process*"
  "Name used for the xmms process")

(defvar stufe-aumix-command 
  "aumix"
  "Command modify the volume")

(defvar stufe-aumix-process
  "*Aumix-Process*"
  "Name used for the aumix process")


(defun stufe-run-xmms (option)
  "Run a xmms command with an option. Display the output of the
command in a buffer."
  (start-process stufe-xmms-process
		 nil 
		 stufe-xmms-command  
		 option))


(defun stufe-run-aumix (option)
  "Run an aumix command with an option. Display the output of the
command in a buffer."
  (message "%s %s %s" stufe-aumix-process stufe-aumix-command option)
  (start-process stufe-aumix-process
		 nil 
		 stufe-aumix-command  
		 option))


(defun stufe-xmms-next ()
  "Next music in the play list"
  (interactive)
  (stufe-run-xmms "-f"))


(defun stufe-xmms-last ()
  "Last music in the play list"
  (interactive)
  (stufe-run-xmms "-r"))


(defun stufe-xmms-change ()
  "Play/stop the current music"
  (interactive)
  (stufe-run-xmms "-t"))

(defun stufe-aumix-up ()
  "Change the current volume of the mixer"
  (interactive)
  (stufe-run-aumix "-w+5"))

(defun stufe-aumix-down ()
  "Change the current volume of the mixer"
  (interactive)
  (stufe-run-aumix "-w-5"))

;; Define global key to bind with these functions
(global-set-key [(control J)] `stufe-xmms-last)
(global-set-key [(control K)] `stufe-xmms-change)
(global-set-key [(control L)] `stufe-xmms-next)
(global-set-key [(control M)] `stufe-aumix-down)
(global-set-key [(control P)] `stufe-aumix-up)


;; Add the items in the stufe menu
(defvar stufe-menu-xmms-context
  nil
  "Context of the xmms menu")


(setq stufe-menu-xmms-context
      (stufe-add-menu-item-group "Xmms" nil "Grep"))
(stufe-add-menu-item stufe-menu-xmms-context
		       "Increase Pcm" 
		       'stufe-aumix-up)
(stufe-add-menu-item stufe-menu-xmms-context
		       "Decrease Pcm" 
		       'stufe-aumix-down)
(stufe-add-menu-item stufe-menu-xmms-context
		       "Play/Stop music" 
		       'stufe-xmms-change)
(stufe-add-menu-item stufe-menu-xmms-context
		       "Next music" 
		       'stufe-xmms-next)
(stufe-add-menu-item stufe-menu-xmms-context
		       "Last music" 
		       'stufe-xmms-last)


