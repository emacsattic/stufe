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


(require 'easymenu)
(stufe-require-file "global/utils.el")


;; *************************************************
;; * 
;; * Function to deal with the items in the stufe 
;; * menu
;; *
;; *************************************************



;; *************************************************
;; * 
;; * Functions to deal with local menu
;; *
;; *************************************************

(defun stufe-menu-create-local (local-item)
  "Create a new (overwriting if it already exists) item in the menu bar"
  (easy-menu-define local (current-local-map) "Project menu" '("Project"))
  (easy-menu-add local (current-local-map)))


(defun stufe-menu-add-item-local (item-string real-binding &optional before)
  "Add an item in the last sub-menu (inside the stufe menu)"
  (easy-menu-add-item local '() (vector item-string real-binding t) before))
  

(defun stufe-menu-add-menubar-local ()
  "Add a menubar item in the current menu"
  (easy-menu-add-item local '() "--"))



;; *************************************************
;; * 
;; * Function to be able to get a context for an 
;; * item
;; *
;; *************************************************

(defun stufe-get-stufe-context ()
  "Return the stufe menu context"
  stufe-menu-context)

(defun stufe-get-context-item (base-context item-string)
  "Get the context of an item from base context"
  (vconcat base-context (vector (make-symbol item-string))))


;; *************************************************
;; * 
;; * Function to create a new item in a context
;; *
;; *************************************************

(defun stufe-add-menu-item (context item-string real-binding &optional before)
  "Add an item in the last sub-menu (inside the stufe menu)"
  (easy-menu-add-item stufe (reverse context) (vector item-string real-binding t) before))


(defun stufe-add-menu-item-group (item-string &optional context before)
  "Add a new group in the stufe menu"
  (ignore-errors (easy-menu-add-item stufe
				     (reverse context)
				     (easy-menu-create-menu item-string '())
				     before))
  (cons item-string context))

(easy-menu-define stufe global-map "Stufe menu" '("Stufe"))
(ignore-errors (easy-menu-add stufe global-map))



