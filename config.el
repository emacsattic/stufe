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
;; * Settings for stufe
;; *
;; *************************************************


(defun load-config ()
  (interactive)
  (load-file (expand-file-name "config.el" stufe-root)))


(defvar stufe-mouse-scroll-step
  3
  "Value representing the number of step scrolled when using 
the mouse wheel")


(defvar stufe-projects-default-path
  nil
  "The default path for creating projects")
(setq stufe-projects-default-path "~/")


(defvar stufe-recettes-default-path
  nil
  "RÅÈpertoire par dÅÈfaut des recettes de cuisine")
(setq stufe-recettes-default-path "~/recettes")


(defvar stufe-debug-size-window
  10
  "Default size for the debug window")


(defvar stufe-image-folder
  ""
  "Default folder for stufe images")
(setq stufe-image-folder (expand-file-name "pixmaps" stufe-root))


(defvar stufe-template-model-file-folder
  nil
  "List of folders containing template model file")
(setq stufe-template-model-file-folder   
      (list (expand-file-name "template/model" stufe-root)))


(defvar stufe-working-folder
  nil
  "Working folder used to compile and using cvs command")

(defvar stufe-make-command
  "make"
  "The make command for stufe")

;; Set the default folder for backup files
(setq backup-directory-alist '(("." . "/tmp")))


;; Set the default size of the compilation window
(setq compilation-window-height 10)

;; Set completion case sensitive by default
(setq dabbrev-upcase-means-case-search 'true)

;; Set default ispell dictionnary
(defvar stufe-default-ispell-langage 
  "")