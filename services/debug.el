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
;; * Function to debug programs
;; *
;; *************************************************


(setq stufe-debug-command "gdb")
(setq stufe-debug-buffer-name "*gud*")


(defun stufe-get-debug-buffer ()
  "Return the buffer used for debugging"
  (get-buffer stufe-debug-buffer-name))

(defun stufe-get-debug-process ()
  "Return the process used for debugging"
  (get-buffer-process (stufe-get-debug-buffer)))

(defun stufe-kill-debug-window ()
  "Kill the buffer used to debug"
  (if (stufe-get-debug-buffer)
      (kill-buffer (stufe-get-debug-buffer))))

(defun stufe-get-debug-window ()
  "Get the current window used to debug"
  (if (stufe-get-debug-buffer)
      (get-buffer-window (stufe-get-debug-buffer))))

(defun stufe-launch-debug-window ()
  "Find a place for the debug buffer"
  (let* ((debug-window (stufe-get-debug-window))
	 (compilation-window (stufe-get-compilation-window)))
    (stufe-kill-debug-window)
    (if (and (not debug-window) compilation-window)
	(setq debug-window (split-window compilation-window nil t)))
    (if debug-window
	(set-window-buffer debug-window (get-buffer-create stufe-debug-buffer-name))))
  (save-selected-window
    (gdb (format "%s %s"
		 stufe-debug-command 
		 (if stufe-working-folder
		     (format "--cd=%s" stufe-working-folder)
		   "")))))

(defun stufe-debug-project () 
  "Start the debug mode"
  (interactive)
  (if (stufe-get-debug-process)
      (stufe-debug-continue-program)
    (progn
      (stufe-launch-debug-window)
      (save-current-buffer
	(stufe-load-project-file)
	(stufe-debug-add-main-breakpoint)
	(stufe-debug-run-program)
	(mapcar (lambda (breakpoint) (stufe-debug-add-breakpoint breakpoint))
		stufe-breakpoint-list)
	(stufe-debug-continue-program)))))


(defun stufe-debug-watch-variable-at-point ()
  (interactive)
  "Watch the variable where the cursor is on"
  (stufe-debug-print-variable (thing-at-point 'word)))


;; *************************************************
;; * 
;; * Command to send to the debugger program
;; *
;; *************************************************

(defun stufe-load-project-file ()
  "Load the project file into the debugger"
  (stufe-send-debug-command
   (concat "file " 
	   (stufe-makefile-get-value (stufe-project-makefile-path)
				       "PROJECT"))))


(defun stufe-debug-run-program ()
  "Start the program in the debugger"
  (stufe-send-debug-command "run"))


(defun stufe-debug-continue-program ()
  "Start the program in the debugger"
  (stufe-send-debug-command "continue"))


(defun stufe-debug-print-variable (variable)
  "Print a variable in the debugger"
  (stufe-send-debug-command (concat "print " variable)))


(defun stufe-debug-add-main-breakpoint ()
  "Add a breakpoint on the beginning of the main function"
  (stufe-send-debug-command "break main"))


(defun stufe-send-debug-command (command)
  "Send a debug command to the debugger"
  (message "Debug command: %s" command)
  (save-current-buffer
    (process-send-string (stufe-get-debug-buffer) (concat command "\n"))))


(defun stufe-debug-add-breakpoint (breakpoint)
  (stufe-send-debug-command 
   (format "break %s:%s"
	   (stufe-breakpoint-get-file breakpoint)
	   (stufe-breakpoint-get-line breakpoint))))

(defun stufe-debug-remove-breakpoint (breakpoint)
  (stufe-send-debug-command 
   (format "clear %s:%s"
	   (stufe-breakpoint-get-file breakpoint)
	   (stufe-breakpoint-get-line breakpoint))))
	   
(defun stufe-debug-kill-debugger ()
  (interactive)
  (if (stufe-get-debug-process)
      (kill-process (stufe-get-debug-process))))

;; *************************************************
;; * 
;; * Key bindings to debug
;; *
;; *************************************************

(global-set-key [(f10)] 'gud-next)
(global-set-key [(f11)] 'gud-step)
(global-set-key [(shift f10)] 'gud-finish)
(global-set-key [(shift f5)] 'stufe-debug-kill-debugger)
(global-set-key [(f8)] 'stufe-debug-watch-variable-at-point)