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




(setq stufe-run-debugger nil)
(setq stufe-debug-function nil)
(setq stufe-debug-command nil)
(setq stufe-debugger-command nil)
(setq stufe-debug-command-table nil)
(setq stufe-debug-get-main-breakpoint nil)
(setq stufe-debug-buffer-name nil)


(defun stufe-get-debug-buffer ()
  "Return the buffer used for debugging"
  (if stufe-debug-buffer-name
      (get-buffer stufe-debug-buffer-name)
    nil))


(defun stufe-get-debug-process ()
  "Return the process used for debugging"
  (let ((debug-buffer (stufe-get-debug-buffer)))
    (if debug-buffer
	(get-buffer-process debug-buffer)
      nil)))


(defun stufe-kill-debug-window ()
  "Kill the buffer used to debug"
  (if (stufe-get-debug-buffer)
      (kill-buffer (stufe-get-debug-buffer))
    nil))


(defun stufe-get-debug-window ()
  "Get the current window used to debug"
  (if (stufe-get-debug-buffer)
      (get-buffer-window (stufe-get-debug-buffer))
    nil))


(defun stufe-launch-debug-window ()
  "Find a place for the debug buffer"
  (let* ((current-buffer (current-buffer))
	 (debug-window (stufe-get-debug-window))
	 (compilation-window (stufe-get-compilation-window)))

    (stufe-kill-debug-window)

    (if (not debug-window)
	(if compilation-window
	    (setq debug-window (split-window compilation-window nil t))
	  (setq debug-window (split-window (selected-window) nil nil))))

    (set-window-buffer debug-window (get-buffer-create stufe-debug-buffer-name))
    (save-selected-window
      (funcall stufe-debug-function
	       (funcall stufe-run-debugger stufe-debug-command)))

    (set-window-buffer (selected-window) current-buffer)))


(defun stufe-debug-project () 
  "Start the debug mode"
  (interactive)
  (save-some-buffers 1)
  (if (stufe-get-debug-process)
      (gud-cont nil)
    (progn
      (stufe-launch-debug-window)
      (let ((command-function (stufe-get-debugger-function "Debug project")))
	(if command-function
	    (funcall command-function))))))


(defun stufe-debug-watch-variable-at-point ()
  (interactive)
  "Watch the variable where the cursor is on"
  (stufe-debug-print-variable (thing-at-point 'word)))



;; *************************************************
;; * 
;; * Command to send to the debugger program
;; *
;; *************************************************

(defun stufe-get-debugger-function (debugger-function)
  "Get the corresponding debugger (gdb, jdb) command for a debug command"
  (cadr (assoc debugger-function stufe-debug-command-table)))


(defun stufe-load-project-file ()
  "Load the project file into the debugger"
  (let ((command-function (stufe-get-debugger-function "Load project")))
    (if command-function
	(funcall command-function
		 ))))


(defun stufe-debug-print-variable (variable)
  "Print a variable in the debugger"
  (let ((command-function (stufe-get-debugger-function "Print variable")))
    (if command-function
	(funcall command-function variable))))


(defun stufe-debug-add-breakpoint (breakpoint)
  (let ((command-function (stufe-get-debugger-function "Add breakpoint")))
    (if command-function
	(funcall command-function
		 (stufe-breakpoint-get-file breakpoint)
		 (stufe-breakpoint-get-line breakpoint)))))


(defun stufe-debug-remove-breakpoint (breakpoint)
  (let ((command-function (stufe-get-debugger-function "Clear breakpoint")))
    (if command-function
	(funcall command-function
		 (stufe-breakpoint-get-file breakpoint)
		 (stufe-breakpoint-get-line breakpoint)))))

	   
(defun stufe-debug-kill-debugger ()
  (interactive)
  (if (stufe-get-debug-process)
      (kill-process (stufe-get-debug-process))))

(defvar stufe-gdb-tty
  nil
  "tty to use for debugging programm")

(defun stufe-debug-tty (ttyfile)
  "Set the tty for the next gdb call"
  (interactive "stty file: ")
  (setq stufe-gdb-tty ttyfile))


;; *************************************************
;; * 
;; * Key bindings to debug
;; *
;; *************************************************

(global-set-key [(f10)] 'gud-next)
(global-set-key [(f11)] 'gud-step)
(global-set-key [(shift f9)] 'gud-tbreak)
(global-set-key [(shift f10)] 'gud-finish)
(global-set-key [(shift f5)] 'stufe-debug-kill-debugger)
(global-set-key [(f8)] 'stufe-debug-watch-variable-at-point)