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
;; * Function to manipulate breakpoint structure
;; *
;; *************************************************


(defvar stufe-breakpoint-list
  nil
  "List used to store breakpoint")


(defun stufe-create-new-breakpoint (file line overlay)
  "Create an overlay from a breakpoint and return the new breakpoint
with the overlay information"
  (list file line overlay))


(defun stufe-breakpoint-get-file (breakpoint)
  "Get the file defined for a breakpoint"
  (car breakpoint))


(defun stufe-breakpoint-get-line (breakpoint)
  "Get the file defined for a breakpoint"
  (cadr breakpoint))


(defun stufe-breakpoint-get-overlays (breakpoint)
  "Get the file defined for a breakpoint"
  (car (cddr breakpoint)))


;; *************************************************
;; * 
;; * Function to set breakpoints in source code
;; *
;; *************************************************


(defun stufe-find-listed-breakpoint (breakpoint)
  "Find a breakpoint in the breakpoint list"
  (let ((breakpoint-file (stufe-breakpoint-get-file breakpoint))
	(breakpoint-line (stufe-breakpoint-get-line breakpoint))
	(evaluation nil)
	(current-element stufe-breakpoint-list))
    (while (and current-element (not evaluation))
      (progn 
	(setq evaluation 
	      (and (string= breakpoint-file 
			    (stufe-breakpoint-get-file (car current-element)))
		   (equal breakpoint-line
			  (stufe-breakpoint-get-line (car current-element)))))
	(if (not evaluation)
	    (setq current-element (cdr current-element)))))
    (if evaluation
	(car current-element)
      nil)))



(defun stufe-set-breakpoint ()
  (interactive)
  (let* ((command-function (stufe-get-debugger-function "Stufe Set BreakPoint"))
	 (current-breakpoint 
	  (stufe-create-new-breakpoint (if command-function
					   (funcall command-function (current-buffer))
					 (buffer-name))
				       (stufe-get-current-line)
				       'nil))
	 (listed-breakpoint 
	  (stufe-find-listed-breakpoint current-breakpoint)))
    (if listed-breakpoint
	(progn
	  (setq stufe-breakpoint-list 
		(delete listed-breakpoint stufe-breakpoint-list))
	  (if (stufe-get-debug-process)
	      (stufe-debug-remove-breakpoint listed-breakpoint))
	  (delete-overlay 
	   (stufe-breakpoint-get-overlays listed-breakpoint)))
      (let* ((overlay (make-overlay (stufe-get-beginning-of-line)
				    (stufe-get-end-of-line)))
	     (new-breakpoint 
	      (stufe-create-new-breakpoint 
	       (stufe-breakpoint-get-file current-breakpoint)
	       (stufe-breakpoint-get-line current-breakpoint)
	       overlay)))
 	    (overlay-put overlay 'before-string "¤")
	    (setq stufe-breakpoint-list 
		  (cons new-breakpoint stufe-breakpoint-list))
	    (if (stufe-get-debug-process)
		(stufe-debug-add-breakpoint current-breakpoint)))))
      (message (format "%s" stufe-breakpoint-list)))


