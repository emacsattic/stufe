;; *************************************************
;; * 
;; * Function to set breakpoints in source code
;; *
;; *************************************************


(defvar stufe-breakpoint-list
  nil
  "List used to store breakpoint")


(defun stufe-get-current-breakpoint ()
  (cons (buffer-name) (stufe-get-current-line)))


(defun stufe-breakpoint-get-file (breakpoint)
  "Get the file defined for a breakpoint"
  (car breakpoint))


(defun stufe-breakpoint-get-line (breakpoint)
  "Get the file defined for a breakpoint"
  (cdr breakpoint))


(defun stufe-set-breakpoint ()
  (interactive)
  (let ((current-breakpoint (stufe-get-current-breakpoint)))
    (if (not (member current-breakpoint
		     stufe-breakpoint-list))
	(progn 
	  (setq stufe-breakpoint-list 
		(cons current-breakpoint stufe-breakpoint-list))
	  (let ((overlay (make-overlay (stufe-get-beginning-of-line)
				       (stufe-get-end-of-line))))
	    (overlay-put overlay 'before-string "¤"))
	  (if (stufe-get-debug-process)
	      (stufe-debug-add-breakpoint current-breakpoint)))
      (progn 
	(setq stufe-breakpoint-list 
	      (delete current-breakpoint stufe-breakpoint-list))
	(if (stufe-get-debug-process)
	    (stufe-debug-remove-breakpoint current-breakpoint))
	(while (overlays-at (point))
	  (delete-overlay (car (overlays-at (point))))))))
    (message (format "%s" stufe-breakpoint-list))
    (redraw-display))



