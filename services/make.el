;; *************************************************
;; * 
;; * Functions to run the make command
;; *
;; *************************************************

(defun stufe-make-exec ()
  (interactive)
  (stufe-run-make "exec"))


(defun stufe-make-doc ()
  (interactive)
  (stufe-run-make "doc"))


(defun stufe-make-build ()
  (interactive)
  (stufe-run-make "all"))


(defun stufe-make-clean ()
  (interactive)
  (stufe-run-make "clean"))


(defun stufe-make-rebuild ()
  (interactive)
  (stufe-run-make "clean all"))


(setq stufe-compile-function 'compile)

(defun stufe-run-make (command)
  "Save all the buffers and run the make command"
  (interactive)
  (if (and (not stufe-working-folder)
	   (not (or (file-exists-p "makefile")
		    (file-exists-p "Makefile"))))
      (stufe-guess-project-makefile))
  (save-some-buffers 1)
  (funcall stufe-compile-function 
	   (stufe-run-in-work-folder stufe-make-command
				       command))
  (if (stufe-get-compilation-window)
      (save-selected-window
	(select-window (stufe-get-compilation-window))
	(goto-char (point-max)))))


(defun stufe-run-in-work-folder (command argument)
  (format "%s%s %s " 
	  (if stufe-working-folder
	      (format "cd %s; " stufe-working-folder)
	    "")
	  command
	  argument))

(defun stufe-guess-project-makefile ()
  (cond
   ;; Default makefile for LaTeX mode
   ((eq major-mode 'latex-mode) 
    (stufe-project-template-to-file 
     (stufe-create-project-details-from-working-folder)
     "latex-makefile" 
     "makefile"))
   ;; Default makefile for C++ mode
   ((or (eq major-mode 'c++-mode)
	(eq major-mode 'c-mode))
    (stufe-project-template-to-file 
     (stufe-create-project-details-from-working-folder)
     "cpp-makefile" 
     "makefile"))
   (t (error "No makefile associated with %s" major-mode))))


(defun stufe-open-makefile ()
  "Open the makefile found in the current folder"
  (interactive)
  (cond ((file-exists-p "Makefile.am") (find-file "Makefile.am"))
	((file-exists-p "makefile") (find-file "makefile"))
	((file-exists-p "Makefile") (find-file "Makefile"))
	(t (progn 
	     (stufe-guess-project-makefile)
	     (find-file "makefile")))))


;; *************************************************
;; * 
;; * Functions to get some information about
;; * the current compilation window
;; *
;; *************************************************

(setq stufe-compilation-buffer-name "*compilation*")

(defun stufe-get-compilation-buffer ()
  "Return the buffer used to compile"
  (get-buffer stufe-compilation-buffer-name))

(defun stufe-get-compilation-process ()
  "Return the process used to compile"
  (get-buffer-process (stufe-get-compilation-buffer)))

(defun stufe-get-compilation-window ()
  "Get the current window used to compile"
  (if (stufe-get-compilation-buffer)
      (get-buffer-window (stufe-get-compilation-buffer))))
