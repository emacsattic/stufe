;; *************************************************
;; * 
;; * Function to access easily to a cvs repository
;; *
;; *************************************************




(defun stufe-cvs-commit (description)
  "Run the 'commit' command of cvs"
  (interactive "*sLog description: ")
  (stufe-cvs-run-cvs (format "commit%s"
				 (progn
				   (stufe-cvs-update)
				   (format " -m \"%s\" " description)))))


(defun stufe-cvs-add-buffer ()
  "Add the current buffer to cvs"
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
	(stufe-cvs-run-cvs (format "add %s"
				     (file-name-nondirectory filename))
			     (file-name-directory filename))
      (error "This buffer doesn't visit any file"))))


(defun stufe-cvs-update ()
  "Run the 'update' command and update buffers in emacs"
  (interactive)
  (stufe-cvs-run-cvs "update -d")
  (update-all-buffers))


(defun stufe-cvs-run-cvs (options &optional working-folder)
  "Run a cvs command with the argument options as options"
  (save-some-buffers 1)
  (shell-command (format "%scvs %s"
			 (if working-folder 
			     (format "cd %s; " working-folder)
			   (if stufe-working-folder
			       (format "cd %s; " stufe-working-folder)
			     ""))
			 options)))



