;; *************************************************
;; * 
;; * Function to manage keyboard shortcut 
;; *
;; *************************************************


(defun stufe-shortcut-add-local (shortcut binding)
  "Add a shortcut in the current local key map"
  (define-key (current-local-map) shortcut binding))
    

