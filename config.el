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
  "francais")