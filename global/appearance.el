;; *************************************************
;; * 
;; * Colors for emacs
;; *
;; *************************************************


(defvar stufe.appearance.el)


(defun define-colors()
  "Define the set of colors to be used"

 ;; Default font color used
  (setq default-frame-alist
        '((foreground-color . "Black")
          (background-color . "Snow")))
 
;; Default font color used
  (set-face-foreground 'default "Black")
  (set-face-background 'default "Snow")

  ;; Mini Buffer description color
  (set-face-foreground 'modeline "Navy")
  (set-face-background 'modeline "Gray")

  ;; Fringe description background
  (if (facep 'fringe)
      (set-face-background 'fringe "Ivory"))

  ;; Functions color
  (copy-face 'bold 'font-lock-function-name-face)
  (set-face-foreground 'font-lock-function-name-face "Navy")

  ;; Type color
  (copy-face 'default 'font-lock-type-face)
  (set-face-foreground 'font-lock-type-face "Navy")

  ;; Variable color
  (copy-face 'default 'font-lock-variable-name-face)
  (set-face-foreground 'font-lock-variable-name-face "Firebrick")

  ;; Keyword color
  (copy-face 'default 'font-lock-keyword-face)
  (set-face-foreground 'font-lock-keyword-face "Blue")

  ;; String color
  (copy-face 'default 'font-lock-string-face)
  (set-face-foreground 'font-lock-string-face "SteelBlue")

  ;; Comment color
  (copy-face 'default 'font-lock-comment-face)
  (set-face-foreground 'font-lock-comment-face "ForestGreen")

  ;; Constant color
  (copy-face 'default 'font-lock-constant-face)
  (set-face-foreground 'font-lock-constant-face "MediumSlateBlue")

  ;; Builtin color
  (copy-face 'default 'font-lock-builtin-face)
  (set-face-foreground 'font-lock-builtin-face "Navy")

  ;; Warning color
  (copy-face 'bold 'font-lock-warning-face)
  (set-face-foreground 'font-lock-warning-face "BlueViolet")

  ;; Selection color
  (copy-face 'default 'region)
  (set-face-foreground 'region "white")
  (set-face-background 'region "light steel blue"))

;; Appel à la fonction précédente pour définir les couleurs
(defun stufe-activate-color ()
  "Active the stufe theme color for Emacs"
  (define-colors)
  (setq global-font-lock-mode t)
  (global-font-lock-mode)
  (global-font-lock-mode))


(stufe-activate-color)

