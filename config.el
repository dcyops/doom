;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;
;;;; Display
(defun set-frame-size-according-to-resolution ()
  "Adjust frame size based on screen resolution."
  (interactive)
  (when window-system
    (let* ((frame-width (if (> (x-display-pixel-width) 1280) 200 200))
           (frame-height (/ (- (x-display-pixel-height) 220) (frame-char-height))))
      (add-to-list 'default-frame-alist (cons 'width frame-width))
      (add-to-list 'default-frame-alist (cons 'height frame-height)))))

(defun ct/frame-center (&optional frame)
  "Center a frame on the screen."
  (interactive)
  (let* ((frame (or frame (selected-frame)))
         (frame-width (frame-pixel-width frame))
         (frame-height (frame-pixel-height frame))
         (display-width (x-display-pixel-width))
         (display-height (x-display-pixel-height))
         (center-x (/ (- display-width frame-width) 2))
         (center-y (/ (- display-height frame-height) 2)))
    (set-frame-position frame center-x center-y)))

;; Hooks to run functions after initialization
(add-hook 'after-init-hook 'set-frame-size-according-to-resolution)
(add-hook 'after-init-hook 'ct/frame-center)

;;
;;;; Appearence
(setq doom-theme 'doom-badger
      doom-font (font-spec :family "Iosevka" :weight 'light :size 24)
<<<<<<< HEAD
      doom-variable-pitch-font (font-spec :family "Iosevka" :weight 'regular :size 22))
=======
      doom-variable-pitch-font (font-spec :family "Iosevka" :weight 'regular :size 24))
>>>>>>> 0f4ac8e ()

(setq nerd-icons-font-names '("SymbolsNerdFontMono-Regular.ttf"))

;; Line numbers are pretty slow all around. The performance boost of disabling
;; them outweighs the utility of always keeping them on.
(setq display-line-numbers-type nil)

;; Prevents some cases of Emacs flickering.
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(set-frame-parameter nil 'alpha-background 97)
(add-to-list 'default-frame-alist '(alpha-background . 97))

;; Make vterm more snappy
(setq vterm-timer-delay nil)

;; Ensure windows split vertically by default
;;(setq split-height-threshold nil)
;;(setq split-width-threshold 160)

(setq lsp-ui-sideline-enable nil)    ;; Disable sideline annotations

;;;; Binds
(map! :n "C-,"    #'switch-to-buffer)
(map! :n "C-."    #'find-file)
(map! "C-c x"     #'execute-extended-command)
(map! "C-c t"     #'vterm)

;; Clipboard
(setq select-active-regions nil)
(setq select-enable-clipboard 't)
(setq select-enable-primary nil)
(setq interprogram-cut-function #'gui-select-text)

(load-file (expand-file-name "custom.el" doom-user-dir))
