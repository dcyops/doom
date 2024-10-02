;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;
;;;; Appearence

(setq doom-theme 'doom-snazzy
      doom-font (font-spec :family "iosevka fixed ss18" :weight 'light :size 22)
      doom-variable-pitch-font (font-spec :family "iosevka fixed ss18" :weight 'semibold :size 22))

(setq nerd-icons-font-names '("SymbolsNerdFontMono-Regular.ttf"))

;; Set transparancy
(add-to-list 'default-frame-alist '(alpha . 96))

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
;;;; UI

;; Project specific tabs
(defun my/projectile-filter-tabs ()
  (interactive)
  (let ((project-root (projectile-project-root)))
    (dolist (tab (tab-bar-tabs))
      (let ((file (alist-get 'name tab)))
        (unless (and file (string-prefix-p project-root file))
          (tab-bar-close-tab-by-name file))))))
(add-hook 'projectile-after-switch-project-hook 'my/projectile-filter-tabs)

;; Cycle through tabs
;;(global-set-key (kbd "M-]") 'centaur-tabs-forward)
;;(global-set-key (kbd "M-[") 'centaur-tabs-backward)

;;
;;;; Performance

;; Prevents some cases of Emacs flickering.
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; Make vterm more snappy
(setq vterm-timer-delay nil)

;; (setq lsp-ui-sideline-enable t)
;; (setq lsp-ui-sideline-show-hover t)
;; (setq lsp-ui-sideline-show-diagnostics t)

;;
;;;; Mappings

(map! :n "C-,"    #'switch-to-buffer)
(map! :n "C-."    #'find-file)
(map! "C-c x"     #'execute-extended-command)
(map! "C-c t"     #'term)
(map! "C-c b"     #'list-bookmarks)
(define-key key-translation-map (kbd "Caps_Lock") (kbd "M"))
;; lsp
(map! :leader :desc "Format buffer" "c ="   #'lsp-format-buffer)
(map! :leader :desc "Format region" "c r"   #'lsp-format-region)
(map! :leader :desc "Format region" "m r =" #'+format/region)

;; documentation
(global-set-key (kbd "C-c d") 'devdocs-lookup)

;; search & replace
(global-unset-key (kbd "C-d"))
(map! :after evil :gni "C-d" #'replace-string)

;; multiple-cursors
(map! "C-k"     #'evil-mc-make-cursor-move-prev-line)
(map! "C-j"     #'evil-mc-make-cursor-move-next-line)
(map! ","       #'evil-mc-undo-all-cursors)

;;
;;;; Clipboard

(setq select-active-regions nil)
(setq select-enable-clipboard 't)
(setq select-enable-primary nil)
(setq interprogram-cut-function #'gui-select-text)

;;
;;; ENV
(setenv "PATH" (concat (getenv "PATH") ":/home/$USER/.local/bin/npm"))

(load-file (expand-file-name "custom.el" doom-user-dir))
