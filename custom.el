;;
;;;; markdown-preview

;; Appearence
(custom-set-faces!
'(markdown-header-delimiter-face :foreground "#616161" :height 0.9)
'(markdown-header-face-1 :height 1.8 :foreground "#A3BE8C" :weight extra-bold :inherit markdown-header-face)
'(markdown-header-face-2 :height 1.4 :foreground "#EBCB8B" :weight extra-bold :inherit markdown-header-face)
'(markdown-header-face-3 :height 1.2 :foreground "#D08770" :weight extra-bold :inherit markdown-header-face)
'(markdown-header-face-4 :height 1.15 :foreground "#BF616A" :weight bold :inherit markdown-header-face)
'(markdown-header-face-5 :height 1.1 :foreground "#b48ead" :weight bold :inherit markdown-header-face)
'(markdown-header-face-6 :height 1.05 :foreground "#5e81ac" :weight semi-bold :inherit markdown-header-face))

;; Hide markdown syntax when editing

 (defvar nb/current-line '(0 . 0)
   "(start . end) of current line in current buffer")
 (make-variable-buffer-local 'nb/current-line)

 (defun nb/unhide-current-line (limit)
   "Font-lock function"
   (let ((start (max (point) (car nb/current-line)))
         (end (min limit (cdr nb/current-line))))
     (when (< start end)
       (remove-text-properties start end
                       '(invisible t display "" composition ""))
       (goto-char limit)
       t)))

 (defun nb/refontify-on-linemove ()
   "Post-command-hook"
   (let* ((start (line-beginning-position))
          (end (line-beginning-position 2))
          (needs-update (not (equal start (car nb/current-line)))))
     (setq nb/current-line (cons start end))
     (when needs-update
       (font-lock-fontify-block 3))))

 (defun nb/markdown-unhighlight ()
   "Enable markdown concealling"
   (interactive)
   (markdown-toggle-markup-hiding 'toggle)
   (font-lock-add-keywords nil '((nb/unhide-current-line)) t)
   (add-hook 'post-command-hook #'nb/refontify-on-linemove nil t))

;; Window behavior

(setq markdown-xwidget-command nil
      markdown-xwidget-github-theme "light"
      markdown-xwidget-mermaid-theme "default"
      markdown-xwidget-code-block-theme "default")

;; Fix for xwidget blank white screen
;; https://www.reddit.com/r/emacs/comments/141jefa/emacs_with_xwidgets_on_wsl/
;;(setenv "WEBKIT_DISABLE_COMPOSITING_MODE" "1")
