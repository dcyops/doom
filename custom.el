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

;; Compiler options

(defun my/compile-gcc ()
  (interactive)
  (let ((filename (file-name-sans-extension (buffer-file-name))))
    (compile (format "gcc -o %s %s.c" filename filename))))

(defun my/compile-gcc-debug ()
  (interactive)
  (let ((filename (file-name-sans-extension (buffer-file-name))))
    (compile (format "gcc -g -o %s %s.c" filename filename))))

(defun my/compile-gcc-asm ()
  (interactive)
  (let ((filename (file-name-sans-extension (buffer-file-name))))
    (compile (format "gcc -O2 -o %s %s.c" filename filename))))

(defun my/compile-gcc-32bit-intel ()
  (interactive)
  (let ((filename (file-name-sans-extension (buffer-file-name))))
    (compile (format "gcc -m32 -masm=intel -o %s %s.c" filename filename))))

(defun my/run-program ()
  (interactive)
  (let ((filename (file-name-sans-extension (buffer-file-name))))
    (async-shell-command (format "./%s" filename))))

(defhydra hydra-compile (:color blue :hint nil)
"
^Compile Options^
------------------
_g_: GCC
_d_: GCC (Debug)
_o_: GCC (Optimized)
_i_: GCC (32-bit Intel)
_r_: Run Program
_q_: Quit
"
  ("g" my/compile-gcc "GCC")
  ("d" my/compile-gcc-debug "GCC (Debug)")
  ("o" my/compile-gcc-optim "GCC (Optimized)")
  ("i" my/compile-gcc-32bit-intel "GCC (32-bit Intel)")
  ("r" my/run-program "Run Program")
  ("q" nil "Quit" :color blue))

(map! :leader
      :desc "Compile Menu" "c m" #'hydra-compile/body)
;;
;;;; Dasnippet
(after! yasnippet
  (add-to-list 'yas-snippet-dirs "~/.config/doom/snippets")
  (yas-reload-all))

(map! :leader
      :desc "Insert comment block"
      "m c"
      (lambda ()
        (interactive)
        (yas-expand-snippet (yas-lookup-snippet "comment-block" 'c-mode))))

;; Formatting
(map! :leader
      :desc "Format buffer" "c =" #'lsp-format-buffer)

(map! :leader
      :desc "Format region" "c r" #'lsp-format-region)

<<<<<<< HEAD
(map! :leader
      :desc "Format region" "m r =" #'+format/region)

;; Dasnippet
(yas-global-mode 1)
(setq yas-snippets-dirs '("$HOME/.config/doom/snippets"))
=======
;; Dasnippet
(yas-global-mode 1)
(setq yas-snippets-dirs '("$HOME/.config/doom/snippets"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(warning-suppress-log-types '((modus-themes) (modus-themes))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
>>>>>>> origin/main
