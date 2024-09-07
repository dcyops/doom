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
;; needs to be set in ~/.zshrc

;;
;;;; gcc

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
;;;; lsp settings
;;;;
(after! lsp-clangd
  (set-lsp-priority! 'clangd 2)) ;; Use clangd as the default LSP server for C/C++
;;
;;;; dasnippet
(after! yasnippet
  (add-to-list 'yas-snippet-dirs "~/.config/doom/snippets")
  (yas-reload-all))

;;(yas-global-mode 1)
;;(setq yas-snippets-dirs '("$HOME/.config/doom/snippets"))


(map! :leader
      :desc "Insert comment block"
      "m c"
      (lambda ()
        (interactive)
        (yas-expand-snippet (yas-lookup-snippet "comment-block" 'c-mode))))


(use-package! pyvenv
  :config
  ;; Automatically activates the virtual environment located at ~/.venv/dev
  (setenv "WORKON_HOME" "~/.venv/")
  (pyvenv-activate "~/.venv/dev"))

;;
;;;; Documentation


;; Ansible
(add-hook 'yaml-mode-hook #'ansible-doc-mode)
(defun ansible-doc-at-point ()
  "Run `ansible-doc` on the ansible module at point."
  (interactive)
  (let ((module (thing-at-point 'symbol)))
    (if module
        (ansible-doc module)
      (message "No Ansible module found at point"))))

(after! yaml-mode
  ;; Unbind the default `K` lookup in yaml-mode
  (map! :map yaml-mode-map
        :n "K" nil) ; Unbind `K`
  ;; Bind `K` to run ansible-doc on the module under the cursor
  (map! :map yaml-mode-map
        :n "K" #'ansible-doc-at-point))

;; C/C++
(add-hook 'c-mode-hook
          (lambda () (setq-local devdocs-current-docs '("c"))))
