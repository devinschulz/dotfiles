(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)

;; Auto-pair
(electric-pair-mode 1)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

;; Replace the globally defined zap-to-char with zap-up-to-char
(global-set-key "\M-z" 'zap-up-to-char)

;; Don't blink the cursor on the opening paren when you insert a
;; closing paren, as we already have superior handling of that from
;; Smartparens.
(setq blink-matching-paren nil)

(defun display-startup-echo-area-message ()
  (message ""))

(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(setq delete-by-moving-to-trash t)
(setq track-eol t) ; Keep cursor at end of lines
(setq line-move-visual nil) ; To be required by track-eol

(setq-default kill-whole-line t) ; Kill line including '\n'
(setq-default indent-tabs-mode nil) ; use space

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package move-text
  :bind (("M-g <up>" . move-text-up)
         ("M-g <down>" . move-text-down)))

(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

;; Highlights changes to the buffer caused by commands
;; such as ‘undo’, ‘yank’/’yank-pop’, etc.
(use-package volatile-highlights
  :demand t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t)
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree))

;; Automatically reload files that was modified by an external program
(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

;; Delete selection when inserting someting
(use-package delsel
  :demand t
  :config
  (delete-selection-mode +1))

(use-package hungry-delete
  :hook (after-init . global-hungry-delete-mode))
