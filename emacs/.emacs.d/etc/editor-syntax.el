;; Disable tabs for indentation
(setq-default index-tabs-mode nil)

;; Set the tab width to 2 spaces
(setq-default tab-width 2)

;; Highlight current line
;; (global-hl-line-mode 1)
;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

(use-package ws-butler
  :defer t
  :hook (prog-mode . ws-butler-mode))

(use-package origami
  :bind (("C-c C-o t" . origami-toggle-node)
         ("C-c C-o r" . origami-toggle-all-nodes)
         ("C-c C-o c" . origami-close-node)
         ("C-c C-o o" . origami-open-node)
         ("C-c C-o s" . origami-close-all-nodes)
         ("C-c C-o w" . origami-open-all-nodes)
         ("C-c C-o n" . origami-next-fold)
         ("C-c C-o p" . origami-previous-fold)
         ("C-c C-o f" . origami-forward-fold))
  :config (origami-mode t))

(use-package crux
  :bind
  ("C-^" . crux-top-join-line)
  ("C-a" . crux-move-beginning-of-line)
  ("C-o" . crux-smart-open-line-above)
  ("M-o" . crux-smart-open-line)
  ("C-<BACKSPACE>" . crux-kill-line-backwards)
  ("C-<DEL>" . crux-kill-line-forwards)
  ("C-c c" . crux-create-scratch-buffer)
  ("C-c d" . crux-duplicate-current-line-or-region)
  ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
  ("C-c D" . crux-delete-file-and-buffer)
  ("C-c r" . crux-rename-buffer-and-file)
  ("C-c t" . crux-visit-term-buffer)
  ("C-h RET" . crux-find-user-init-file)
  ("C-x x e" . crux-open-with)
  ("C-x 7" . crux-swap-windows))

(use-package hydra)

(defhydra dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back"))

(use-package editorconfig
  :defer t
  :config
  (editorconfig-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)))

(use-package embrace
  :bind
  ("C-c e d" . embrace-delete)
  ("C-c e r" . embrace-replace)
  ("C-c e a" . embrace-add)
  ("C-c e c" . embrace-change))
