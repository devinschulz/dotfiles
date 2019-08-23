;;  "JS development.npm i -g javascript-typescript-langserver"
(use-package js2-mode
  ; :defines flycheck-javascript-eslint-executable
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode)
         ("\\.prettierrc'" . js2-mode))
  :interpreter (("node" . js2-mode)
                ("node" . js2-jsx-mode))
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-highlight-unused-variables-mode)
         (js2-mode . add-node-modules-path))
  :config
  (setq js-indent-level 2
        tab-width 2))

(use-package js2-refactor
  :hook (js2-mode . js2-refactor-mode))

(use-package add-node-modules-path
  :hook (js2-mode . add-node-modules-path))

;; Run Mocha or Jasmine tests
(use-package mocha
  :defer t
  :config (use-package mocha-snippets))

(defun web-mode-init-prettier-hook ()
  "Initialize prettier"
  (add-node-modules-path)
  (prettier-js-mode))

(use-package prettier-js
  :hook (js2-mode . web-mode-init-prettier-hook))

(use-package tern
  :config
  (bind-key "C-c C-c" 'compile tern-mode-keymap)
  :hook (js2-mode . tern-mode))

(use-package company-tern
  :init (add-to-list 'company-backends 'company-tern))

;; Typescript
(defun my/setup-tide-mode ()
  "Set up tide mode."
  (interactive)
  (setq company-tooltip-align-annotations t)
  (tide-setup)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (setq typescript-indent-level 2)
  (setq js2-indent-level 2)
  (flycheck-mode +1)
  (company-mode +1))

(use-package typescript-mode)

(use-package tide
  :hook ((before-save . tide-format-before-save)
         (typescript-mode . my/setup-tide-mode)
         (js2-mode . my/setup-tide-mode)))
