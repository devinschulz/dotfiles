(use-package json-mode)

(use-package css-mode
  :ensure nil
  :init (setq css-indent-offset 2))

;; SCSS mode
(use-package scss-mode
  :init
  ;; Disable complilation on save
  (setq scss-compile-at-save nil))

;; New `less-cs-mde' in Emacs 26
(unless (fboundp 'less-css-mode)
  (use-package less-css-mode))

;; CSS eldoc
(use-package css-eldoc
  :commands turn-on-css-eldoc
  :hook ((css-mode scss-mode less-css-mode) . turn-on-css-eldoc))

(use-package typescript-mode)

;;  "JS development.npm i -g javascript-typescript-langserver"
(use-package js2-mode
  ; :defines flycheck-javascript-eslint-executable
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter (("node" . js2-mode)
                ("node" . js2-jsx-mode))
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-highlight-unused-variables-mode)))

(use-package js2-refactor
  :hook (js2-mode . js2-refactor-mode))

(use-package add-node-modules-path
  :hook (js2-mode . add-node-modules-path))

;; Run Mocha or Jasmine tests
(use-package mocha
  :defer t
  :config (use-package mocha-snippets))

(use-package prettier-js :defer t)

;; Major mode for editing web templates
(use-package web-mode
  :defines company-backends
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

;; Live browser JavaScript, CSS, and HTML interaction
(use-package skewer-mode
  :hook ((js2-mode . skewer-mode)
         (css-mode . skewer-css-mode)
         (web-mode . skewer-html-mode)
         (html-mode . skewer-html-mode)))

(provide 'init-web-mode)
