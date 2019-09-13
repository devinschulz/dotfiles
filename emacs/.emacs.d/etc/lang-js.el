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
        js-basic-offset 2
        tab-width 2
        js2-strict-warning-semi-warning nil))

(use-package js2-refactor
  :hook (js2-mode . js2-refactor-mode))

(use-package add-node-modules-path
  :commands add-node-modules-path
  :hook (js2-mode typescript-mode))

;; Run Mocha or Jasmine tests
(use-package mocha
  :defer t
  :config (use-package mocha-snippets))

(defun web-mode-init-prettier-hook ()
  "Initialize prettier"
  (add-node-modules-path)
  (prettier-js-mode))

(use-package prettier-js
  :hook ((js2-mode . web-mode-init-prettier-hook)
         (less-css-mode . web-mode-init-prettier-hook)
         (css-mode . web-mode-init-prettier-hook))
  :bind ("C-c p" . prettier-js)
  :config
  (setq prettier-js-args '("--trailing-comma" "none" "--single-quote" "true" "--no-semi" "true" "--config-precedence" "prefer-file")))

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode)))
