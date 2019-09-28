(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode)
         ("\\.prettierrc'" . js2-mode))
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-highlight-unused-variables-mode)
         (js2-mode . add-node-modules-path))
  :config
  (setq js-indent-level 2
        js-basic-offset 2
        tab-width 2
        js2-strict-warning-semi-warning nil))

(use-package rjsx-mode
  :mode ("\\.jsx\\'" . rjsx-mode)
  :hook (rjsx-mode . add-node-modules-path))

(use-package js2-refactor
  :hook (js2-mode rjsx-mode))

(use-package add-node-modules-path
  :commands add-node-modules-path
  :hook (rjsx-mode typescript-mode js2-mode))

;; Run Mocha or Jasmine tests
(use-package mocha
  :defer t
  :config (use-package mocha-snippets))

(defun web-mode-init-prettier-hook ()
  "Initialize prettier"
  (add-node-modules-path)
  (prettier-js-mode))

(use-package prettier-js
  ;; :hook ((rjsx-mode . web-mode-init-prettier-hook)
  ;;        (less-css-mode . web-mode-init-prettier-hook)
  ;;        (css-mode . web-mode-init-prettier-hook))
  :bind ("C-c p" . prettier-js)
  :config
  (setq prettier-js-args '("--trailing-comma" "es5" "--single-quote" "true" "--no-semi" "true" "--config-precedence" "prefer-file")))

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :config
  (setq-default typescript-indent-level 2))
