(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode)
         ("\\.prettierrc'" . js2-mode))
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-highlight-unused-variables-mode)
         (js2-mode . add-node-modules-path)
         (js2-mode . web-mode-init-prettier-hook))
  :config
  (setq js-indent-level 2
        js-basic-offset 2
        tab-width 2
        ;; Flycheck provides these features, so disable them: conflicting with
        ;; the eslint settings.
        js2-strict-trailing-comma-warning nil
        js2-strict-missing-semi-warning nil))

(use-package xref-js2
  :after (:or js2-mode rjsx-mode typescript-mode)
  :config
  (setq xref-js2-search-program 'rg)
  (add-hook 'js2-mode-hook (lambda ()
                             (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

(use-package eslintd-fix
  :after (:or js2-mode rxjs-mode)
  :commands eslintd-fix)

(use-package rjsx-mode
  :mode ("\\.jsx\\'" . rjsx-mode)
  :hook ((rjsx-mode . add-node-modules-path)
         (rjsx-mode . web-mode-init-prettier-hook)))

(use-package add-node-modules-path
  :commands add-node-modules-path
  :hook (rjsx-mode typescript-mode js2-mode))

;; Run Mocha or Jasmine tests
(use-package mocha
  :config (use-package mocha-snippets))

(defun web-mode-init-prettier-hook ()
  "Initialize prettier"
  (add-node-modules-path)
  (prettier-js-mode))

(use-package prettier-js
  :bind ("C-c p" . prettier-js))

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :config
  (setq-default typescript-indent-level 2))
