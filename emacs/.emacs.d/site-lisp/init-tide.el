(use-package tide
  :diminish tide-mode
  :defines (company-backends tide-format-options)
  :functions (tide-setup tide-hl-identifier-mode)
  :preface
  (defun setup-tide-mode ()
    "Setup tide mode."
    (interactive)
    (tide-setup)
    (eldoc-mode 1)
    (tide-hl-identifier-mode 1))
  :hook (((typescript-mode js2-mode) . setup-tide-mode)
         (before-save . tide-format-before-save))
  :config
  (setq tide-format-options
        '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions
          t
          :placeOpenBraceOnNewLineForFunctions
          nil))

  (with-eval-after-load 'company
    (cl-pushnew 'company-tide company-backends)))

(provide 'init-tide)
