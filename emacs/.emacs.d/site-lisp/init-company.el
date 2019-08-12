;; company
(use-package company
  :config (global-company-mode)
  (setq company-lighter-base "@")
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (defun company-complete-custom (&optional prefix)
    "Company and Yasnippet(PREFIX)."
    (interactive "P")
    (if (company--active-p) (company-cancel))
    (if prefix
        (if (not company-mode) (yas-expand)
          (call-interactively 'company-yasnippet))
      (call-interactively 'company-complete)))
  :bind ("M-]" . company-complete-custom))

(provide 'init-company)
