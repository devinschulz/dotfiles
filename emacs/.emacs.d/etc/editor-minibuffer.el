(use-package selectrum
  :straight (:host github :repo "raxod502/selectrum")
  :defer t
  :init

  ;; This doesn't actually load Selectrum.
  (selectrum-mode +1))

(use-package ctrlf
  :straight (:host github :repo "raxod502/ctrlf")
  :init
  (ctrlf-mode +1))

(use-package prescient
  :config
  (prescient-persist-mode +1))

(use-package selectrum-prescient
  :straight (:host github :repo "raxod502/prescient.el"
                   :files ("selectrum-prescient.el"))
  :demand t
  :after selectrum
  :config

  (selectrum-prescient-mode +1))

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode +1))

(use-package yasnippet
  :config
  (yas-global-mode +1))

(use-package yasnippet-snippets
  :after yassnippet)
