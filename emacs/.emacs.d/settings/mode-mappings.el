;; YAML
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; HTML
(add-to-list 'auto-mode-alist '("\\.tmp$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tmpl$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.htm$" . web-mode))

;; CSS
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))

;; JavaScript
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.jshintrc$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.prettierrc$" . javascript-mode))

(provide 'mode-mappings)
