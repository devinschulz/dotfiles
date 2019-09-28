(use-package org)

(use-package org-journal
  :after org
  :config
  (setq org-journal-dir "~/notes/journal/"
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-date-format "%Y-%m-%d, %A"))

(use-package org-bullets
  :hook (org-mode . (lambda () (org-bullets-mode 1))))
