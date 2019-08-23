(use-package org)

(use-package org-journal
  :after org
  :config
  (setq org-journal-dir "~/Google\ Drive/org/journal/"
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-date-format "%Y-%m-%d, %A"))
