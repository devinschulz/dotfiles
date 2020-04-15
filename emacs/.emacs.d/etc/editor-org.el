(use-package org
  :init
  (setq org-directory "~/Google Drive/My Notes")
  (setq org-hide-emphasis-markers nil)
  (setq org-pretty-entities nil))

(use-package org-journal
  :after org
  :config
  (setq org-journal-dir (concat org-directory "/journal")
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-date-format "%Y-%m-%d, %A"))

(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  ("C-c n l" . dev/get-linked-files)
  ("C-c n i" . dev/insert-zettel)
  :config
  (setq zettel-indicator "§")
  (setq deft-extensions '("org" "md" "txt"))
  (setq deft-default-extension "txt")
  (setq deft-directory (concat org-directory "/braindump"))
  (setq deft-recursive t)

  (defun dev/insert-zettel (file-name)
    "Finds a file, inserts it as a link with the base file name as the link name, and adds the zd-link-indicator I use to the front."
    (interactive (list (completing-read "File: " (deft-find-all-files-no-prefix))))
    (org-insert-link nil (concat "file:" (file-name-base file-name) "." (file-name-extension file-name)) (concat zettel-indicator (file-name-base file-name))))

  (defun dev/get-linked-files ()
    "Show links to this file."
    (interactive)
    (let* ((search-term (file-name-nondirectory buffer-file-name))
           (files deft-all-files)
	         (tnames (mapcar #'file-truename files)))
      (multi-occur
       (mapcar (lambda (x)
	               (with-current-buffer
		                 (or (get-file-buffer x) (find-file-noselect x))
		               (widen)
		               (current-buffer)))
	             files)
       search-term
       3))))

(use-package zetteldeft
  :after deft
  :config
  (setq zetteldeft-id-format "%Y%m%d%H%M")
  (setq zetteldeft-id-regex "[0-9]\\{12\\}"))
