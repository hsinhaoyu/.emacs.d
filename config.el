(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))

(when (not (package-installed-p 'use-package))
(package-refresh-contents)
(package-install 'use-package))

(setq custom-file "~/.emacs.d/custom-file.el")
(load-file custom-file)

(setq inhibit-startup-message t)
(setq column-number-mode t)

(use-package monokai-theme
    :ensure t)
(load-theme 'monokai t)

(use-package smex
    :ensure t
    :disabled t
    :config
        (setq smex-save-file (concat user-emacs-directory ".smex-items"))
	(smex initialize)
    :bind 
        ("M-x" . smex))

(use-package company
    :config
        (setq company-idle-delay 0.3)
        (global-company-mode t))

(use-package ido-completing-read+
    :ensure t
    :config
        ;; This enables ido in all contexts where it could be useful, not just
        ;; for selecting buffer and file names
        (ido-mode t)
        (ido-everywhere t)
        ;; This allows partial matches, e.g. "uzh" will match "Ustad Zakir Hussain"
	(setq ido-enable-flex-matching t)
	(setq ido-use-filename-at-point nil)
	;; Includes buffer names of recently opened files, even if they're not open now.
	(setq ido-use-virtual-buffers t)
	:diminish nil)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package markdown-mode
    :ensure t
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown")
    :config (setq markdown-open-command "/usr/local/bin/mark")
    :hook ((markdown-mode . flyspell-mode)
           (gfm-mode . flyspell-mode)))

(use-package deft
    :ensure t
    :bind (("<f8>" . deft))
    :commands (deft)
    :config
    (setq deft-extensions '("txt" "tex" "md" "org")
          deft-directory "~/.deft"
	    deft-default-extension "org"
	    deft-recursive t
	    deft-use-filter-string-for-filename nil
	    deft-use-filename-as-title nil
	    deft-markdown-mode-title-level 1
	    deft-file-naming-rules '((noslash . "-")
                                   (nospace . "-")
				     (case-fn . downcase))))

;; spellcheck all org documents
(add-hook 'org-mode-hook 'flyspell-mode)

;; evoke agenda with one key
(define-key global-map "\C-ca" 'org-agenda)

;; used identation to indicate the hierarchy of headings
(setq org-startup-indented t)

;; wrap around
(setq org-startup-truncated nil)

(use-package org-bullets
    :ensure t
    :init
        (setq org-bullets-bullet-list'("◉" "●" "○" "▪" "▪"))
    :config
        (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
        (setq org-ellipsis " ▾"))

;; %l is hours in 12 clock
;; %p is AM/PM
(setq-default org-display-custom-times t)
(setq org-time-stamp-custom-formats '("<%a %b %e %Y>" . "<%a %b %e %Y %l:%M%p>"))

(setq org-agenda-files '("~/.deft"))

(setq org-todo-keywords
    '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

;; use 12 hour clock in timegrid
(setq org-agenda-timegrid-use-ampm 1)

;; time grid takes too much space
(setq org-agenda-use-time-grid nil)

(defun air-org-skip-subtree-if-priority (priority)
    "Skip an agenda subtree if it has a priority of PRIORITY.
     PRIORITY may be one of the characters ?A, ?B, or ?C."
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (pri-value (* 1000 (- org-lowest-priority priority)))
          (pri-current (org-get-priority (thing-at-point 'line t))))
         (if (= pri-value pri-current)
             subtree-end
             nil)))

(defun air-org-skip-subtree-if-habit ()
    "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
         (if (string= (org-entry-get nil "STYLE") "habit")
             subtree-end
         nil)))

(setq org-agenda-custom-commands
    '(("d" "Daily agenda and all TODOs"
       ((tags "PRIORITY=\"A\""
              ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
               (org-agenda-overriding-header "High-priority unfinished tasks:")))

        (agenda "")

	(alltodo ""
                 ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                 (air-org-skip-subtree-if-priority ?A)
                                                 (org-agenda-skip-if nil '(scheduled deadline))))
                  (org-agenda-overriding-header "ALL normal priority tasks:"))))
       ((org-agenda-compact-blocks nil)))))

(use-package org-journal
      :ensure t
      :custom
          (org-journal-dir "~/.deft/journal/")
          (org-journal-file-format "%Y-%m-%d.org"))

(defun get-journal-file-today ()
    "Return filename for today's journal entry."
    (let ((daily-name (format-time-string "%Y-%m-%d.org")))
           (expand-file-name (concat org-journal-dir daily-name))))

(defun journal-file-today ()
    "Create and load a journal file based on today's date."
    (interactive)
    (find-file (get-journal-file-today)))

(defun org-journal-find-location ()
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    (org-journal-new-entry t)
    (org-narrow-to-subtree))

(setq org-capture-templates
    '(("a" "My TODO task format"
       entry
       (file "todo.org")
       "* TODO %?
          SCHEDULED: %t")
      ("j" "Journal entry"
       plain
       (function org-journal-find-location)
       "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
       :jump-to-captured t
       :immediate-finish t)))

(use-package magit
    :ensure t
    :bind ("C-x g" . magit-status))

(global-set-key (kbd "M-i") 'imenu)

(use-package yasnippet
    :ensure t
    :init (yas-global-mode 1)
    :config
        (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets")))

(defun hhyu-init ()
    (interactive)
    (find-file "~/.emacs.d/config.org"))

(defun hhyu-cheatsheet ()
    (interactive)
    (dired "~/.emacs.d/notes"))
