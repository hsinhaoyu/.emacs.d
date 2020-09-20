;; disable start-up
(setq inhibit-startup-message t)
(load-theme 'wheatgrass)

;; replaces list-buffers with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; do not use `init.el` for `custom-*` code. Use `cusetom-file.el`
(setq custom-file "~/.emacs.d/custom-file.el")
(load-file custom-file)

;; Add MELPA to package-archives
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

;; make sure that use-package is loaded
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(use-package company
  :config
  (setq company-idle-delay 0.3)
  (global-company-mode t))

;; Enhance M-x
(use-package smex
  :ensure t
  :disabled t
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (smex initialize)
  :bind ("M-x" . smex))

;; Git integration for Emacs
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; Display possible completions at all places
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

;; markdown
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

;; ========== ORG

(add-hook 'org-mode-hook 'flyspell-mode)

(define-key global-map "\C-ca" 'org-agenda)

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

(setq org-agenda-files '("~/.deft"))

;; used identation to indicate the hierarchy of headings
(setq org-startup-indented t)

;; wrap around
(setq org-startup-truncated nil)

;; use 12 hour clock
(setq org-agenda-timegrid-use-ampm 1)

;; time grid takes too much space
(setq org-agenda-use-time-grid nil)

;; Customize agenda

;; based on https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html

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

;; display timestamps in 12hour
;; This changes how timestamps are displayed, but not how agenda time grid is displayed
;; see https://emacs.stackexchange.com/questions/19863/how-to-set-my-own-date-format-for-org
;; %l is hours in 12 clock
;; %p is AM/PM
(setq-default org-display-custom-times t)
(setq org-time-stamp-custom-formats '("<%a %b %e %Y>" . "<%a %b %e %Y %l:%M%p>"))

;; ========== ORG capture
(setq org-capture-templates
      '(("a" "My TODO task format." entry
         (file "todo.org")
         "* TODO %?
SCHEDULED: %t")))

;; ========== org journal
(use-package org-journal
  :ensure t
  :custom
    (org-journal-dir "~/.deft/journal/"))

;; ========== Deft
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

;; ========== imenu - jump to definition
(global-set-key (kbd "M-i") 'imenu)

;; ========== snippets
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets")))

;; ========== interactive functions

(defun hhyu-init ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun hhyu-cheatsheet ()
  (interactive)
  (dired "~/.emacs.d/notes"))
