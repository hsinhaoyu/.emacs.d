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

;; ORG

(add-hook 'org-mode-hook 'flyspell-mode)

(define-key global-map "\C-ca" 'org-agenda)

;; (setq org-todo-keywords
;;       '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE" "CANCELED")))

(setq org-agenda-files '("~/.deft"))

(setq org-startup-indented t)

(setq org-startup-truncated nil)

(setq org-agenda-timegrid-use-ampm 1)

;; incomplete
;; based on this article https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
;; org-agenda-prefix-format: https://emacs.stackexchange.com/questions/15309/is-there-a-way-to-show-an-agenda-with-just-a-time-grid
(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
	 ((tags "PRIORITY=\"A\""
		((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
		 (org-agenda-overriding-header "High-priority unfinished tasks:")))
	  (agenda ""
		  ((org-agenda-use-time-grid nil)))
	  (alltodo "")))))

(setq org-agenda-include-diary t)

;; This changes how timestamps are displayed, but not how agenda time grid is displayed
;; see https://emacs.stackexchange.com/questions/19863/how-to-set-my-own-date-format-for-org
;; (setq-default org-display-custom-times t)
;; (setq org-time-stamp-custom-formats '("<%a %b %e %Y>" . "<%a %b %e %Y %H:%M>"))

;; ORG capture
(setq org-capture-templates
      '(("a" "My TODO task format." entry
         (file "todo.org")
         "* TODO %?
SCHEDULED: %t")))

;; Deft
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

;; imenu - jump to definition
(global-set-key (kbd "M-i") 'imenu)

;; snippets
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets")))

;; customize menu bar

;; create a new menu item after Buffers
(define-key-after
  global-map
  [menu-bar bookmarks]
  (cons "Bookmarks" (make-sparse-keymap "Bookmarks"))
  'buffers)


(define-key
  global-map
  [menu-bar bookmarks bookmarksList]
  '("List bookmarks" . bookmark-bmenu-list))

(define-key
  global-map
  [menu-bar bookmarks bookmarksSet]
  '("Set a bookmark" . bookmark-set))

