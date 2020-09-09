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
  :init (setq markdown-command "multimarkdown"))

(setq markdown-open-command "/usr/local/bin/mark")
(add-hook 'markdown-mode-hook 'flyspell-mode)


;; python mode
;;(use-package elpy
;;  :ensure t
;;  :config
;;  (setq elpy-rpc-virtualenv-path 'current)
;;  :init
;;  (elpy-enable))

;; Taskpaper
;; (use-package taskpaper-mode
;;   :ensure t
;;   :bind ("M-z" . taskpaper-cycle))

