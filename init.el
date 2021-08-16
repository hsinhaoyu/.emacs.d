(package-initialize)

(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))

(put 'dired-find-alternate-file 'disabled nil)
