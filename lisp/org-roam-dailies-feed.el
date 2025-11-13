;;; org-roam-dailies-feed.el --- Continuous view of org-roam dailies -*- lexical-binding: t; -*-

;; Simple “feed-style” viewer for org-roam dailies.

(require 'org)
(require 'org-roam)

(defgroup org-roam-dailies-feed nil
  "Continuous scrolling view of org-roam daily notes."
  :group 'org-roam)

(defcustom org-roam-dailies-feed-buffer-name "*org-roam-dailies-feed*"
  "Name of the buffer used to display the org-roam dailies feed."
  :type 'string
  :group 'org-roam-dailies-feed)

;; (defun org-roam-dailies-feed--dailies-dir ()
;;  "Return the org-roam dailies directory."
;;  (file-name-as-directory
;;   (if (bound-and-true-p org-roam-dailies-directory)
;;       org-roam-dailies-directory
;;    (expand-file-name "daily/" org-roam-directory))))

(defun org-roam-dailies-feed--dailies-dir ()
  (file-name-as-directory
   (expand-file-name "daily/" org-roam-directory)))


(defun org-roam-dailies-feed--daily-files ()
  "Return a list of daily files, newest first."
  (let* ((dir (org-roam-dailies-feed--dailies-dir))
         ;; Expecting filenames like 2025-11-13.org etc.
         (files (directory-files dir t "^[0-9]\\{4\\}-[01][0-9]-[0-3][0-9].*\\.org$")))
    ;; Newest first: reverse lexicographic sort
    (sort files #'string>)))

(define-derived-mode org-roam-dailies-feed-mode org-mode "Roam-Dailies-Feed"
  "Major mode for viewing a continuous feed of org-roam daily notes.

This buffer is read-only by default. If you really want to edit,
open the original daily file instead (e.g. via org-roam-dailies)."
  (setq buffer-read-only t)
  (setq-local org-startup-folded 'showeverything))

;;;###autoload
(defun org-roam-dailies-feed-show (&optional arg)
  "Show a continuous scrolling view of org-roam dailies.

With prefix ARG, reload the feed even if it already exists."
  (interactive "P")
  (let* ((buf (get-buffer-create org-roam-dailies-feed-buffer-name))
         (files (org-roam-dailies-feed--daily-files)))
    (when (or arg                  ; force reload
              (not (buffer-live-p buf))
              (y-or-n-p "Reload org-roam dailies feed? "))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (org-roam-dailies-feed-mode)
          (dolist (file files)
            ;; Insert a date header for each day
            (insert (format "* %s\n" (file-name-base file)))
            (insert (make-string 60 ?-) "\n\n")
            ;; Insert the actual content of the daily file
            (insert-file-contents file)
            ;; Ensure spacing between days
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert "\n\n")))
        (goto-char (point-min))))
    (switch-to-buffer buf)))

(provide 'org-roam-dailies-feed)
;;; org-roam-dailies-feed.el ends here
