;;; org-roam-dailies-feed.el --- Continuous lazy view of org-roam dailies -*- lexical-binding: t; -*-

(require 'org)
(require 'org-roam)

(defgroup org-roam-dailies-feed nil
  "Continuous scrolling view of org-roam daily notes."
  :group 'org-roam)

(defcustom org-roam-dailies-feed-buffer-name "*org-roam-dailies-feed*"
  "Name of the buffer used to display the org-roam dailies feed."
  :type 'string
  :group 'org-roam-dailies-feed)

(defcustom org-roam-dailies-feed-batch-size 7
  "How many daily files to load at a time in the feed."
  :type 'integer
  :group 'org-roam-dailies-feed)

(defcustom org-roam-dailies-feed-near-end-threshold 3000
  "If point is within this many characters of `point-max',
older daily files are lazily loaded."
  :type 'integer
  :group 'org-roam-dailies-feed)

(defvar-local org-roam-dailies-feed--files nil
  "List of all daily files for the current feed buffer, newest first.")

(defvar-local org-roam-dailies-feed--loaded-count 0
  "How many daily files have been inserted into the current feed buffer.")

;;(defun org-roam-dailies-feed--dailies-dir ()
;;  "Return the org-roam dailies directory."
;;  (file-name-as-directory
;;   (if (bound-and-true-p org-roam-dailies-directory)
;;       org-roam-dailies-directory
;;     (expand-file-name "daily/" org-roam-directory))))

(defun org-roam-dailies-feed--dailies-dir ()
  "Return the org-roam dailies directory."
  (file-name-as-directory
     (expand-file-name "daily/" org-roam-directory)))

(defun org-roam-dailies-feed--daily-files ()
  "Return a list of daily files, newest first."
  (let* ((dir (org-roam-dailies-feed--dailies-dir))
         ;; Filenames like 2025-11-13.org etc.
         (files (directory-files dir t "^[0-9]\\{4\\}-[01][0-9]-[0-3][0-9].*\\.org$")))
    (sort files #'string>)))  ;; reverse lexicographic: newest first

(defun org-roam-dailies-feed--insert-day (file)
  "Insert one daily FILE into the current buffer, with a header."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert (format "* %s\n" (file-name-base file)))
    (insert (make-string 60 ?-) "\n\n")
    (insert-file-contents file)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "\n\n")))

(defun org-roam-dailies-feed--insert-next-batch ()
  "Insert the next batch of daily files into the current buffer."
  (let* ((files org-roam-dailies-feed--files)
         (start org-roam-dailies-feed--loaded-count)
         (end (min (length files)
                   (+ start org-roam-dailies-feed-batch-size))))
    (when (< start end)
      (let ((inhibit-read-only t))
        (while (< start end)
          (org-roam-dailies-feed--insert-day (nth start files))
          (setq start (1+ start)))
        (setq org-roam-dailies-feed--loaded-count end)))))

(defun org-roam-dailies-feed--maybe-load-more ()
  "If point is near `point-max', lazily load more daily files."
  (when (and (derived-mode-p 'org-roam-dailies-feed-mode)
             org-roam-dailies-feed--files)
    (let ((remaining (- (point-max) (point))))
      (when (and (< remaining org-roam-dailies-feed-near-end-threshold)
                 (< org-roam-dailies-feed--loaded-count
                    (length org-roam-dailies-feed--files)))
        (save-excursion
          (goto-char (point-max))
          (org-roam-dailies-feed--insert-next-batch))))))

(define-derived-mode org-roam-dailies-feed-mode org-mode "Roam-Dailies-Feed"
  "Major mode for viewing a continuous feed of org-roam daily notes.

This buffer is read-only by default. If you want to edit, open the
original daily file instead (e.g. via org-roam-dailies).

Scrolling near the bottom lazily loads older daily files."
  (setq buffer-read-only t)
  (setq-local org-startup-folded 'showeverything)
  ;; Make the lazy loader buffer-local.
  (add-hook 'post-command-hook #'org-roam-dailies-feed--maybe-load-more nil t))

;;;###autoload
(defun org-roam-dailies-feed-show (&optional arg)
  "Show a continuous scrolling view of org-roam dailies.

With prefix ARG, reload the feed even if it already exists."
  (interactive "P")
  (let ((buf (get-buffer-create org-roam-dailies-feed-buffer-name)))
    (with-current-buffer buf
      (when (or arg
                (not (derived-mode-p 'org-roam-dailies-feed-mode))
                (= (buffer-size) 0)
                (y-or-n-p "Reload org-roam dailies feed? "))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (org-roam-dailies-feed-mode)
          (setq org-roam-dailies-feed--files (org-roam-dailies-feed--daily-files))
          (setq org-roam-dailies-feed--loaded-count 0)
          (if org-roam-dailies-feed--files
              (progn
                (org-roam-dailies-feed--insert-next-batch)
                (goto-char (point-min)))
            (insert "No org-roam dailies found.\n")))))
    (switch-to-buffer buf)))

;;;###autoload
(defun org-roam-dailies-feed-load-older ()
  "Manually load older daily files into the current feed buffer."
  (interactive)
  (if (not (derived-mode-p 'org-roam-dailies-feed-mode))
      (user-error "Not in an org-roam dailies feed buffer")
    (if (>= org-roam-dailies-feed--loaded-count
            (length org-roam-dailies-feed--files))
        (message "No more daily files to load.")
      (org-roam-dailies-feed--insert-next-batch)
      (message "Loaded more org-roam dailies..."))))

(provide 'org-roam-dailies-feed)
;;; org-roam-dailies-feed.el ends here
