;;; sn-tasks.el --- Agenda + pomodoro bridge for Quickshell -*- lexical-binding: t; -*-

;; Exposes a single JSON-emitting entry point `sn-tasks/snapshot' consumed by
;; the Caelestia Quickshell Tasks service. All Emacs-side task, clock, and
;; type-break state ends up in one round-trip.

(require 'json)
(require 'org)
(require 'org-clock)
(require 'org-agenda)
(require 'type-break)
(require 'cl-lib)

(defgroup sn-tasks nil "Quickshell agenda/pomodoro bridge." :group 'org)

(defcustom sn-tasks-agenda-files '("~/doc/inbox.org" "~/doc/projects.org")
  "Files scanned for the Quickshell agenda view."
  :type '(repeat file))

(defcustom sn-tasks-filter-file "~/.cache/sn-tasks-filter"
  "Persists the current tag filter (work/personal) across Emacs sessions."
  :type 'file)

(defcustom sn-tasks-max 15
  "Maximum number of tasks to return in the snapshot."
  :type 'integer)

(defcustom sn-tasks-todo-keywords
  '((sequence "TODO(t)" "NEXT(n/!)" "INPROGRESS(i/!)" "|" "DONE(d!/!)")
    (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
    (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)"))
  "Todo keyword sequences used when producing the snapshot."
  :type 'sexp)


;;; Filter persistence

(defun sn-tasks/get-filter ()
  "Return the stored filter tag (work or personal)."
  (if (file-exists-p sn-tasks-filter-file)
    (with-temp-buffer
      (insert-file-contents sn-tasks-filter-file)
      (string-trim (buffer-string)))
    "work"))

(defun sn-tasks/set-filter (tag)
  "Persist TAG (a string) as the active filter."
  (with-temp-file sn-tasks-filter-file (insert tag))
  tag)


;;; Type-break / pomodoro snapshot

(defun sn-tasks--pomodoro ()
  "Return an alist describing the current type-break / active-task state."
  (let* ((time-difference
           (when type-break-mode
             (type-break-time-difference nil type-break-time-next-break)))
          (break-time-difference
            (when type-break-mode
              (type-break-time-difference type-break-time-last-break nil)))
          (on-break (and type-break-mode
                      break-time-difference
                      (< break-time-difference type-break-good-break-interval)))
          (formatted-time
            (cond
              (on-break
                (format-seconds "%02m:%02s"
                  (- type-break-good-break-interval break-time-difference)))
              (time-difference (format-seconds "%02m:%02s" time-difference))
              (t "00:00")))
          (percent
            (if (and type-break-mode time-difference)
              (/ (* 100.0 time-difference) type-break-interval)
              0))
          (clocked-in (and (fboundp 'org-clocking-p) (org-clocking-p)))
          (task-text
            (cond
              (on-break "Take a break - relax your hands")
              ((not clocked-in) "No Active Task")
              (t org-clock-heading))))
    `((enabled . ,(if type-break-mode t :json-false))
       (on-break . ,(if on-break t :json-false))
       (clocked-in . ,(if clocked-in t :json-false))
       (percent . ,percent)
       (time . ,formatted-time)
       (task . ,task-text)
       (summary . ,(concat task-text " " formatted-time))
       (keystrokes-target . ,(if type-break-mode
                               (or (cdr type-break-keystroke-threshold) 0)
                               0))
       (keystrokes . ,(if type-break-mode type-break-keystroke-count 0)))))


;;; Agenda snapshot

(defun sn-tasks--tag-matches-p (filter tags)
  "Return non-nil when FILTER matches TAGS.
Accepts either `tag' or `@tag' forms in either FILTER or TAGS."
  (let ((candidates (list filter (concat "@" filter)
                     (replace-regexp-in-string "^@" "" filter))))
    (seq-some (lambda (c) (member c tags)) candidates)))

(defun sn-tasks--agenda-rows (tag)
  "Return a list of plists for agenda entries matching TAG."
  (let ((org-todo-keywords sn-tasks-todo-keywords)
         (org-agenda-files sn-tasks-agenda-files)
         (rows '()))
    (dolist (file sn-tasks-agenda-files)
      (when (file-exists-p (expand-file-name file))
        (with-current-buffer (find-file-noselect (expand-file-name file))
          (org-with-wide-buffer
            (goto-char (point-min))
            (while (re-search-forward org-heading-regexp nil t)
              (let* ((todo (substring-no-properties (or (org-get-todo-state) "")))
                      (tags (mapcar #'substring-no-properties (org-get-tags))))
                (when (and (not (string-empty-p todo))
                        (sn-tasks--tag-matches-p tag tags)
                        (not (member "ARCHIVE" tags)))
                  (push (list :title (substring-no-properties
                                       (org-get-heading t t t t))
                          :state todo
                          :tags tags
                          :marker (point-marker))
                    rows))))))))
    (nreverse rows)))

(defun sn-tasks--task-counts (rows)
  "Return (total . visible) counts of non-PROJECT entries in ROWS."
  (let ((total 0) (visible 0) (i 0))
    (dolist (r rows)
      (let ((state (plist-get r :state)))
        (when (and state (not (string= state "PROJECT")))
          (cl-incf total)
          (when (< i sn-tasks-max)
            (cl-incf visible))))
      (cl-incf i))
    (cons total visible)))


;;; Clocktable reports

(defun sn-tasks--clocktable (block)
  "Return a clocktable as formatted plain text for BLOCK (e.g. \\='today, \\='thisweek)."
  (let ((org-agenda-files sn-tasks-agenda-files))
    (with-temp-buffer
      (org-mode)
      (insert (format "#+BEGIN: clocktable :scope agenda :maxlevel 3 :block %s :match \"@work\" :tags t :emphasize t :fileskip0 t :indent t :hidefiles t :link nil\n#+END:\n"
                block))
      (goto-char (point-min))
      (org-update-dblock)
      (buffer-substring-no-properties (point-min) (point-max)))))


;;; Public API (called from Quickshell)

;;;###autoload
(defun sn-tasks/snapshot (&optional filter)
  "Return a JSON string describing pomodoro + tasks for Quickshell.
If FILTER is supplied, update the persisted filter first."
  (when filter (sn-tasks/set-filter filter))
  (let* ((tag (sn-tasks/get-filter))
          (rows (sn-tasks--agenda-rows tag))
          (counts (sn-tasks--task-counts rows))
          (visible (cl-subseq rows 0 (min sn-tasks-max (length rows))))
          (json-payload
            `((filter . ,tag)
               (pomodoro . ,(sn-tasks--pomodoro))
               (counts . ((total . ,(car counts))
                           (visible . ,(cdr counts))))
               (tasks . ,(vconcat
                          (mapcar (lambda (r)
                                    `((title . ,(plist-get r :title))
                                       (state . ,(plist-get r :state))
                                       (tags . ,(vconcat (plist-get r :tags)))))
                            visible))))))
    (json-encode json-payload)))

;;;###autoload
(defun sn-tasks/report (period)
  "Return a plain-text clocktable for PERIOD (\"today\" or \"thisweek\")."
  (sn-tasks--clocktable (intern period)))

;;;###autoload
(defun sn-tasks/clock-in-by-title (title)
  "Clock into the first agenda entry whose heading matches TITLE."
  (catch 'done
    (dolist (file sn-tasks-agenda-files)
      (when (file-exists-p (expand-file-name file))
        (with-current-buffer (find-file-noselect (expand-file-name file))
          (org-with-wide-buffer
            (goto-char (point-min))
            (while (re-search-forward org-heading-regexp nil t)
              (when (string= (org-get-heading t t t t) title)
                (org-clock-in)
                (throw 'done t)))))))
    nil))

;;;###autoload
(defun sn-tasks/clock-out ()
  "Clock out of the current task, if any."
  (when (org-clocking-p) (org-clock-out))
  t)

;;;###autoload
(defun sn-tasks/mark-done ()
  "Mark the currently clocked task DONE."
  (when (org-clocking-p)
    (save-excursion
      (org-clock-goto)
      (org-todo "DONE")))
  t)

;;;###autoload
(defun sn-tasks/type-break-toggle ()
  "Toggle type-break-mode."
  (if type-break-mode (type-break-mode -1) (type-break-mode 1))
  (if type-break-mode t :json-false))

(provide 'sn-tasks)
;;; sn-tasks.el ends here
