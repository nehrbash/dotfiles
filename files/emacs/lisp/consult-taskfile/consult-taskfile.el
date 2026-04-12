;;; consult-taskfile.el --- Simple taskfile runner with consult -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "28.1") (consult "0.31") (transient "0.3.0"))
;;; Commentary:

;; Simple taskfile runner with consult for task selection and optional
;; transient menu for advanced options.

;;; Code:

(require 'consult)
(require 'transient)
(require 'compile)
(require 'project)

(defgroup consult-taskfile nil
  "Taskfile runner with consult."
  :group 'tools)

(defcustom consult-taskfile-default-args nil
  "Default arguments for task command."
  :type '(repeat string)
  :group 'consult-taskfile)

(defcustom consult-taskfile-environment-variables nil
  "Environment variables to set when running tasks.
Should be an alist of (VAR . VALUE) pairs."
  :type '(alist :key-type string :value-type string)
  :group 'consult-taskfile)

(defcustom consult-taskfile-directory-function #'consult-taskfile--project-root
  "Function to determine the directory to run tasks in.
Should return a directory path string."
  :type 'function
  :group 'consult-taskfile)

(defvar consult-taskfile-before-run-hook nil
  "Hook run before executing a task.
Functions are called with the task name and arguments.")

(defvar consult-taskfile-after-run-hook nil
  "Hook run after executing a task.
Functions are called with the task name and arguments.")

;;; Core Functions

(defun consult-taskfile--project-root ()
  "Get project root directory using project.el, fallback to 'default-directory'."
  (if-let* ((project (project-current)))
      (project-root project)
    default-directory))

(defun consult-taskfile--get-working-directory ()
  "Get the directory to run tasks in."
  (funcall consult-taskfile-directory-function))

(defun consult-taskfile--get-tasks ()
  "Get available tasks from taskfile."
  (let* ((default-directory (consult-taskfile--get-working-directory))
         (json-output (with-temp-buffer
                        (when (zerop (process-file "task" nil t nil "--list" "--json"))
                          (buffer-string)))))
    (if (and json-output (not (string-empty-p json-output)))
        ;; Try JSON parsing first
        (condition-case err
            (let* ((json-data (json-parse-string json-output :object-type 'alist))
                   (tasks (alist-get 'tasks json-data)))
              (if tasks
                  (mapcar (lambda (task)
                            (let ((name (alist-get 'name task))
                                  (desc (alist-get 'desc task))
                                  (up-to-date (alist-get 'up_to_date task)))
                              (propertize name
                                          'consult--candidate name
                                          'annotation (format "%s %s"
                                                            (if up-to-date "✓" "✗")
                                                            (or desc "")))))
                          tasks)
                (error "No tasks found in JSON output")))
          (json-parse-error
           (message "Failed to parse task JSON: %s" (error-message-string err))
           nil)
          (error
           (message "Error processing tasks: %s" (error-message-string err))
           nil))
      (message "Failed parsing json output"))))

(defun consult-taskfile--annotate (candidate)
  "Annotation function for CANDIDATE."
  (get-text-property 0 'annotation candidate))

(defun consult-taskfile--run (task &optional args)
  "Run TASK with optional ARGS."
  (let* ((default-directory (consult-taskfile--get-working-directory))
         (process-environment (append
                               (mapcar (lambda (env-var)
                                         (format "%s=%s" (car env-var) (cdr env-var)))
                                       consult-taskfile-environment-variables)
                               process-environment))
         (cmd (format "task %s %s"
                      (shell-quote-argument task)
                      (mapconcat #'shell-quote-argument (or args consult-taskfile-default-args) " "))))
    (run-hook-with-args 'consult-taskfile-before-run-hook task args)
    (compile (string-trim cmd))
    (run-hook-with-args 'consult-taskfile-after-run-hook task args)))

;;; Consult Integration

(defvar consult-taskfile--history nil
  "History for taskfile selection.")

;;;###autoload
(defun consult-taskfile ()
  "Select and run a taskfile task."
  (interactive)
  (if-let* ((tasks (consult-taskfile--get-tasks))
            ((not (null tasks))))
      (let ((task (consult--read tasks
                                 :prompt "Task: "
                                 :category 'taskfile
                                 :history 'consult-taskfile--history
                                 :annotate #'consult-taskfile--annotate
                                 :require-match t)))
        (consult-taskfile--run task))
    (user-error "No tasks found. Make sure you're in a directory with a Taskfile")))

;;; Transient Menu

(transient-define-argument consult-taskfile:--concurrency ()
  :description "Concurrency limit"
  :class 'transient-option
  :shortarg "-c"
  :argument "--concurrency="
  :reader 'transient-read-number-N+)

(transient-define-argument consult-taskfile:--output ()
  :description "Output format"
  :class 'transient-option
  :shortarg "-o"
  :argument "--output="
  :choices '("interleaved" "group" "prefixed"))

;;;###autoload
(transient-define-prefix consult-taskfile-menu ()
  "Transient menu for taskfile options."
  ["Options"
   ("-f" "Force" "--force")
   ("-v" "Verbose" "--verbose")
   ("-s" "Silent" "--silent")
   ("-w" "Watch" "--watch")
   ("-y" "Yes to all" "--yes")
   (consult-taskfile:--concurrency)
   (consult-taskfile:--output)]
  ["Actions"
   ("RET" "Run task" consult-taskfile-with-args)
   ("q" "Quit" transient-quit-one)])

(defun consult-taskfile-with-args (&optional args)
  "Run taskfile with ARGS from transient."
  (interactive (list (transient-args 'consult-taskfile-menu)))
  (if-let* ((tasks (consult-taskfile--get-tasks))
            ((not (null tasks))))
      (let ((task (consult--read tasks
                                 :prompt "Task: "
                                 :category 'taskfile
                                 :history 'consult-taskfile--history
                                 :annotate #'consult-taskfile--annotate
                                 :require-match t)))
        (consult-taskfile--run task args))
    (user-error "No tasks found.  Make sure you're in a directory with a Taskfile")))

(provide 'consult-taskfile)
;;; consult-taskfile.el ends here
