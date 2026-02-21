;;; test-consult-taskfile.el --- Tests for consult-taskfile -*- lexical-binding: t; -*-

(require 'ert)
(require 'json)
(require 'cl-lib)

;; Load the package being tested
(require 'consult-taskfile)

;;; Test Data

(defvar test-consult-taskfile-json-output
  "{\"tasks\":{\"build\":{\"name\":\"build\",\"desc\":\"Build the project\",\"up_to_date\":false},\"test\":{\"name\":\"test\",\"desc\":\"Run tests\",\"up_to_date\":true},\"deploy\":{\"name\":\"deploy\",\"desc\":\"Deploy to production\",\"up_to_date\":false}}}"
  "Mock JSON output from task --list --json.")

(defvar test-consult-taskfile-text-output
  "task: Available tasks for this project:

* build:    Build the project
* test:     Run tests
* deploy:   Deploy to production"
  "Mock text output from task --list.")

;;; Helper Functions

(defmacro with-mock-process-call (return-value output &rest body)
  "Mock `call-process' to return RETURN-VALUE and OUTPUT."
  `(cl-letf (((symbol-function 'call-process)
			  (lambda (program &optional infile destination display &rest args)
				(when destination
				  (with-current-buffer destination
					(insert ,output)))
				,return-value)))
	 ,@body))

(defmacro with-temp-taskfile-dir (&rest body)
  "Execute BODY in a temporary directory with a test Taskfile."
  `(let ((temp-dir (make-temp-file "test-taskfile-" t)))
	 (unwind-protect
		 (let ((default-directory temp-dir))
		   (with-temp-file (expand-file-name "Taskfile.yml" temp-dir)
			 (insert "version: '3'

tasks:
  build:
	desc: Build the project
	cmds:
	  - echo \"Building...\"

  test:
	desc: Run tests
	cmds:
	  - echo \"Running tests...\"

  deploy:
	desc: Deploy to production
	cmds:
	  - echo \"Deploying...\""))
		   ,@body)
	   (delete-directory temp-dir t))))

;;; Tests for Core Functions

(ert-deftest test-consult-taskfile--project-root ()
  "Test project root detection."
  (let ((default-directory "/tmp/test"))
	;; Mock project-current to return nil (no project)
	(cl-letf (((symbol-function 'project-current) (lambda () nil)))
	  (should (string= (consult-taskfile--project-root) "/tmp/test")))

	;; Mock project-current to return a project
	(cl-letf (((symbol-function 'project-current)
			   (lambda () '(vc . "/home/user/project")))
			  ((symbol-function 'project-root)
			   (lambda (project) "/home/user/project/")))
	  (should (string= (consult-taskfile--project-root) "/home/user/project/")))))

(ert-deftest test-consult-taskfile--get-working-directory ()
  "Test working directory function."
  (let ((consult-taskfile-directory-function (lambda () "/custom/dir")))
	(should (string= (consult-taskfile--get-working-directory) "/custom/dir"))))

(ert-deftest test-consult-taskfile--get-tasks-json-success ()
  "Test task parsing from JSON output."
  (with-mock-process-call 0 test-consult-taskfile-json-output
	(let ((tasks (consult-taskfile--get-tasks)))
	  (should (= (length tasks) 3))
	  (should (member "build" tasks))
	  (should (member "test" tasks))
	  (should (member "deploy" tasks))

	  ;; Test annotations
	  (let ((build-task (cl-find "build" tasks :test #'string=)))
		(should (string-match-p "✗.*Build the project"
							   (get-text-property 0 'annotation build-task))))

	  (let ((test-task (cl-find "test" tasks :test #'string=)))
		(should (string-match-p "✓.*Run tests"
							   (get-text-property 0 'annotation test-task)))))))

(ert-deftest test-consult-taskfile--get-tasks-json-failure-fallback ()
  "Test fallback to text parsing when JSON fails."
  ;; First call (JSON) fails, second call (text) succeeds
  (let ((call-count 0))
	(cl-letf (((symbol-function 'call-process)
			   (lambda (program &optional infile destination display &rest args)
				 (cl-incf call-count)
				 (cond
				  ((and (= call-count 1) (member "--json" args))
				   ;; JSON call fails
				   1)
				  ((= call-count 2)
				   ;; Text call succeeds
				   (when destination
					 (with-current-buffer destination
					   (insert test-consult-taskfile-text-output)))
				   0)
				  (t 1)))))
	  (let ((tasks (consult-taskfile--get-tasks)))
		(should (= (length tasks) 3))
		(should (member "build" tasks))
		(should (member "test" tasks))
		(should (member "deploy" tasks))))))

(ert-deftest test-consult-taskfile--get-tasks-both-fail ()
  "Test when both JSON and text parsing fail."
  (with-mock-process-call 1 ""
	(let ((tasks (consult-taskfile--get-tasks)))
	  (should (null tasks)))))

(ert-deftest test-consult-taskfile--run-basic ()
  "Test basic task running."
  (let ((compile-called nil)
		(compile-command nil))
	(cl-letf (((symbol-function 'compile)
			   (lambda (cmd)
				 (setq compile-called t
					   compile-command cmd))))
	  (consult-taskfile--run "build")
	  (should compile-called)
	  (should (string-match-p "task build" compile-command)))))

(ert-deftest test-consult-taskfile--run-with-args ()
  "Test task running with arguments."
  (let ((compile-called nil)
		(compile-command nil))
	(cl-letf (((symbol-function 'compile)
			   (lambda (cmd)
				 (setq compile-called t
					   compile-command cmd))))
	  (consult-taskfile--run "build" '("--force" "--verbose"))
	  (should compile-called)
	  (should (string-match-p "task build --force --verbose" compile-command)))))

(ert-deftest test-consult-taskfile--run-with-environment ()
  "Test task running with environment variables."
  (let ((consult-taskfile-environment-variables '(("NODE_ENV" . "test") ("DEBUG" . "1")))
		(process-env-called nil)
		(compile-called nil))
	(cl-letf (((symbol-function 'compile)
			   (lambda (cmd)
				 (setq compile-called t
					   process-env-called process-environment))))
	  (consult-taskfile--run "test")
	  (should compile-called)
	  (should (cl-some (lambda (env) (string-match-p "NODE_ENV=test" env))
					   process-env-called))
	  (should (cl-some (lambda (env) (string-match-p "DEBUG=1" env))
					   process-env-called)))))

(ert-deftest test-consult-taskfile--run-hooks ()
  "Test that hooks are called properly."
  (let ((before-hook-called nil)
		(after-hook-called nil)
		(hook-task nil)
		(hook-args nil))
	(add-hook 'consult-taskfile-before-run-hook
			  (lambda (task args)
				(setq before-hook-called t
					  hook-task task
					  hook-args args)))
	(add-hook 'consult-taskfile-after-run-hook
			  (lambda (task args)
				(setq after-hook-called t)))

	(cl-letf (((symbol-function 'compile) (lambda (cmd) nil)))
	  (consult-taskfile--run "build" '("--force")))

	(should before-hook-called)
	(should after-hook-called)
	(should (string= hook-task "build"))
	(should (equal hook-args '("--force")))

	;; Clean up hooks
	(setq consult-taskfile-before-run-hook nil
		  consult-taskfile-after-run-hook nil)))

;;; Integration Tests

(ert-deftest test-consult-taskfile-annotation-function ()
  "Test annotation function."
  (let ((candidate (propertize "test-task" 'annotation "✓ Test annotation")))
	(should (string= (consult-taskfile--annotate candidate) "✓ Test annotation"))))

(ert-deftest test-consult-taskfile-custom-directory-function ()
  "Test custom directory function."
  (let ((consult-taskfile-directory-function (lambda () "/custom/project/dir"))
		(default-directory-used nil))
	(with-mock-process-call 0 test-consult-taskfile-json-output
	  (cl-letf (((symbol-function 'call-process)
				 (lambda (program &optional infile destination display &rest args)
				   (setq default-directory-used default-directory)
				   (when destination
					 (with-current-buffer destination
					   (insert test-consult-taskfile-json-output)))
				   0)))
		(consult-taskfile--get-tasks)
		(should (string= default-directory-used "/custom/project/dir"))))))

;;; Configuration Tests

(ert-deftest test-consult-taskfile-default-args ()
  "Test default arguments configuration."
  (let ((consult-taskfile-default-args '("--verbose" "--force"))
		(compile-command nil))
	(cl-letf (((symbol-function 'compile)
			   (lambda (cmd) (setq compile-command cmd))))
	  (consult-taskfile--run "build")
	  (should (string-match-p "--verbose --force" compile-command)))))

;;; Error Handling Tests

(ert-deftest test-consult-taskfile-invalid-json ()
  "Test handling of invalid JSON."
  (with-mock-process-call 0 "{invalid json"
	;; Should not error, should fall back to text parsing
	(let ((tasks (consult-taskfile--get-tasks)))
	  ;; Since text parsing also fails in this mock, should return nil
	  (should (null tasks)))))

(ert-deftest test-consult-taskfile-command-not-found ()
  "Test handling when task command is not found."
  (with-mock-process-call 127 ""  ; Command not found exit code
	(let ((tasks (consult-taskfile--get-tasks)))
	  (should (null tasks)))))

;;; Test Runner

(defun run-consult-taskfile-tests ()
  "Run all consult-taskfile tests."
  (interactive)
  (ert-run-tests-batch "test-consult-taskfile"))

(provide 'test-consult-taskfile)
;;; test-consult-taskfile.el ends here
