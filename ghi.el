(require 'magit-core)
(require 'magit-diff)
(require 'magit-apply)
(require 'magit-log)
(require 'magit)

(defvar ghi-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    (define-key map "jz" 'magit-jump-to-stashes)
    (define-key map "jt" 'magit-jump-to-tracked)
    (define-key map "jn" 'magit-jump-to-untracked)
    (define-key map "ju" 'magit-jump-to-unstaged)
    (define-key map "js" 'magit-jump-to-staged)
    (define-key map "jfu" 'magit-jump-to-unpulled-from-upstream)
    (define-key map "jfp" 'magit-jump-to-unpulled-from-pushremote)
    (define-key map "jpu" 'magit-jump-to-unpushed-to-upstream)
    (define-key map "jpp" 'magit-jump-to-unpushed-to-pushremote)
    map)
  "Keymap for `ghi-list-mode'.")

(define-derived-mode ghi-list-mode magit-mode "ghi"
  "Mode for looking at ghi-list

\\<magit-mode-map>\
Type \\[magit-refresh] to refresh the current buffer.
Type \\[magit-section-toggle] to expand or hide the section at point.
Type \\[magit-visit-thing] to visit the change or commit at point.

Type \\[magit-dispatch-popup] to see available prefix popups.

Staging and applying changes is documented in info node
`(magit)Staging and unstaging' and info node `(magit)Applying'.

\\<magit-hunk-section-map>Type \
\\[magit-apply] to apply the change at point, \
\\[magit-stage] to stage,
\\[magit-unstage] to unstage, \
\\[magit-discard] to discard, or \
\\[magit-reverse] to reverse it.

\\<ghi-list-mode-map>\
Type \\[magit-commit-popup] to create a commit.

\\{ghi-list-mode-map}"
  :group 'ghi-list
  (hack-dir-local-variables-non-file-buffer))

(defun ghi-list-issues ()
  (interactive)
  (ghi--display
   (shell-command-to-string "ghi list -P")))

;;;###autoload
(defun ghi-list (&optional directory)
  (interactive
   (list (and (or current-prefix-arg (not (magit-toplevel)))
              (magit-read-repository
               (>= (prefix-numeric-value current-prefix-arg) 16)))))
  (if directory
      (let ((toplevel (magit-toplevel directory)))
        (setq directory (file-name-as-directory (expand-file-name directory)))
        (if (and toplevel (string-equal directory toplevel))
            (ghi-list-internal directory)
          (when (y-or-n-p
                 (if toplevel
                     (format "%s is a repository.  Create another in %s? "
                             toplevel directory)
                   (format "Create repository in %s? " directory)))
            (magit-init directory))))
    (ghi-list-internal default-directory)))

(put 'ghi-list 'interactive-only 'ghi-list-internal)

;;;###autoload
(defun ghi-list-internal (directory)
  (magit-tramp-asserts directory)
  (let ((default-directory directory))
    (magit-mode-setup #'ghi-list-mode)))

(defun ghi-process-file (&rest args)
  "Process files synchronously in a separate process.
Identical to `process-file' but temporarily enable Cygwin's
\"noglob\" option during the call and ensure unix eol
conversion."
  (let ((process-environment process-environment)
        (default-process-coding-system (magit--process-coding-system)))
    (apply #'process-file args)))

(defun ghi-insert (&rest args)
  "Execute Git with ARGS, inserting its output at point.
If Git exits with a non-zero exit status, then show a message and
add a section in the respective process buffer."
    (apply #'ghi-process-file "ghi"
           nil (list t nil) nil args))

(defun ghi-list-wash (washer &rest args)
  "Execute Git with ARGS, inserting washed output at point.
Actually first insert the raw output at point.  If there is no
output call `magit-cancel-section'.  Otherwise temporarily narrow
the buffer to the inserted text, move to its beginning, and then
call function WASHER with no argument."
  (declare (indent 1))
  (let ((beg (point)))
    (setq args (-flatten args))
    (ghi-insert args)
    (if (= (point) beg)
        (magit-cancel-section)
      (unless (bolp)
        (insert "\n"))
      (save-restriction
        (narrow-to-region beg (point))
        (goto-char beg)
        (funcall washer args))
      (when (or (= (point) beg)
                (= (point) (1+ beg)))
        (magit-cancel-section)))))

(cl-defun ghi-list-insert-list-header
    (&optional (branch (magit-get-current-branch))
               (push   (magit-get-push-branch branch)))
  "Insert a header line about the branch the current branch is pushed to."
  (when push
    (magit-insert-section (ghi-test "header")
      (magit-insert-heading "Git issues list")
      (ghi-list-wash (apply-partially 'magit-log-wash-log 'log) "list" "-P")
      (magit-section-cache-visibility)
      )))

(defvar ghi-list-insert-hook
  '(ghi-list-insert-list-header))

(defun ghi-list-insert-list ()
  "Insert list appropriate for `ghi-list-mode' buffers.
The sections are inserted by running the functions on the hook
`magit-status-headers-hook'."
  (magit-insert-headers ghi-list-insert-hook))

(defcustom ghi-list-sections-hook
  '(ghi-list-insert-list)
  "Hook run to insert sections into a ghi buffer."
  :group 'ghi-list
  :type 'hook)

(defvar ghi-list-sections-hook-1 nil)

(defvar ghi-list-refresh-hook nil
  "Hook run after a ghi-list buffer has been refreshed.")

(defun ghi-list-refresh-buffer ()
  (magit-git-exit-code "update-index" "--refresh")
  (magit-insert-section (ghi-list)
    (if (-all-p #'functionp ghi-list-sections-hook)
        (run-hooks 'ghi-list-sections-hook)
      (message "`ghi-list-sections-hook' contains entries that are \
no longer valid.\nUsing standard value instead.  Please re-configure")
      (sit-for 5)
      (let ((ghi-list-sections-hook-1
             (eval (car (get 'ghi-list-sections-hook 'standard-value)))))
        (run-hooks 'ghi-list-sections-hook-1))))
  (run-hooks 'ghi--refresh-hook))
