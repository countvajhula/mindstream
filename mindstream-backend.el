;;; mindstream-backend.el --- Start writing, stay focused, don't worry -*- lexical-binding: t -*-

;; URL: https://github.com/countvajhula/mindstream

;; This work is "part of the world." You are free to do whatever you
;; like with it and it isn't owned by anybody, not even the creators.
;; Attribution would be appreciated and would help, but it is not
;; strictly necessary nor required.

;; The freely released, copyright-free work in this repository
;; represents an investment in a better way of doing things called
;; attribution-based economics (ABE). Attribution-based economics is
;; based on the simple idea that we gain more by giving more, not by
;; holding on to things that, truly, we could only create because we,
;; in our turn, received from others. As it turns out, an economic
;; system based on attribution -- where those who give more are more
;; empowered -- is significantly more efficient than capitalism while
;; also being stable and fair (unlike capitalism, on both counts),
;; giving it transformative power to elevate the human condition and
;; address the problems that face us today along with a host of others
;; that have been intractable since the beginning. You can help make
;; this a reality by releasing your work in the same way -- freely
;; into the public domain in the simple hope of providing value. Learn
;; more about attribution-based economics at drym.org, tell your
;; friends, do your part.

;; This is free and unencumbered software released into the public domain.
;; The authors relinquish any copyright claims on this work.
;;

;;; Commentary:

;; Backend (e.g. Git) abstraction for Mindstream

;;; Code:

(require 'mindstream-custom)

;; collisions unlikely within a single project repo
;; and we don't want branch names to be too long as they
;; may make things awkward in the Git UI.
(defconst mindstream-branch-name-length 7)

(defun mindstream--execute-shell-command (command &optional directory)
  "Execute shell COMMAND at DIRECTORY.

COMMAND is expected to be a list of strings, the first element of
which is the command itself, and the remaining elements are the flags
and arguments that are to be supplied to the command."
  (let ((cmd (car command))
        (args (cdr command))
        (default-directory (or directory
                               (and (buffer-file-name)
                                    (file-name-directory
                                     (buffer-file-name)))
                               default-directory)))
    (let ((exit-code (apply #'call-process cmd nil nil nil args)))
      exit-code)))

(defun mindstream--execute-shell-command-to-string (command &optional directory)
  "Execute COMMAND at DIRECTORY and return its stdout as a trimmed string.
Returns nil if the command exits with a non-zero status.

See `mindstream--execute-shell-command' for the format of COMMAND."
  (let ((cmd (car command))
        (args (cdr command))
        (default-directory (or directory
                               (and (buffer-file-name)
                                    (file-name-directory
                                     (buffer-file-name)))
                               default-directory)))
    (with-temp-buffer
      ;; 't' tells call-process to insert stdout into this temp buffer
      (let ((exit-code (apply #'call-process cmd nil t nil args)))
        (when (eq exit-code 0)
          (string-trim (buffer-string)))))))

(defun mindstream-backend-initialize (base-path)
  "Initialize the backend at the path BASE-PATH."
  (mindstream--execute-shell-command (list "git" "init")
                                     base-path))

(defun mindstream-backend-iterate ()
  "Iterate using the backend (e.g. git)."
  (when mindstream-add-everything
    ;; if this is configured, then add all files
    ;; in the repo that aren't gitignored, even
    ;; if they aren't being visited right now
    (mindstream--execute-shell-command
     (list "git" "add" "-A")))
  (mindstream--execute-shell-command
   (list "git" "commit" "-a" "--allow-empty-message" "-m" "")))

(defun mindstream-backend-add-file (file)
  "Add FILE to the Git index."
  (mindstream--execute-shell-command
   (list "git" "add" file)))

(defun mindstream-backend-root (&optional buffer)
  "Get the root folder of the VCS for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (mindstream--execute-shell-command-to-string
     (list "git" "rev-parse" "--show-toplevel"))))

(defun mindstream-git-branch-name (&optional buffer)
  "Get the Git branch name for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (mindstream--execute-shell-command-to-string
     (list "git" "branch" "--show-current"))))

(defun mindstream--current-version (&optional buffer)
  "Get the Git version hash for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (mindstream--execute-shell-command-to-string
     (list "git" "rev-parse" "HEAD"))))

(defun mindstream-create-git-branch (&optional name)
  "Start a new branch (stream) in the current repo.

Note that this prefixes NAME with `mindstream-branch-prefix' before
creating the branch."
  (let ((branch-name (concat mindstream-branch-prefix
                             "-"
                             (or name
                                 (mindstream--unique-name mindstream-branch-name-length))))
        (start-point (mindstream--current-version)))
    (mindstream--execute-shell-command
     (list "git" "checkout" "-b" branch-name start-point))))

(defun mindstream-backend-rename-branch (new-name)
  "Rename the current branch to NEW-NAME.

Note that this prefixes NAME with `mindstream-branch-prefix' before
renaming the branch."
  (let ((full-name (concat mindstream-branch-prefix "-" new-name)))
    (mindstream--execute-shell-command (list "git" "branch" "-m" full-name))))

(defun mindstream--session-dir (&optional buffer)
  "The repo base path containing BUFFER."
  (abbreviate-file-name
   (mindstream-backend-root buffer)))

(provide 'mindstream-backend)
;;; mindstream-backend.el ends here
