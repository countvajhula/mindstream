;;; mindstream-session.el --- Start writing, stay focused, don't worry -*- lexical-binding: t -*-

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

;; Utilities for Mindstream sessions and buffers

;;; Code:

(require 'mindstream-custom)
(require 'mindstream-backend)
(require 'mindstream-util)

(defvar mindstream-active-sessions nil
  "A set of active sessions.

Sessions are just paths to git repos.  A path being an \"active
session\" means that any and all changes made at that path to
git-tracked files will be versioned if they pull any
`mindstream-triggers`.

For now, this is implemented as a list for simplicity, since the
number of active sessions is likely to be small.")

(defvar mindstream-session-file-history nil)

(defvar mindstream-session-history nil)

(defun mindstream--unique-name ()
  "Generate a unique name."
  (let ((time (current-time)))
    (concat (format-time-string "%F" time)
            "-"
            (sha1 (format "%s" time)))))

(defun mindstream--session-file-name-relative (file dir)
  "Return relative FILE name for `mindstream-session-history'.

This returns the path of FILE relative to DIR if FILE is in DIR,
otherwise it returns an abbreviated path (e.g. starting with ~
if it is in the home folder)."
  (if (string-match-p
       (expand-file-name dir)
       (expand-file-name file))
      (file-relative-name file dir)
    (abbreviate-file-name file)))

(defun mindstream--starting-file-for-session (dir)
  "Select an appropriate starting file for a session.

DIR is expected to be a path to an existing folder.  This returns a
filename relative to DIR rather than an absolute path."
  (let ((files (mindstream--directory-files-recursively dir)))
    (if (and files (= 1 (length files)))
        (mindstream--session-file-name-relative (car files)
                                                dir)
      (completing-read "Which file? "
                       (mapcar (lambda (f)
                                 (mindstream--session-file-name-relative f
                                                                         dir))
                               files)
                       nil t nil
                       mindstream-session-file-history))))

(defun mindstream-begin-session ()
  "Begin a session at the current path."
  (interactive)
  (push default-directory mindstream-active-sessions)
  (add-to-list 'mindstream-session-history
               (mindstream--session-file-name-relative default-directory
                                                       mindstream-save-session-path))
  (message "Session started at %s." default-directory))

(defun mindstream-end-session (&optional session)
  "End SESSION.

This only removes implicit versioning.  It does not close any open
buffers at the SESSION path."
  (interactive (list (completing-read "Which session? "
                                      mindstream-active-sessions
                                      ;; We typically end sessions on existing
                                      ;; projects to stop implicit versioning.
                                      ;; Exclude anonymous sessions here since
                                      ;; it's OK if those aren't ended (we can
                                      ;; just forget about them and move on).
                                      (lambda (session)
                                        (not
                                         (string-prefix-p mindstream-path
                                                          session))))))
  (let ((session (or session (mindstream--current-session))))
    ;; session can be nil if called non-interactively
    (setq mindstream-active-sessions
          (remove session mindstream-active-sessions))
    (message "Session %s ended." session)))

(defun mindstream--current-session ()
  "The session at the current path, if it is active."
  (and (member default-directory mindstream-active-sessions)
       default-directory))

(defun mindstream-native-session-p (&optional path)
  "Predicate to check whether PATH is a standard Mindstream path.

That is, either the anonymous session path or the archive path."
  (let ((path (or path default-directory)))
    (or (mindstream-anonymous-session-p path)
        (mindstream-archived-session-p path))))

(defun mindstream-session-p (&optional path)
  "Predicate to check whether PATH is an active session."
  (let ((path (or path default-directory)))
    (or (member path mindstream-active-sessions)
        (mindstream-native-session-p path))))

(defun mindstream--iterate ()
  "Commit the current state as part of iteration."
  ;; always add the current file (where mindstream-session-mode
  ;; should be active) to the backend index before iteration
  (mindstream-backend-add-file
   (file-name-nondirectory (buffer-file-name)))
  (mindstream-backend-iterate))

(defun mindstream--generate-anonymous-session-path (template)
  "A path on disk to use for a newly created SESSION.

This creates an appropriate base path on disk for the TEMPLATE if it
isn't already present."
  (let* ((session-name (mindstream--unique-name))
         (base-path (mindstream--anonymous-path template)))
    (mindstream--ensure-path base-path)
    (mindstream--build-path base-path
                            session-name)))

(defun mindstream--template-path (&optional template)
  "Path to TEMPLATE.

TEMPLATE is expected to be a simple name corresponding to the name of
a folder at `mindstream-template-path'.  If it isn't provided, use the
default template."
  (mindstream--build-path mindstream-template-path
                          (or template mindstream-default-template)))

(defun mindstream--anonymous-path (&optional template)
  "Path to anonymous sessions using TEMPLATE.

TEMPLATE is expected to be a simple name corresponding to the name of
a folder at `mindstream-template-path'.  If it isn't provided, use the
default template."
  (mindstream--build-path mindstream-path
                          (or template mindstream-default-template)))

(defun mindstream--archive-path (&optional template)
  "Path to archived sessions using TEMPLATE.

TEMPLATE is expected to be a simple name corresponding to the name of
a folder at `mindstream-template-path'.  If it isn't provided, use the
default template."
  (mindstream--build-path mindstream-archive-path
                          (or template mindstream-default-template)))

(defun mindstream--template-name (path)
  "Name of template at PATH."
  (file-name-base path))

(defun mindstream--ensure-path (path)
  "Ensure that PATH exists on the filesystem."
  (unless (file-directory-p path)
    (mkdir path t)))

(defun mindstream--ensure-paths ()
  "Ensure that paths that mindstream needs to function exist."
  (mindstream--ensure-path mindstream-path)
  (mindstream--ensure-path mindstream-archive-path))

(defun mindstream--ensure-templates-exist ()
  "Ensure that the templates directory exists and contains the default template."
  (unless (file-directory-p mindstream-template-path)
    (let ((default-template-dir (mindstream--template-path)))
      (mkdir default-template-dir t)
      (let ((template-file
             (mindstream--build-path default-template-dir
                                     (concat mindstream-anonymous-buffer-prefix
                                             ".txt"))))
        (let ((buf (generate-new-buffer "default-template")))
          (with-current-buffer buf
            (insert mindstream-default-template-contents)
            (write-file template-file))
          (kill-buffer buf))))))

(defun mindstream--initialize-buffer ()
  "Initialize a newly created buffer."
  (setq buffer-offer-save nil)
  ;; place point at the end of the buffer
  (goto-char (point-max)))

(defun mindstream--find-template-for-mode (major-mode-to-use)
  "Find an appropriate template to use for a major mode.

Search the templates folder for a template recognizable to MAJOR-MODE-TO-USE."
  (catch 'return
    (dolist (assoc auto-mode-alist)
      (pcase-let ((`(,ext . ,mode) assoc))
        (when (eq mode major-mode-to-use)
          (let ((file (mindstream--file-with-extension ext
                                                       mindstream-template-path)))
            (when file
              (throw 'return (mindstream--directory-name file)))))))
    (error "Template not found for %s! Please create one and try again"
           major-mode-to-use)))

(defun mindstream--infer-template (major-mode-to-use)
  "Infer template to use based on MAJOR-MODE-TO-USE."
  (let ((template-name (plist-get mindstream-preferred-template
                                  major-mode-to-use)))
    (or template-name
        (mindstream--find-template-for-mode major-mode-to-use))))

(defun mindstream-anonymous-session-p (&optional path)
  "Predicate to check if PATH is part of an anonymous session.

If PATH isn't specified, assume the current file."
  (mindstream--path-in-tree-p (or path (buffer-file-name))
                              mindstream-path))

(defun mindstream-archived-session-p (&optional path)
  "Predicate to check if PATH is part of an archived session.

If PATH isn't specified, assume the current file."
  (mindstream--path-in-tree-p (or path (buffer-file-name))
                              mindstream-archive-path))

(provide 'mindstream-session)
;;; mindstream-session.el ends here
