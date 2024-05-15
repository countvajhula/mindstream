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

(defun mindstream--starting-file-for-session (dir)
  "Select an appropriate starting file for a session.

DIR is expected to be a path to an existing folder."
  (let ((files (mindstream--directory-files-recursively dir)))
    (if (and files (= 1 (length files)))
        (car files)
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

This only removes implicit versioning. It does not close any open
buffers at the SESSION path."
  (interactive (list (completing-read "Which session? "
                                      mindstream-active-sessions
                                      ;; This excludes all anonymous
                                      ;; sessions, which we should be
                                      ;; able to also end.

                                      ;; (lambda (session)
                                      ;;   (not
                                      ;;    (string-prefix-p mindstream-path
                                      ;;                     session)))
                                      )))
  ;; Since we have completing-read above, do we have a case when session
  ;; is nil and use `mind-stream--curent-session' at all?
  (let ((session (or session (mindstream--current-session))))
    (setq mindstream-active-sessions
          (remove session mindstream-active-sessions))
    (message "Session %s ended." session)))

(defun mindstream--current-session ()
  "The session at the current path, if it is active."
  (and (member default-directory mindstream-active-sessions)
       default-directory))

(defun mindstream-session-p (&optional path)
  "Predicate to check whether PATH is an active session."
  (let ((path (or path default-directory)))
    (member path mindstream-active-sessions)))

(defun mindstream-start-anonymous-session (&optional template)
  "Start a new anonymous session.

This creates a new directory and Git repository for the new session
after copying over the contents of TEMPLATE if one is specified.
Otherwise, it uses the configured default template.

New sessions always start anonymous."
  (let* ((session (mindstream--unique-name))
         (base-path (mindstream--generate-anonymous-session-path session))
         (template (or template mindstream-default-template)))
    (unless (file-directory-p base-path)
      (copy-directory template base-path)
      (mindstream-backend-initialize base-path)
      (let ((filename (mindstream--starting-file-for-session base-path)))
        ;; end existing anonymous session for this major mode
        (mindstream--end-anonymous-session
         (mindstream--infer-major-mode
          filename))
        (find-file
         (expand-file-name filename
                           base-path))
        (mindstream--initialize-buffer)
        (rename-buffer (mindstream-anonymous-buffer-name))
        (mindstream-begin-session)
        (current-buffer)))))

(defun mindstream--iterate ()
  "Commit the current state as part of iteration."
  ;; always add the current file (where mindstream-session-mode
  ;; should be active) to the backend index before iteration
  (mindstream-backend-add-file
   (file-name-nondirectory (buffer-file-name)))
  (mindstream-backend-iterate))

(defun mindstream--generate-anonymous-session-path (session)
  "A path on disk to use for a newly created SESSION."
  (mindstream--build-path mindstream-path
                          session))

(defun mindstream--template (&optional name)
  "Path to template NAME.

If NAME isn't provided, use the default template."
  (mindstream--build-path mindstream-template-path
                          (or name mindstream-default-template)))

(defun mindstream--ensure-anonymous-path ()
  "Ensure that the anonymous session path exists."
  (unless (file-directory-p mindstream-path)
    (mkdir mindstream-path t)))

(defun mindstream--ensure-templates-exist ()
  "Ensure that the templates directory exists and contains the default template."
  (unless (file-directory-p mindstream-template-path)
    (let ((default-template-dir (mindstream--template)))
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

(defun mindstream--infer-major-mode (filename)
  "Infer a major mode to use.

Given a FILENAME, this infers the appropriate major mode based on the
file extension by consulting Emacs."
  (let ((extension (concat "." (file-name-extension filename))))
    (let ((mode (mindstream--major-mode-for-file-extension extension)))
      (or mode
          (error "No major modes recognize the template extension '%s'!" extension)))))

(defun mindstream--mode-name (major-mode-to-use)
  "Human readable name of MAJOR-MODE-TO-USE.

If MAJOR-MODE-TO-USE is not provided, the major mode of the current
buffer is used."
  (if major-mode-to-use
      (string-trim-right
       (capitalize
        (substring
         (symbol-name major-mode-to-use)
         0 -5)))
    (if (stringp mode-name)
        ;; on older versions of Emacs
        mode-name
      (car mode-name))))

(defun mindstream-anonymous-buffer-name (&optional major-mode-to-use)
  "Name of the anonymous session buffer for MAJOR-MODE-TO-USE."
  (concat "*"
          mindstream-anonymous-buffer-prefix
          " - "
          (mindstream--mode-name major-mode-to-use)
          "*"))

(defun mindstream--get-anonymous-session-buffer (&optional major-mode-to-use)
  "Get the active anonymous session buffer for MAJOR-MODE-TO-USE, if it exists."
  (let ((buffer-name (mindstream-anonymous-buffer-name major-mode-to-use)))
    (get-buffer buffer-name)))

(defun mindstream-anonymous-session-buffer-p ()
  "Predicate to check if the current buffer is the anonymous scratch buffer."
  ;; TODO: this is fairly weak
  (string-match-p mindstream-anonymous-buffer-prefix (buffer-name)))

(provide 'mindstream-session)
;;; mindstream-session.el ends here
