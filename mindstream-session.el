;;; mindstream-session.el --- Scratch buffer sessions -*- lexical-binding: t -*-

;; Author: Siddhartha Kasivajhula <sid@countvajhula.com>
;; URL: https://github.com/countvajhula/mindstream
;; Version: 0.0
;; Keywords: convenience, files, languages, outlines, tools, vc, wp

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

(require 'cl-lib)
(require 'mindstream-custom)
(require 'mindstream-backend)
(require 'mindstream-util)

(defvar-local mindstream-template-used nil
  "The template used (if any) in creating the current buffer.

This is a string representing a path to a file on disk.")

(defun mindstream--unique-name ()
  "Generate a unique name."
  (let ((time (current-time)))
    (concat (format-time-string "%F" time)
            "-"
            (sha1 (format "%s" time)))))

(defun mindstream-start-session (&optional template)
  "Start a new anonymous session.

This creates a new directory and Git repository for the new session.
It populates the empty buffer with the contents of TEMPLATE if one is
specified.  Otherwise, it uses the configured default template.

New sessions always start anonymous."
  (let* ((session (mindstream--unique-name))
         (base-path (mindstream--generate-anonymous-session-path session))
         (template (or template mindstream-default-template))
         (file-extension (file-name-extension template))
         (buf (mindstream--new-buffer-from-template template))
         (filename (mindstream--joindirs base-path
                                         (concat mindstream-filename "." file-extension))))
    (unless (file-directory-p base-path)
      (mkdir base-path t)
      (mindstream-backend-initialize base-path)
      (with-current-buffer buf
        ;; looks like writing an unsaved buffer automatically sets
        ;; the appropriate major mode based on the file extension
        ;; so there is no need for us to do that
        (write-file filename)
        (rename-buffer (mindstream-anonymous-buffer-name)))
      buf)))

(defun mindstream--iterate ()
  "Commit the current state as part of iteration."
  (mindstream-backend-iterate))

(defun mindstream--generate-anonymous-session-path (session)
  "A path on disk to use for a newly created SESSION."
  (mindstream--joindirs mindstream-path
                        session))

(defun mindstream--template (&optional name)
  "Path to template NAME.

If NAME isn't provided, use the default template."
  (mindstream--joindirs mindstream-template-path
                        (or name mindstream-default-template)))

(defun mindstream--ensure-templates-exist ()
  "Ensure that the templates directory exists and contains the default template."
  (unless (file-directory-p mindstream-template-path)
    (mkdir mindstream-template-path t))
  (let ((default-template-file (mindstream--template)))
    (unless (file-exists-p default-template-file)
      (let ((buf (generate-new-buffer "default-template")))
        (with-current-buffer buf
          (insert mindstream-default-template-contents)
          (write-file default-template-file))
        (kill-buffer buf)))))

(defun mindstream--file-contents (filename)
  "Get contents of FILENAME as a string."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun mindstream--initialize-buffer ()
  "Initialize a newly created buffer."
  (setq buffer-offer-save nil)
  ;; place point at the end of the buffer
  (goto-char (point-max)))

(defun mindstream--new-buffer-with-contents (contents)
  "Create a new scratch buffer containing CONTENTS.

This does not save the buffer.

As a \"scratch\" buffer, its contents will be treated as
disposable, and it will not prompt to save if it is closed or
if Emacs is exited."
  (let ((buf (generate-new-buffer
              ;; will be renamed later, after it is written to disk
              ;; so the name here doesn't matter
              (mindstream--unique-name))))
    (with-current-buffer buf
      (insert contents)
      (mindstream--initialize-buffer))
    buf))

(defun mindstream--new-buffer-from-template (template)
  "Create a new (unsaved) buffer from TEMPLATE."
  (let* ((contents (condition-case nil
                       (mindstream--file-contents template)
                     (error
                      (error
                       (format "Template %s not found! Please create it and try again."
                               template)))))
         (buf (mindstream--new-buffer-with-contents contents)))
    (with-current-buffer buf
      ;; store the template used as a buffer-local variable
      ;; on the scratch buffer
      ;; and also declare/document it so we know it's a fully
      ;; qualified path
      (setq mindstream-template-used template))
    buf))

(defun mindstream--find-template-for-mode (major-mode-to-use)
  "Find an appropriate template to use for a major mode.

Search the templates folder for a template recognizable to MAJOR-MODE-TO-USE."
  (catch 'return
    (dolist (assoc auto-mode-alist)
      (pcase-let ((`(,ext . ,mode) assoc))
        (when (eq mode major-mode-to-use)
          (let ((template (mindstream--file-with-extension ext
                                                           mindstream-template-path)))
            (when template
              (throw 'return template))))))
    (error
     (format "Template not found for %s! Please create one and try again."
             major-mode-to-use))))

(defun mindstream--infer-template (major-mode-to-use)
  "Infer template to use based on MAJOR-MODE-TO-USE."
  (let ((template (plist-get mindstream-preferred-template
                             major-mode-to-use)))
    (or template
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

If MAJOR-MODE-TO-USE is not provided, the major mode of the current buffer is used."
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
