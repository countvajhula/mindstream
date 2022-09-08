;;; mindstream-scratch.el --- A scratch buffer -*- lexical-binding: t -*-

;; Author: Siddhartha Kasivajhula <sid@countvajhula.com>
;; URL: https://github.com/countvajhula/mindstream
;; Version: 0.0
;; Package-Requires: ((emacs "25.1") (racket-mode "20220705.1452"))
;; Keywords: lisp, convenience, languages

;; This program is "part of the world," in the sense described at
;; https://drym.org.  From your perspective, this is no different than
;; MIT or BSD or other such "liberal" licenses that you may be
;; familiar with, that is to say, you are free to do whatever you like
;; with this program.  It is much more than BSD or MIT, however, in
;; that it isn't a license at all but an idea about the world and how
;; economic systems could be set up so that everyone wins.  Learn more
;; at drym.org.
;;
;; This work transcends traditional legal and economic systems, but
;; for the purposes of any such systems within which you may need to
;; operate:
;;
;; This is free and unencumbered software released into the public domain.
;; The authors relinquish any copyright claims on this work.
;;

;;; Commentary:

;; A scratch buffer.

;;; Code:

(require 'mindstream-custom)

(defvar mindstream-session-name nil) ; TODO: rename to reflect "scratch" session

(defun mindstream--unique-session-name ()
  "Unique name for a scratch buffer session."
  (let ((time (current-time)))
    (concat (format-time-string "%Y-%m-%d" time)
            "-"
            (sha1 (format "%s" time)))))

(defun mindstream-start-session ()
  "Start a new session.

This updates the current session name and creates a new directory
and Git repository for the new session."
  (setq mindstream-session-name (mindstream--unique-session-name))
  (let* ((session mindstream-session-name)
         (base-path (mindstream--session-path session)))
    (unless (file-directory-p base-path)
      (mkdir base-path t)
      (mindstream--execute-shell-command "git init" base-path))))

(cl-defun mindstream--session-path (&optional session)
  "Path to the directory on disk containing SESSION."
  (let ((session (or session mindstream-session-name)))
    (concat (file-name-as-directory mindstream-path)
            (file-name-as-directory session))))

(cl-defun mindstream--session-max-index (&optional session)
  "Max (latest) index for the current session."
  (let* ((session (or session mindstream-session-name))
         (path (mindstream--session-path session))
         ;; note that non-numeric filenames are converted to
         ;; index 0 by string-to-number. That shouldn't be
         ;; relevant for our purposes since we're picking
         ;; the largest, in any case.
         (indices (seq-map
                   ;; filter to only numbers and then sort them
                   ;; as numbers
                   (lambda (filespec) (string-to-number (car filespec)))
                   (directory-files-and-attributes path))))
    (apply #'max indices)))

(defun mindstream--ensure-templates-exist ()
  "Ensure that the templates directory exists and contains the default template."
  ;; consider alternative: an initialization function to do this the first time
  (unless (file-directory-p mindstream-template-path)
    (mkdir mindstream-template-path t)
    (let ((buf (generate-new-buffer "default-template")))
      (with-current-buffer buf
        (insert "#lang racket\n\n")
        (write-file (concat mindstream-template-path
                            mindstream-default-template-name)))
      (kill-buffer buf))))

(defun mindstream--file-contents (filename)
  "Get contents of FILENAME as a string."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun mindstream--buffer-index (buffer)
  "Get the index of the buffer in the current scratch session."
  (string-to-number
   (file-name-base
    (buffer-file-name buffer))))

(defun mindstream--initialize-buffer ()
  "Initialize a newly created buffer.

This sets the session name and any other necessary attributes."
  (let* ((buffer-name mindstream-buffer-name)
         (major-mode-to-use mindstream-major-mode))
    (unless (eq major-mode major-mode-to-use)
      (funcall major-mode-to-use))
    (setq buffer-offer-save nil)
    (setq-local buffer-session mindstream-session-name)
    ;; Ignore whatever `racket-repl-buffer-name-function' just did to
    ;; set `racket-repl-buffer-name' and give this its own REPL.
    (setq-local racket-repl-buffer-name "*scratch - Racket REPL*")
    ;; place point at the end of the buffer
    (goto-char (point-max))))

(defun mindstream--new-buffer-with-contents (contents)
  "Create a new scratch buffer containing CONTENTS.

This does not save the buffer.

As a \"scratch\" buffer, its contents will be treated as
disposable, and it will not prompt to save if it is closed or
if Emacs is exited."
  (let* ((buffer-name mindstream-buffer-name)
         (buf (generate-new-buffer buffer-name))
         (major-mode-to-use mindstream-major-mode))
    (with-current-buffer buf
      (insert contents)
      (mindstream--initialize-buffer))
    buf))

(defun mindstream--new-buffer-from-template (template)
  "Create a new (unsaved) buffer from TEMPLATE."
  (mindstream--ensure-templates-exist)
  (let* ((contents (mindstream--file-contents template))
         (buf (mindstream--new-buffer-with-contents contents)))
    (with-current-buffer buf
      ;; store the template used as a buffer-local variable
      ;; on the scratch buffer
      (setq-local buffer-template template))
    buf))

(defun mindstream--get-scratch-buffer ()
  "Get the active scratch buffer, if it exists."
  (let ((buffer-name mindstream-buffer-name))
    (get-buffer buffer-name)))

(defun mindstream--get-or-create-scratch-buffer ()
  "Get the active scratch buffer or create a new one.

If the scratch buffer doesn't exist, this creates a new one using
the default configured template.

This is a convenience utility for \"read only\" cases where we simply want to
get the scratch buffer - whatever it may be. It is too connoted to be
useful in features implementing the scratch buffer iteration model."
  (or (mindstream--get-scratch-buffer)
      (mindstream-new mindstream-default-template)))

(defun mindstream-switch-to-scratch-buffer ()
  "Switch to scratch buffer."
  (interactive)
  (let ((buf (mindstream--get-or-create-scratch-buffer)))
    (switch-to-buffer buf)))

(defun mindstream-scratch-buffer-p ()
  "Predicate to check if the current buffer is the Scratch buffer."
  (equal mindstream-buffer-name (buffer-name)))

(provide 'mindstream-scratch)
;;; mindstream-scratch.el ends here
