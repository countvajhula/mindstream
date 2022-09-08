;;; mindstream.el --- A scratch buffer -*- lexical-binding: t -*-

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
(require 'mindstream-scratch)

;;;###autoload
(define-minor-mode mindstream-mode
  "Minor mode providing keybindings for mindstream mode."
  :lighter " mindstream"
  :global t
  :keymap
  (let ((mindstream-map (make-sparse-keymap)))
    (define-key mindstream-map (kbd "C-c C-r n") #'mindstream-new)
    (define-key mindstream-map (kbd "C-c C-r x") #'mindstream-switch-to-scratch-buffer)
    (define-key mindstream-map (kbd "C-c C-r h") #'mindstream-previous)
    (define-key mindstream-map (kbd "C-c C-r l") #'mindstream-next)
    (define-key mindstream-map (kbd "C-c C-r c") #'mindstream-clear)
    (define-key mindstream-map (kbd "C-c C-r s") #'mindstream-save-file)
    (define-key mindstream-map (kbd "C-c C-r S") #'mindstream-save-session)
    mindstream-map)
  (if mindstream-mode
      (mindstream-initialize)
    (mindstream-disable)))

;; TODO: restore navigation backwards and forwards
;; TODO: test "save file" still works
;; TODO: restore "save session" functionality (probably just
;;       git-clone it to another path without setting an upstream)
;; TODO: test that git-timemachine for read-only navigation
;;       and magit for all the usual stuff work out of the box
;; TODO: use first line of changeset as commit message?
;; TODO: start with linearized navigation of versions,
;;       i.e. modification of any prior state results
;;       in a fresh commit on a single branch.
;;       Later, support nonlinear navigation, i.e. modification
;;       creates a commit on a fresh branch beginning there
;; TODO: create a rigpa mode to navigate buffer states
;; TODO: switching branch selects the latest commit on the branch
;;       to avoid confusion
;; TODO: test everything, especially the various entry points
;;       from an ab initio state
;; TODO: support operation in any buffer, but also provide a
;;       (decoupled) scratch buffer framework
;;       - it may make sense to tag the initial state so that
;;         the session could be kept track of and e.g. squashed
;;         when progress has been made.
;;       - it could be even better to just start a branch when
;;         a mindstream session is started, instead
;; TODO: design feedback loops at scales other than module-level.
;;       e.g. natural transitions from expression-level (REPL),
;;       to module-level (scratch buffer) to application-level
;;       (define an execution loop that is decoupled from the
;;       current buffer)

(cl-defun mindstream--execute-shell-command (command &optional directory)
  "Execute COMMAND at DIRECTORY and return its output."
  (let ((default-directory (or directory
                               (file-name-directory (buffer-file-name)))))
    (shell-command-to-string command)))

(defun mindstream-commit ()
  "Commit the current state as part of iteration."
  (mindstream--execute-shell-command "git add -A && git commit -a --allow-empty-message -m ''"))

(cl-defun mindstream-write ()
  "Write scratch buffer to disk and increment the version.

This assumes that the scratch buffer is the current buffer, so
it should typically be run using `with-current-buffer`."
  (let* ((session mindstream-session-name)
         (base-path (mindstream--session-path session))
         (filename (concat base-path
                           mindstream-filename
                           mindstream-file-extension)))
    ;; except for the ab initio case, the file would exist
    (write-file filename)
    ;; writing the file changes the buffer name to the filename,
    ;; so we restore the original buffer name
    (rename-buffer mindstream-buffer-name)
    (mindstream-commit)))

(defun mindstream--buffer-index (buffer)
  "Get the index of the buffer in the current scratch session."
  (string-to-number
   (file-name-base
    (buffer-file-name buffer))))

(defun mindstream-new (template)
  "Start a new scratch buffer using a specific template.

This also begins a new session."
  (interactive (list (read-file-name "Which template? " mindstream-template-path)))
  (with-current-buffer (mindstream--get-scratch-buffer)
    ;; first write the existing scratch buffer
    ;; if there are unsaved changes
    (mindstream-write)
    ;; then kill it
    (kill-buffer))
  ;; start a new session
  (mindstream-start-session)
  ;; (ab initio) iterate
  (let ((buf (mindstream-iterate template)))
    (switch-to-buffer buf)))

;; Note that many of these interfaces assume that they are
;; invoked while visiting a scratch buffer.
(defun mindstream-clear ()
  "Start a new scratch buffer using a specific template."
  (interactive)
  ;; first write the existing scratch buffer
  ;; if there are unsaved changes
  (mindstream-write)
  (mindstream-iterate buffer-template))

(defun mindstream--ab-initio-iterate (&optional template)
  "Create a scratch buffer for the first time."
  (let* ((template (or template mindstream-default-template))
         ;; create the scratch buffer
         (buf (mindstream--new-buffer-from-template template)))
    ;; save and commit the initial state
    (with-current-buffer buf
      (mindstream-write))
    buf))

(cl-defun mindstream-iterate (&optional template)
  "Create a successor to the current scratch buffer.

This uses the index of the current scratch buffer incremented by one
as the index of the new file. If no scratch buffer exists, this starts
the index at 1.  If a TEMPLATE is provided, it will be used as the
contents of the new buffer, otherwise, the contents of the existing
scratch buffer will be copied over to the new file. If no scratch
buffer currently exists, then TEMPLATE is ignored."
  (let ((buffer (mindstream--get-scratch-buffer)))
    (if buffer
        ;; note: in this case, this assumes that this is called
        ;; while visiting a scratch buffer
        (progn (mindstream-write)
               buffer)
      (mindstream--ab-initio-iterate template))))

;; TODO: modify this to just git checkout the rev and proactively
;; refresh from disk
(defun mindstream--navigate (fn)
  "Go to an older or newer scratch buffer in the current session.

FN is expected to be a function that accepts an index and returns a new
index. Typically, FN will be either 1+ or 1-, to navigate forwards or
backwards in the scratch buffer history."
  (let* ((original-point (point))
         (file (buffer-file-name))
         (dir (file-name-directory file))
         (ext (file-name-extension file))
         (index (mindstream--buffer-index (current-buffer)))
         (next-file (concat dir (number-to-string (funcall fn index)) "." ext)))
    (when (file-exists-p next-file)
      (erase-buffer)
      (insert-file-contents next-file)
      (set-visited-file-name next-file)
      (set-buffer-modified-p nil)
      (rename-buffer mindstream-buffer-name)
      ;; TODO: avoid duplication of some of these vs mindstream--new-buffer-with-contents
      ;; (setq-local racket-repl-buffer-name "*scratch - Racket REPL*")
      (goto-char (if (> original-point (point-max))
                     (point-max)
                   original-point)))))

(defun mindstream-next ()
  "Go to a newer scratch buffer in the current session."
  (interactive)
  (mindstream--navigate #'1+))

(defun mindstream-previous ()
  "Go to an older scratch buffer in the current session."
  (interactive)
  (mindstream--navigate #'1-))

;;;###autoload
(defun mindstream-initialize ()
  "Advise any functions that should implicitly cause the scratch buffer to iterate."
  (advice-add #'racket-run :around #'mindstream-implicitly-iterate-advice))

(defun mindstream-disable ()
  "Remove any advice for racket scratch buffers."
  (advice-remove #'racket-run #'mindstream-implicitly-iterate-advice))

(defun mindstream-implicitly-iterate-advice (orig-fn &rest args)
  "Implicitly iterate the scratch buffer upon execution of some command.

This only iterates the buffer if it is the current buffer and has been
modified since the last persistent state. Otherwise, it takes no action."
  (when (and (mindstream-scratch-buffer-p) (buffer-modified-p))
    (mindstream-iterate))
  (let ((result (apply orig-fn args)))
    result))

(defun mindstream-save-file (filename)
  "Save the current scratch buffer to a file.

This is for interactive use only, for saving the file to a persistent
location of your choice. To just save the file to its existing (tmp)
location, use a low-level utility like `save-buffer` or `write-file`
directly."
  (interactive (list (read-file-name "Save file as: " mindstream-save-file-path "")))
  (write-file filename))

(defun mindstream-save-session (dir)
  "Save the current scratch session to a directory."
  (interactive (list (read-directory-name "Save session in: " mindstream-save-session-path)))
  ;; TODO: ideally, also be able to give it a name
  (copy-directory (mindstream--session-path mindstream-session-name)
                  dir))

(provide 'mindstream)
;;; mindstream.el ends here
