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

(setq mindstream-session-name nil)

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

(defun mindstream--unique-session-name ()
  "Unique name for a scratch buffer session."
  (let ((time (current-time)))
    (concat (format-time-string "%Y-%m-%d" time)
            "-"
            (sha1 (format "%s" time)))))

(defun mindstream-start-session ()
  "Start a new session.

This simply updates the current session name. The next time the scratch buffer
is iterated, it will create a new folder to hold the scratch buffer stages
for the new session."
  (setq mindstream-session-name (mindstream--unique-session-name)))

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

(cl-defun mindstream-write (&optional index)
  "Write scratch buffer to disk with index INDEX.

This assumes that the scratch buffer is the current buffer, so
it should typically be run using `with-current-buffer`."
  (let* ((session (or mindstream-session-name
                      (mindstream--unique-session-name)))
         (base-path (mindstream--session-path session))
         (index (or index 1)))
    (unless (file-directory-p base-path)
      (mkdir base-path t))
    (let ((filename (concat base-path
                            (format "%d" index)
                            mindstream-file-extension)))
      (if (file-exists-p filename)
          ;; we don't expect this to happen and it would be a bug if it did
          (error (format "Scratch file with requested index %d in session %s already exists!"
                         index
                         mindstream-session-name))
        (write-file filename)))
    (rename-buffer mindstream-buffer-name)
    ;; store the current session as a buffer-local variable
    ;; on the scratch buffer. This is used to reset the index
    ;; to 1 when a new session is started.
    (setq mindstream-session-name session)))

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

(defun mindstream--buffer-contents (buffer)
  "Get contents of BUFFER."
  (with-current-buffer buffer
    (buffer-string)))

(defun mindstream--buffer-index (buffer)
  "Get the index of the buffer in the current scratch session."
  (string-to-number
   (file-name-base
    (buffer-file-name buffer))))

(defun mindstream--buffer-session (buffer)
  "Get the session of the BUFFER."
  (with-current-buffer buffer
    buffer-session))

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
      (funcall major-mode-to-use)
      (setq buffer-offer-save nil)
      (setq-local buffer-session mindstream-session-name)
      ;; Ignore whatever `racket-repl-buffer-name-function' just did to
      ;; set `racket-repl-buffer-name' and give this its own REPL.
      (setq-local racket-repl-buffer-name "*scratch - Racket REPL*")
      (insert contents)
      ;; place point at the end of the buffer
      (goto-char (point-max)))
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

(defun mindstream--new-buffer-copy (buffer)
  "Create a new (unsaved) buffer using the contents of BUFFER."
  (let* ((contents (mindstream--buffer-contents buffer))
         (buf (mindstream--new-buffer-with-contents contents))
         (template (with-current-buffer buffer
                     buffer-template)))
    (with-current-buffer buf
      ;; copy over the template used as a buffer-local variable
      ;; on the new scratch buffer
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

(defun mindstream-new (template)
  "Start a new scratch buffer using a specific template.

This also begins a new session."
  (interactive (list (read-file-name "Which template? " mindstream-template-path)))
  (mindstream-start-session) ; start a new session
  (let ((buf (mindstream-iterate template)))
    (switch-to-buffer buf)))

(defun mindstream-clear ()
  "Start a new scratch buffer using a specific template."
  (interactive)
  (let ((buf (mindstream-iterate buffer-template)))
    (switch-to-buffer buf)))

(defun mindstream--ab-initio-iterate ()
  "Create a scratch buffer for the first time."
  (let ((buf (mindstream--new-buffer-from-template mindstream-default-template)))
    ;; now that it's created, save it
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
        (let* ((session (mindstream--buffer-session buffer))
               (index (mindstream--session-max-index session)))
          (if template
              (let ((buf (mindstream--new-buffer-from-template template)))
                (with-current-buffer buffer
                  ;; first save the existing scratch buffer
                  (save-buffer)
                  (kill-buffer))
                ;; save and return the new one
                (with-current-buffer buf
                  (if (equal session mindstream-session-name)
                      (mindstream-write (1+ index))
                    (mindstream-write))) ; start numbering from 1 if new session
                buf)
            (with-current-buffer buffer
              ;; save with the new filename, so that any mutations to
              ;; the scratch buffer always get appended to the end
              ;; as a fresh state, instead of being treated as mutations
              ;; of an existing state.
              (if (equal session mindstream-session-name)
                  (mindstream-write (1+ index))
                (mindstream-write))) ; start numbering from 1 if new session
            buffer))
      (mindstream--ab-initio-iterate))))

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

(defun mindstream-scratch-buffer-p ()
  "Predicate to check if the current buffer is the Scratch buffer."
  (equal mindstream-buffer-name (buffer-name)))

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
