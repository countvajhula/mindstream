;; defcustoms
(setq rackscratch-path "/var/tmp/racket/") ; TODO: make platform-independent
(setq rackscratch-template-path "~/.racket-mode/scratch/templates/") ; TODO: make platform-independent
(setq rackscratch-save-file-path user-home-directory)
(setq rackscratch-session-name nil)
(setq rackscratch-major-mode 'racket-mode)
(setq rackscratch-file-extension ".rkt")
(setq rackscratch-buffer-name "*scratch - Racket*")
(setq rackscratch-default-template-name "racket.rkt")
(setq rackscratch-default-template
      (concat (file-name-as-directory rackscratch-template-path)
              rackscratch-default-template-name))

(defun rackscratch--unique-session-name ()
  "Unique name for a scratch buffer session."
  (let ((time (current-time)))
    (concat (format-time-string "%Y-%m-%d" time)
            "-"
            (sha1 (format "%s" time)))))

(defun rackscratch-start-session ()
  "Start a new session.

This simply updates the current session name. The next time the scratch buffer
is iterated, it will create a new folder to hold the scratch buffer stages
for the new session."
  (setq rackscratch-session-name (rackscratch--unique-session-name)))

(cl-defun rackscratch-write (&optional index)
  "Write scratch buffer to disk with index INDEX.

This assumes that the scratch buffer is the current buffer, so
it should typically be run using `with-current-buffer`."
  (let* ((session (or rackscratch-session-name
                      (rackscratch--unique-session-name)))
         (base-path (concat (file-name-as-directory rackscratch-path)
                            (file-name-as-directory session)))
         (index (or index 1)))
    (unless (file-directory-p base-path)
      (mkdir base-path t))
    (let ((filename (concat base-path
                            (format "%d" index)
                            rackscratch-file-extension)))
      (if (file-exists-p filename)
          ;; we don't expect this to happen and it would be a bug if it did
          (error (format "Scratch file with requested index %d in session %s already exists!"
                         index
                         rackscratch-session-name))
          (write-file filename)))
    (rename-buffer rackscratch-buffer-name)
    ;; store the current session as a buffer-local variable
    ;; on the scratch buffer. This is used to reset the index
    ;; to 1 when a new session is started.
    (setq rackscratch-session-name session)))

(defun rackscratch--ensure-templates-exist ()
  "Ensure that the templates directory exists and contains the default template."
  ;; consider alternative: an initialization function to do this the first time
  (unless (file-directory-p rackscratch-template-path)
    (mkdir rackscratch-template-path t)
    (let ((buf (generate-new-buffer "default-template")))
      (with-current-buffer buf
        (insert "#lang racket\n\n")
        (write-file (concat rackscratch-template-path
                            rackscratch-default-template-name)))
      (kill-buffer buf))))

(defun rackscratch--file-contents (filename)
  "Get contents of FILENAME as a string."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun rackscratch--buffer-contents (buffer)
  "Get contents of BUFFER."
  (with-current-buffer buffer
    (buffer-string)))

(defun rackscratch--buffer-index (buffer)
  "Get the index of the buffer in the current scratch session."
  (string-to-number
   (file-name-base
    (buffer-file-name buffer))))

(defun rackscratch--buffer-session (buffer)
  "Get the session of the BUFFER."
  (with-current-buffer buffer
    buffer-session))

(defun rackscratch--new-buffer-with-contents (contents)
  "Create a new scratch buffer containing CONTENTS.

This does not save the buffer.

As a \"scratch\" buffer, its contents will be treated as
disposable, and it will not prompt to save if it is closed or
if Emacs is exited."
  (let* ((buffer-name rackscratch-buffer-name)
         (buf (generate-new-buffer buffer-name))
         (major-mode-to-use rackscratch-major-mode))
    (with-current-buffer buf
      (funcall major-mode-to-use)
      (setq buffer-offer-save nil)
      (setq-local buffer-session rackscratch-session-name)
      (insert contents)
      ;; place point at the end of the buffer
      (goto-char (point-max)))
    buf))

(defun rackscratch--new-buffer-from-template (template)
  "Create a new (unsaved) buffer from TEMPLATE."
  (rackscratch--ensure-templates-exist)
  (let* ((contents (rackscratch--file-contents template))
         (buf (rackscratch--new-buffer-with-contents contents)))
    (with-current-buffer buf
      ;; store the template used as a buffer-local variable
      ;; on the scratch buffer
      (setq-local buffer-template template))
    buf))

(defun rackscratch--new-buffer-copy (buffer)
  "Create a new (unsaved) buffer using the contents of BUFFER."
  (let* ((contents (rackscratch--buffer-contents buffer))
         (buf (rackscratch--new-buffer-with-contents contents))
         (template (with-current-buffer buffer
                     buffer-template)))
    (with-current-buffer buf
      ;; copy over the template used as a buffer-local variable
      ;; on the new scratch buffer
      (setq-local buffer-template template))
    buf))

(defun rackscratch--get-scratch-buffer ()
  "Get the active scratch buffer, if it exists."
  (let ((buffer-name rackscratch-buffer-name))
    (get-buffer buffer-name)))

(defun rackscratch--get-or-create-scratch-buffer ()
  "Get the active scratch buffer or create a new one.

If the scratch buffer doesn't exist, this creates a new one using
the default configured template.

This is a convenience utility for \"read only\" cases where we simply want to
get the scratch buffer - whatever it may be. It is too connoted to be
useful in features implementing the scratch buffer iteration model."
  (or (rackscratch--get-scratch-buffer)
      (rackscratch-new rackscratch-default-template)))

(defun rackscratch-switch-to-scratch-buffer ()
  "Switch to scratch buffer."
  (interactive)
  (let ((buf (rackscratch--get-or-create-scratch-buffer)))
    (switch-to-buffer buf)))

(defun rackscratch-new (template)
  "Start a new scratch buffer using a specific template.

This also begins a new session."
  (interactive (list (read-file-name "Which template? " rackscratch-template-path)))
  (rackscratch-start-session) ; start a new session
  (let ((buf (rackscratch-iterate template)))
    (switch-to-buffer buf)))

(defun rackscratch-clear ()
  "Start a new scratch buffer using a specific template."
  (interactive)
  (let ((buf (rackscratch-iterate buffer-template)))
    (switch-to-buffer buf)))

(defun rackscratch--ab-initio-iterate ()
  "Create a scratch buffer for the first time."
  (let ((buf (rackscratch--new-buffer-from-template rackscratch-default-template)))
    ;; now that it's created, save it
    (with-current-buffer buf
      (rackscratch-write))
    buf))

(cl-defun rackscratch-iterate (&optional template)
  "Create a successor to the current scratch buffer.

This uses the index of the current scratch buffer incremented by one
as the index of the new file. If no scratch buffer exists, this starts
the index at 1.  If a TEMPLATE is provided, it will be used as the
contents of the new buffer, otherwise, the contents of the existing
scratch buffer will be copied over to the new file. If no scratch
buffer currently exists, then TEMPLATE is ignored."
  (let ((buffer (rackscratch--get-scratch-buffer)))
    (if buffer
        (let ((index (rackscratch--buffer-index buffer))
              (session (rackscratch--buffer-session buffer))
              (buf (if template
                       (rackscratch--new-buffer-from-template template)
                     (rackscratch--new-buffer-copy buffer))))
          (with-current-buffer buffer
            ;; first save the existing scratch buffer
            (save-buffer)
            (kill-buffer))
          ;; save and return the new one
          (with-current-buffer buf
            (if (equal session rackscratch-session-name)
                ;; TODO: in these cases, it may be better to save the current
                ;; scratch buffer as the N+1 file and copy it to the N file.
                ;; That way, the point location and other state information
                ;; (e.g. the current evil state) would be preserved without
                ;; needing any bookkeeping. But that seems just a bit hacky.
                (rackscratch-write (1+ index))
              (rackscratch-write))) ; start numbering from 1 if new session
          buf)
      (rackscratch--ab-initio-iterate))))

(defun rackscratch--navigate (fn)
  "Go to an older or newer scratch buffer in the current session.

FN is expected to be a function that accepts an index and returns a new
index. Typically, FN will be either 1+ or 1-, to navigate forwards or
backwards in the scratch buffer history."
  (let* ((file (buffer-file-name))
         (dir (file-name-directory file))
         (ext (file-name-extension file))
         (index (rackscratch--buffer-index (current-buffer)))
         (next-file (concat dir (number-to-string (funcall fn index)) "." ext)))
    (when (file-exists-p next-file)
      (kill-buffer)
      (find-file next-file) ; also set REPL?
      (rename-buffer rackscratch-buffer-name))))

(defun rackscratch-next ()
  "Go to a newer scratch buffer in the current session."
  (interactive)
  (rackscratch--navigate #'1+))

(defun rackscratch-previous ()
  "Go to an older scratch buffer in the current session."
  (interactive)
  (rackscratch--navigate #'1-))

(defun rackscratch-initialize ()
  "Advise any functions that should implicitly cause the scratch buffer to iterate."
  (advice-add #'racket-run :around #'rackscratch-implicitly-iterate-advice))

(defun rackscratch-implicitly-iterate-advice (orig-fn &rest args)
  "Implicitly iterate the scratch buffer upon execution of some command."
  (let ((buffer-modified (buffer-modified-p)))
    (let ((result (apply orig-fn args)))
      (when buffer-modified
        (let ((buf (rackscratch-iterate)))
          (switch-to-buffer buf)))
      result)))

(defun rackscratch-save-file (filename)
  "Save the current scratch buffer to a file."
  (interactive (list (read-file-name "Save file as: " rackscratch-save-file-path "")))
  (write-file filename))
