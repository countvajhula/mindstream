;; defcustoms
(setq rackscratch-path "/var/tmp/racket/")
(setq rackscratch-session-name nil)
(setq rackscratch-major-mode 'racket-mode)
(setq rackscratch-file-extension ".rkt")

(defun rackscratch--unique-session-name ()
  "Unique name for a scratch buffer session."
  (let ((time (current-time)))
    (concat (format-time-string "%Y-%m-%d" time)
            "-"
            (sha1 (format "%s" time)))))

(defun rackscratch-write (index)
  "Write scratch buffer to disk with index INDEX."
  (let* ((session (or rackscratch-session-name
                      (rackscratch--unique-session-name)))
         (base-path (concat (file-name-as-directory rackscratch-path)
                            (file-name-as-directory session)))
         (index (or index 1)))
    (unless (file-directory-p base-path)
      (mkdir base-path t))
    (write-file
     (concat base-path
             (format "%d" index)
             rackscratch-file-extension))))

(cl-defun rackscratch--new-scratch-buffer (buffer-name &optional index)
  "Create a new empty buffer.

The buffer will be named BUFFER-NAME and will be created in the
currently active (at the time of command execution) major mode.
As a \"scratch\" buffer, its contents will be treated as
disposable, and it will not prompt to save if it is closed or
if Emacs is exited.

Modified from:
URL `https://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
  (interactive)
  (let ((buf (generate-new-buffer buffer-name))
        (major-mode-to-use rackscratch-major-mode))
    (with-current-buffer buf
      (funcall major-mode-to-use)
      (setq buffer-offer-save nil)
      (rackscratch-write index)
      (rename-buffer buffer-name))
    buf))

(defun rackscratch-get-or-create-scratch-buffer ()
  "Get the active scratch buffer or create a new one."
  (let* ((buffer-name "*scratch - Racket*")
         (buf (get-buffer buffer-name)))
    (or buf (rackscratch--new-scratch-buffer buffer-name))))

(defun rackscratch-switch-to-scratch-buffer ()
  "Switch to scratch buffer."
  (interactive)
  (let ((buf (rackscratch-get-or-create-scratch-buffer)))
    (switch-to-buffer buf)))
