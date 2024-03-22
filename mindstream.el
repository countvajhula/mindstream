;;; mindstream.el --- A scratch buffer -*- lexical-binding: t -*-

;; Author: Siddhartha Kasivajhula <sid@countvajhula.com>
;; URL: https://github.com/countvajhula/mindstream
;; Version: 0.0
;; Package-Requires: ((emacs "25.1") (racket-mode "20210517.1613") (magit "3.3.0"))
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

(require 'magit-git)

(require 'mindstream-custom)
(require 'mindstream-scratch)
(require 'mindstream-util)

;; These are customization or config variables defined elsewhere;
;; explicitly declare them here to avoid byte compile warnings
;; TODO: handle this via an explicit configuration step
(declare-function racket-run "ext:racket-mode")

;;;###autoload
(define-minor-mode mindstream-mode
  "Minor mode providing keybindings for mindstream mode."
  :lighter " mindstream"
  :keymap
  (let ((mindstream-map (make-sparse-keymap)))
    (define-key mindstream-map (kbd "C-c C-r c") #'mindstream-clear)
    (define-key mindstream-map (kbd "C-c C-r s") #'mindstream-save-session)
    (define-key mindstream-map (kbd "C-c C-r C-s") #'mindstream-save-session)
    mindstream-map))

;;;###autoload
(define-minor-mode mindstream-global-mode
  "Minor mode providing keybindings for mindstream mode."
  :lighter " mindstream"
  :global t
  :keymap
  (let ((mindstream-global-map (make-sparse-keymap)))
    (define-key mindstream-global-map (kbd "C-c C-r n") #'mindstream-new)
    (define-key mindstream-global-map (kbd "C-c C-r r") #'mindstream-load-session)
    (define-key mindstream-global-map (kbd "C-c C-r b") #'mindstream-switch-to-scratch-buffer)
    mindstream-global-map))

(defun mindstream--iterate ()
  "Commit the current state as part of iteration."
  (mindstream--execute-shell-command
   (concat "git add -A"
           " && "
           "git commit -a --allow-empty-message -m ''")))

(defun mindstream--end-anonymous-session (&optional major-mode)
  "End the current anonymous session.

This always affects the current anonymous session and does not affect
a named session that you may happen to be visiting."
  (let ((buf (mindstream--get-anonymous-scratch-buffer major-mode)))
    (when buf
      (with-current-buffer buf
        ;; first write the existing scratch buffer
        ;; if there are unsaved changes
        (mindstream--iterate)
        ;; then kill it
        (kill-buffer)))))

(defun mindstream--new (template)
  "Start a new scratch buffer using a specific TEMPLATE.

This also begins a new session."
  ;; end the current anonymous session for the
  ;; desired major mode
  (mindstream--end-anonymous-session
   (mindstream--infer-major-mode template))
  ;; start a new session (sessions always start anonymous)
  (let ((buf (mindstream-start-session template)))
    ;; (ab initio) iterate
    (with-current-buffer buf
      (mindstream-mode 1)
      (mindstream--iterate))
    buf))

(defun mindstream-new (template)
  "Start a new scratch buffer using a specific TEMPLATE.

This also begins a new session."
  (interactive (list (read-file-name "Which template? " mindstream-template-path)))
  (let ((buf (mindstream--new template)))
    (switch-to-buffer buf)))

(defun mindstream-clear ()
  "Start a new scratch buffer using a specific template."
  (interactive)
  (unless mindstream-mode
    (error "Not a mindstream buffer!"))
  ;; first write the existing scratch buffer
  ;; if there are unsaved changes
  (save-buffer)
  (mindstream--iterate)
  ;; clear the buffer
  (erase-buffer)
  ;; if the buffer was originally created using a template,
  ;; then insert the template contents
  (when mindstream-template-used
    (insert (mindstream--file-contents mindstream-template-used)))
  ;; write the fresh state
  (mindstream--iterate))

;;;###autoload
(defun mindstream-initialize ()
  "Advise any functions that should implicitly cause the scratch buffer to iterate."
  (dolist (fn mindstream-triggers)
    (advice-add fn :around #'mindstream-implicitly-iterate-advice)))

(defun mindstream-disable ()
  "Remove any advice for racket scratch buffers."
  (dolist (fn mindstream-triggers)
    (advice-remove fn #'mindstream-implicitly-iterate-advice)))

(defun mindstream--call-live-trigger ()
  "Call configured live trigger for major mode."
  (when (and mindstream-mode (boundp 'mindstream-live-timer) mindstream-live-timer)
    ;; TODO: have a way to configure these for each major mode
    ;; and call the appropriate one from here
    ;; it's currently hardcoded
    (if (eq major-mode 'racket-mode)
        (racket-run)
      (save-buffer))))

(defun mindstream--start-live-timer ()
  "Start live timer."
  (let ((timer (run-at-time mindstream-live-delay nil #'mindstream--call-live-trigger)))
    (setq-local mindstream-live-timer timer)))

(defun mindstream--cancel-live-timer ()
  "Cancel live timer."
  (let ((timer (and (boundp 'mindstream-live-timer) mindstream-live-timer)))
    (when timer
      (cancel-timer timer))))

(defun mindstream--reset-live-timer (_beg _end _len)
  "Reset the live timer."
  (mindstream--cancel-live-timer)
  (mindstream--start-live-timer))

(defun mindstream-go-live ()
  "Live mode ... ENGAGE."
  (interactive)
  (add-hook 'after-change-functions
            #'mindstream--reset-live-timer
            t t))

(defun mindstream-go-offline ()
  "Disable live mode."
  (interactive)
  (remove-hook 'after-change-functions
               #'mindstream--reset-live-timer
               t))

(defun mindstream-implicitly-iterate-advice (orig-fn &rest args)
  "Implicitly iterate the scratch buffer upon execution of some command.

This only iterates the buffer if it is the current buffer and has been
modified since the last persistent state.  Otherwise, it takes no
action.

ORIG-FN is the original function invoked, and ARGS are the arguments
in that invocation."
  (let ((result (apply orig-fn args)))
    (when (and mindstream-mode
               (magit-anything-modified-p))
      (mindstream--iterate))
    result))

(defun mindstream--session-name ()
  "Name of the current session.

This is simply the name of the containing folder."
  (string-trim-left
   (directory-file-name
    (file-name-directory (buffer-file-name)))
   "^.*/"))

(defun mindstream-save-session (dest-dir)
  "Save the current scratch session to a directory.

If DEST-DIR is a non-existent path, it will be used as the name of a
new directory that will contain the session.  If it is an existing
path, then the session will be saved at that path using its current
(e.g. randomly generated) name as the name of the saved session folder.

It is advisable to use a descriptive name when saving a session, i.e.
you would typically want to specify a new, non-existent folder."
  (interactive (list (read-directory-name "Save session in: " mindstream-save-session-path)))
  (unless mindstream-mode
    (error "Not a mindstream buffer!"))
  (save-buffer) ; ensure it saves any WIP
  ;; The chosen name of the directory becomes the name of the session.
  (let* ((original-session-name (mindstream--session-name))
         (buffer-file (buffer-file-name))
         (filename (file-name-nondirectory buffer-file))
         (named (not (file-directory-p dest-dir))))
    ;; ensure no unsaved changes
    ;; note: this is a no-op if save-buffer is a trigger for iteration
    (mindstream--iterate)
    ;; TODO: verify behavior with existing vs non-existent containing folder
    (copy-directory (file-name-directory buffer-file)
                    dest-dir)
    (mindstream--end-anonymous-session)
    ;; TODO: platform-independent paths
    (if named
        (mindstream-load-session dest-dir)
      (mindstream-load-session (concat (file-name-as-directory dest-dir)
                                       original-session-name)))))

(defun mindstream--session-file-p (file)
  "Predicate to identify whether FILE is a Mindstream session file."
  (string-match-p
   (concat "^"
           mindstream-filename)
   file))

(defun mindstream-load-session (dir)
  "Load a session from a directory.

DIR is the directory containing the session."
  (interactive (list (read-directory-name "Load session: " mindstream-save-session-path)))
  ;; restore the old session
  (let ((filename (expand-file-name
                   (seq-find #'mindstream--session-file-p
				             (directory-files dir))
                   dir)))
    (find-file filename)
    (mindstream-mode 1)))

(defun mindstream--get-or-create-scratch-buffer ()
  "Get the active scratch buffer or create a new one.

If the scratch buffer doesn't exist, this creates a new one using
the default configured template.

This is a convenience utility for \"read only\" cases where we simply
want to get the scratch buffer - whatever it may be. It is too
connoted to be useful in features implementing the scratch buffer
iteration model."
  (or (mindstream--get-anonymous-scratch-buffer)
      (mindstream--new (mindstream--template
                        (mindstream--infer-template major-mode)))))

(defun mindstream-switch-to-scratch-buffer ()
  "Switch to the anonymous scratch buffer."
  (interactive)
  (let ((buf (mindstream--get-or-create-scratch-buffer)))
    (switch-to-buffer buf)))

(provide 'mindstream)
;;; mindstream.el ends here
