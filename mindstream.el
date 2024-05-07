;;; mindstream.el --- Start writing, stay focused, don't worry -*- lexical-binding: t -*-

;; Author: Siddhartha Kasivajhula <sid@countvajhula.com>
;; URL: https://github.com/countvajhula/mindstream
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (magit "3.3.0"))
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

;; Start writing, stay focused, don't worry.

;;; Code:

(require 'magit-git)

(require 'mindstream-custom)
(require 'mindstream-session)
(require 'mindstream-util)

(defvar-local mindstream-live-timer nil
  "A timer used to execute a periodic action in \"live mode\".

This is stored as a local variable in the session buffer so that it
can be retrieved and canceled when you leave live mode.")

;;;###autoload
(define-minor-mode mindstream-mode
  "Minor mode providing global keybindings for mindstream mode."
  :lighter " mindstream"
  :global t
  :group 'mindstream
  :keymap
  (let ((mindstream-map (make-sparse-keymap)))
    (define-key mindstream-map (kbd "C-c , n") #'mindstream-new)
    (define-key mindstream-map (kbd "C-c , b") #'mindstream-enter-anonymous-session)
    (define-key mindstream-map (kbd "C-c , m") #'mindstream-begin-session)
    (define-key mindstream-map (kbd "C-c , q") #'mindstream-end-session)
    (define-key mindstream-map (kbd "C-c , s") #'mindstream-save-session)
    (define-key mindstream-map (kbd "C-c , C-s") #'mindstream-save-session)
    (define-key mindstream-map (kbd "C-c , r") #'mindstream-load-session)
    (define-key mindstream-map (kbd "C-c , C-l") #'mindstream-go-live)
    (define-key mindstream-map (kbd "C-c , C-o") #'mindstream-go-offline)

    mindstream-map)
  (if mindstream-mode
      (mindstream-initialize)
    (mindstream-disable)))

(defun mindstream--end-anonymous-session (&optional major-mode-to-use)
  "End the current anonymous session.

This ends the current anonymous session for MAJOR-MODE-TO-USE and does not
affect a named session that you may happen to be visiting."
  ;; TODO: may want to also kill any other open buffers at the same base path
  (let ((buf (mindstream--get-anonymous-session-buffer major-mode-to-use)))
    (when buf
      (with-current-buffer buf
        ;; first write the existing scratch buffer
        ;; if there are unsaved changes
        (mindstream--iterate)
        ;; end the anonymous session
        (mindstream-end-session)
        ;; then kill it
        (kill-buffer)))))

(defun mindstream--infer-major-mode-for-template (template)
  "Infer the starting major mode for the TEMPLATE."
  (let ((session-file
         (mindstream--session-file-for-template template)))
    (mindstream--infer-major-mode session-file)))

(defun mindstream--new (template)
  "Start a new anonymous session using a specific TEMPLATE.

This also begins a new session."
  ;; end the current anonymous session for the
  ;; desired major mode
  (mindstream--end-anonymous-session
   (mindstream--infer-major-mode-for-template template))
  ;; start a new session (sessions always start anonymous)
  (let ((buf (mindstream-start-anonymous-session template)))
    ;; (ab initio) iterate
    (with-current-buffer buf
      (mindstream--iterate))
    buf))

(defun mindstream-new (template)
  "Start a new anonymous session.

This creates a new directory using the specified TEMPLATE, and begins
a new session that records a new version every time you save the
buffer.  The session is anonymous so you don't have to name it up
front, and if you decide you want to keep it around, you can save the
session at any time and give it a name then.

Even though you don't name the session when you begin, it is
still saved on disk from the beginning, with a randomly-generated
name, in a dedicated Git version-controlled folder at
`mindstream-path', which you can customize."
  (interactive (list (read-directory-name "Which template? "
                                          mindstream-template-path
                                          nil
                                          t
                                          "")))
  (let ((buf (mindstream--new template)))
    (switch-to-buffer buf)))

(defun mindstream-initialize ()
  "Do any setup that's necessary for Mindstream.

This advises any functions that should implicitly cause the session to
iterate.  By default, this is just `save-buffer', so that the session
is iterated every time the buffer is saved.  This is the recommended
usage, intended to capture \"natural\" points at which the session is
meaningful.

While it doesn't make sense to iterate the session if the buffer
has *not* been saved (there would be no changes to record a fresh
version for!), it's possible that you might want to iterate the
session at a coarser granularity than every save. In that case, you
can customize `mindstream-triggers' and add the function(s) that
should trigger session iteration (and remove `save-buffer')."
  (mindstream--ensure-templates-exist)
  (dolist (fn mindstream-triggers)
    (advice-add fn :around #'mindstream-implicitly-iterate-advice)))

(defun mindstream-disable ()
  "Cleanup actions on exiting `mindstream-mode'.

This removes any advice (e.g. on `save-buffer') that was added for
session iteration."
  (dolist (fn mindstream-triggers)
    (advice-remove fn #'mindstream-implicitly-iterate-advice)))

(defun mindstream--call-live-action ()
  "Call configured live action for major mode."
  (when (and (mindstream-session-p)
             (boundp 'mindstream-live-timer)
             mindstream-live-timer)
    (let ((action (plist-get mindstream-live-action
                             major-mode)))
      (if action
          (funcall action)
        (save-buffer)))))

(defun mindstream--start-live-timer ()
  "Start live timer."
  (let ((timer (run-at-time mindstream-live-delay nil #'mindstream--call-live-action)))
    (setq-local mindstream-live-timer timer)))

(defun mindstream--cancel-live-timer ()
  "Cancel live timer."
  (let ((timer (and (boundp 'mindstream-live-timer) mindstream-live-timer)))
    (when timer
      (cancel-timer timer))))

(defun mindstream--reset-live-timer (_beg _end _len)
  "Reset the live timer."
  (when (mindstream-session-p)
    (mindstream--cancel-live-timer)
    (mindstream--start-live-timer)))

(defun mindstream-go-live ()
  "Live mode ... ENGAGE.

This invokes an action you indicate every time there is a pause in
typing.  Typically, you might use this in programming settings to
\"run\" the buffer and generate its output, or some other such action
to give you quick feedback on the results of your changes.

The action is customized via `mindstream-live-action', and the delay
before invoking it is customized via `mindstream-live-delay'."
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
  "Implicitly iterate the session upon execution of some command.

This only iterates the session if there have been changes since
the last persistent state.  Otherwise, it takes no action.

ORIG-FN is the original function invoked, and ARGS are the arguments
in that invocation."
  (let ((result (apply orig-fn args)))
    (when (mindstream-session-p)
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
  "Save the current session to a permanent location.

If DEST-DIR is a non-existent path, it will be used as the name of a
new directory that will contain the session.  If it is an existing
path, then the session will be saved at that path using its current
name as the name of the saved session folder.  Note that if you are
saving an anonymous session, its original name is a randomly generated
identifier.

It is advisable to use a descriptive name when saving a session, i.e.
you would typically want to specify a new, non-existent folder."
  (interactive (list (read-directory-name "Save session in: " mindstream-save-session-path)))
  (unless (mindstream-session-p)
    (error "Not a mindstream buffer!"))
  (save-buffer) ; ensure it saves any WIP
  ;; The chosen name of the directory becomes the name of the session.
  (let* ((original-session-name (mindstream--session-name))
         (file (file-name-nondirectory (buffer-file-name)))
         (dir (file-name-directory (buffer-file-name)))
         (named (not (file-directory-p dest-dir))))
    ;; ensure no unsaved changes
    ;; note: this is a no-op if save-buffer is a trigger for iteration
    (mindstream--iterate)
    ;; TODO: verify behavior with existing vs non-existent containing folder
    (copy-directory dir dest-dir)
    (mindstream--end-anonymous-session)
    (if named
        (mindstream-load-session dest-dir file)
      (mindstream-load-session
       (mindstream--joindirs dest-dir
                             original-session-name)
       file))))

(defun mindstream-load-session (dir &optional file)
  "Load a previously saved session.

DIR is the directory containing the session.  If FILE is specified, it
will be opened upon loading the session.  Otherwise, follow the default
protocol for selecting a file, including, if necessary, prompting for
the file to be opened."
  (interactive (list (read-directory-name "Load session: "
                                          mindstream-save-session-path
                                          nil
                                          t
                                          "")))
  (let ((file (if file
                  (expand-file-name file dir)
                (let ((files (mindstream--directory-files dir)))
                  (if (and files (= 1 (length files)))
                      (expand-file-name (car files)
                                        dir)
                    (read-file-name "Which file? "
                                    dir
                                    nil
                                    t
                                    ""))))))
    (find-file file)
    (mindstream-begin-session)))

(defun mindstream--get-or-create-session ()
  "Get the anonymous session buffer or create a new one.

If an anonymous buffer doesn't exist, this creates a new one using the
default configured template.

This is a convenience utility for \"read only\" cases where we simply
want to get a session buffer for the current major mode, without
worrying about how that happens. It is too connoted to be useful in
features implementing the session iteration model."
  (or (mindstream--get-anonymous-session-buffer)
      (mindstream--new (mindstream--template
                        (mindstream--infer-template major-mode)))))

(defun mindstream-enter-anonymous-session ()
  "Enter an anonymous session buffer.

This enters an existing anonymous session if one is present,
otherwise, it creates a new one and enters it."
  (interactive)
  (let ((buf (mindstream--get-or-create-session)))
    (switch-to-buffer buf)))

(provide 'mindstream)
;;; mindstream.el ends here
