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
(require 'dired)

(require 'mindstream-custom)
(require 'mindstream-session)
(require 'mindstream-backend)
(require 'mindstream-util)

(defvar mindstream-template-history nil
  "History of previously selected templates for use with `completing-read'.")

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
    (define-key mindstream-map (kbd "C-c , t") #'mindstream-enter-session-for-template)
    (define-key mindstream-map (kbd "C-c , m") #'mindstream-begin-session)
    (define-key mindstream-map (kbd "C-c , q") #'mindstream-end-session)
    (define-key mindstream-map (kbd "C-c , s") #'mindstream-save-session)
    (define-key mindstream-map (kbd "C-c , C-s") #'mindstream-save-session)
    (define-key mindstream-map (kbd "C-c , r") #'mindstream-load-session)
    (define-key mindstream-map (kbd "C-c , a") #'mindstream-archive)
    (define-key mindstream-map (kbd "C-c , o") #'mindstream-open)
    (define-key mindstream-map (kbd "C-c , C-l") #'mindstream-go-live)
    (define-key mindstream-map (kbd "C-c , C-o") #'mindstream-go-offline)

    mindstream-map)
  (if mindstream-mode
      (mindstream-initialize)
    (mindstream-disable)))

(defun mindstream--full-filename-to-alist (filename)
  "Return cons cell of \(template . FILENAME\).
FILENAME is assumed to be an absolute file name of a directory,
such that the last part of the name is mindstream template's name.

For example:
- FILENAME: /home/<user name>/.config/emacs/mindstream/templates/text
- template: text"
  (let ((template (car (last (split-string filename "/")))))
    (cons template filename)))

(defun mindstream--list-templates ()
  "List all templates by name."
  (mindstream--directory-files mindstream-template-path))

(defun mindstream--completing-read-template ()
  "Completion for template."
  (let ((templates (mindstream--list-templates)))
    (completing-read "Which template? " templates nil t nil
                     'mindstream-template-history)))

(defun mindstream--new (template)
  "Start a new anonymous session using a specific TEMPLATE.

This also begins a new session."
  ;; start a new session (sessions always start anonymous)
  (let ((buf (mindstream-start-anonymous-session template)))
    ;; (ab initio) iterate
    (with-current-buffer buf
      (mindstream--iterate))
    buf))

(defun mindstream-new (&optional template)
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
  (interactive)
  (let ((template (or template
                      (mindstream--completing-read-template))))
    (switch-to-buffer (mindstream--new template))))

(defun mindstream-start-anonymous-session (&optional template)
  "Start a new anonymous session.

This creates a new directory and Git repository for the new session
after copying over the contents of TEMPLATE if one is specified.
Otherwise, it uses the configured default template.

New sessions always start anonymous."
  (let* ((template-path (mindstream--template-path template))
         (filename (mindstream--starting-file-for-session template-path))
         (path (mindstream--generate-anonymous-session-path template)))
    (unless (file-directory-p path)
      (when mindstream-unique
        (mindstream-archive-template-sessions template))
      (copy-directory template-path path)
      (mindstream-backend-initialize path)
      (find-file
       (expand-file-name filename
                         path))
      (mindstream--initialize-buffer)
      (mindstream-begin-session)
      (current-buffer))))

(defun mindstream-initialize ()
  "Do any setup that's necessary for Mindstream.

This advises any functions that should implicitly cause the session to
iterate.  By default, this is just `basic-save-buffer', so that the
session is iterated every time the buffer is saved.  This is the
recommended usage, intended to capture \"natural\" points at which the
session is meaningful.

While it doesn't make sense to iterate the session if the buffer
has *not* been saved (there would be no changes to record a fresh
version for!), it's possible that you might want to iterate the
session at a coarser granularity than every save. In that case, you
can customize `mindstream-triggers' and add the function(s) that
should trigger session iteration (and remove `basic-save-buffer')."
  (mindstream--ensure-paths)
  (mindstream--ensure-templates-exist)
  (unless mindstream-persist
    ;; archive all sessions on startup
    (mindstream-archive-all))
  (dolist (fn mindstream-triggers)
    (advice-add fn :after #'mindstream-implicitly-iterate-advice)))

(defun mindstream-disable ()
  "Cleanup actions on exiting `mindstream-mode'.

This removes any advice (e.g. on `basic-save-buffer') that was added
for session iteration."
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
        ;; TODO: here and in other places in the code, might there
        ;; be any reason to use `basic-save-buffer' instead?
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

(defun mindstream-implicitly-iterate-advice (&rest _)
  "Implicitly iterate the session upon execution of some command.

This only iterates the session if there have been changes since
the last persistent state.  Otherwise, it takes no action."
  (when (mindstream-session-p)
    (mindstream--iterate)))

(defun mindstream-save-session (dest-dir)
  "Save the current session to a permanent location.

If DEST-DIR is a non-existent path, it will be used as the name of a
new directory that will contain the session.  If it is an existing
path, then the session will be saved at that path using its current
name as the name of the saved session folder.  Note that if you are
saving an anonymous session, its original name is a randomly generated
identifier.

It is advisable to use a descriptive name when saving a session, i.e.
if you are saving an anonymous session, you would typically want to
specify a new, non-existent folder. Otherwise, if you're simply saving
an existing saved session to a new location, then you likely just want
to select the existing destination path."
  (interactive
   (let ((base-save-path (mindstream--saved-path
                          (mindstream--template-used default-directory))))
     (unless (mindstream-native-session-p)
       ;; Only save buffers that are at a standard Mindstream path.
       ;; We do not expect that a user would want to "save" (and thereby move)
       ;; a repo that is already at a path that isn't maintained by Mindstream.
       (error "Not a mindstream buffer!"))
     (mindstream--ensure-path base-save-path)
     (list
      (read-directory-name "Save session in: "
                           base-save-path))))
  ;; ensure no unsaved changes
  (save-buffer)
  ;; note: this is a no-op if save-buffer is a trigger for iteration
  (mindstream--iterate)
  ;; The chosen name of the directory becomes the name of the session.
  (let ((source-dir (mindstream--session-dir (current-buffer)))
        (dest-dir (if (file-directory-p dest-dir)
                      ;; not named - retain anonymous session name
                      (mindstream--build-path dest-dir
                                              (mindstream--session-name))
                    dest-dir))
        (file-to-open (file-name-nondirectory
                       (buffer-file-name))))
    ;; TODO: verify behavior with existing vs non-existent containing folder
    (mindstream--move-dir source-dir dest-dir)
    ;; TODO: this is a no-op, and it currently leaves the
    ;; session in `mindsteam-active-sessions'. But that's OK
    ;; for now as it doesn't affect anything, and this "state"
    ;; will be removed eventually anyway in the design refactor
    ;; (mindstream--end-anonymous-session)
    (mindstream-load-session dest-dir file-to-open)))

(defun mindstream-load-session (dir &optional file)
  "Load a previously saved session.

DIR is the directory containing the session.  If FILE is specified, it
will be opened upon loading the session.  Otherwise, follow the default
protocol for selecting a file, including, if necessary, prompting for
the file to be opened."
  (interactive (list
                (mindstream--completing-read-session)))
  (let ((file (or file
                  (mindstream--starting-file-for-session dir))))
    (find-file (expand-file-name file dir))
    (mindstream-begin-session)))

(defun mindstream--completing-read-session ()
  "Return session-file via completion for template."
  ;; It's probably also good to delete sub-directories of
  ;; anon. There will be too many.
  (let* ((dirs-alist (mapcar
                      (lambda (d)
                        (cons d (mindstream--session-file-name-expand d)))
                      (seq-uniq
                       ;; deduplicate candidates
                       (seq-filter
                        ;; exclude anonymous sessions
                        (lambda (f)
                          (not (string-match-p (abbreviate-file-name
                                                (expand-file-name mindstream-path))
                                               f)))
                        (append mindstream-session-history
                                (apply #'append
                                       (seq-map #'mindstream--directory-dirs
                                                (mindstream--directory-dirs mindstream-save-session-path)))))
                       ;; trim any trailing slashes for comparison
                       (lambda (a b)
                         (equal (string-trim-right a "/")
                                (string-trim-right b "/"))))))
         (dir-key (completing-read "Which session? " dirs-alist nil t nil
                                   'mindstream-session-history))
         (dir (cdr (assoc-string dir-key dirs-alist))))
    ;; Return directory name
    (file-name-as-directory dir)))

(defun mindstream--session-file-name-expand (file)
  "Return fully expanded FILE name for `mindstream-session-history'."
  (if (file-name-absolute-p file)
      file  ; (expand-file-name file) - probably don't need to expand if absolute?
    (expand-file-name file mindstream-save-session-path)))

(defun mindstream--list-template-sessions (template)
  "List all active anonymous sessions for TEMPLATE.

TEMPLATE is expected to be a name rather than a path.  The sessions are
returned as absolute paths to the session directories."
  (let ((path (mindstream--anonymous-path template)))
    (when (file-directory-p path)
      (let ((date-dirs (mindstream--directory-dirs path))
            (result nil))
        (dolist (dir date-dirs)
          (setq result
                (append result
                        (mindstream--directory-dirs dir))))
        result))))

(defun mindstream--visit-anonymous-session (template)
  "Visit the most recent open anonymous session for TEMPLATE."
  (mindstream--find-buffer
   (lambda ()
     (and buffer-file-name
		  (mindstream--path-in-tree-p
           buffer-file-name
           (mindstream--anonymous-path template))))))

(defun mindstream--get-or-create-session (template)
  "Get an existing anonymous session buffer for TEMPLATE or create a new one.

If an anonymous buffer doesn't exist, this creates a new one using TEMPLATE.

This is a convenience utility for \"read only\" cases where we simply
want to get a session buffer for the current major mode, without
worrying about how that happens. It is too connoted to be useful in
features implementing the session iteration model."
  (or (mindstream--visit-anonymous-session template)
      (mindstream-open template)
      (mindstream--new template)))

(defun mindstream-enter-session-for-template (template)
  "Enter an anonymous session buffer for TEMPLATE.

This enters an existing anonymous session if one is present,
otherwise, it creates a new one and enters it."
  (interactive (list
                (mindstream--completing-read-template)))
  (let ((buf (mindstream--get-or-create-session template)))
    (switch-to-buffer buf)))

(defun mindstream-enter-anonymous-session ()
  "Enter a relevant anonymous session buffer.

This infers the template based on the current major mode and then
enters an existing anonymous session for that template if one is
present, otherwise, it creates a new one and enters it."
  (interactive)
  (mindstream-enter-session-for-template
   (mindstream--infer-template
    major-mode)))

(defun mindstream--list-anonymous-sessions ()
  "List anonymous session paths."
  (let ((sessions nil))
    (mindstream--for-all-buffers
     (lambda ()
       (when (mindstream-anonymous-session-p)
         (push (mindstream--session-dir (current-buffer))
               sessions))))
    (let ((sessions (sort (seq-uniq sessions)
                          #'files-sort-by-modified-time)))
      (when (mindstream-session-p)
        ;; ensure the session in the current buffer
        ;; is at the top of the completion menu
        (let ((this-session (mindstream--session-dir)))
          (setq sessions
                (cons this-session
                      (remove this-session
                              sessions)))))
      sessions)))

(defun mindstream-close-session (dir)
  "Close all open buffers at DIR."
  (mindstream--close-buffers-at-path dir))

(defun mindstream--archive (session-dir template)
  "Close all open buffers at SESSION-DIR and move it to the archive for TEMPLATE."
  ;; first close the session
  (mindstream-close-session session-dir)
  ;; then move it to its new location in the archive
  (let* ((base-path (mindstream--archive-path template))
         (date-dir (mindstream--get-containing-dir session-dir))
         (anon-date-dir (mindstream--get-containing-dir session-dir t))
         (to-dir (mindstream--build-path base-path
                                         date-dir)))
    (mindstream--ensure-path to-dir)
    (mindstream--move-dir session-dir
                          to-dir)
    (when (mindstream--directory-empty-p anon-date-dir)
      (delete-directory anon-date-dir))))

;; TODO: archive should be ordered by recency, so that the current session is highlighted.
(defun mindstream-archive (session)
  "Move the selected SESSION to `mindstream-archive-path'.

The session is expected to be anonymous - it does not make sense to
archive saved sessions."
  (interactive (list (completing-read "Which session? "
                                      (mindstream--list-anonymous-sessions)
                                      nil
                                      t
                                      nil
                                      'mindstream-session-history)))
  (let ((template (mindstream--template-used session)))
    (mindstream--archive session
                         template)))

(defun mindstream-archive-template-sessions (template)
  "Archive all sessions associated with TEMPLATE.

TEMPLATE is expected to be a simple name rather than a full path."
  (interactive (list
                (mindstream--completing-read-template)))
  (let ((sessions (mindstream--list-template-sessions template)))
    (when sessions
      (dolist (dir sessions)
        (mindstream--archive dir
                             template)))))

(defun mindstream-archive-all ()
  "Archive sessions for _all_ templates."
  (interactive)
  (dolist (template (mindstream--list-templates))
    (mindstream-archive-template-sessions template)))

(defun mindstream-open (template)
  "Open all active anonymous sessions for TEMPLATE."
  (interactive (list
                (mindstream--completing-read-template)))
  (let ((sessions (mindstream--list-template-sessions template)))
    (when sessions
      (dolist (dir sessions)
        (mindstream-load-session dir))
      ;; TODO: seems artificial to return this just so we
      ;; have a no-op buffer to switch to in "enter session"
      (current-buffer))))

(defun mindstream-open-all ()
  "Open anonymous sessions for _all_ templates."
  (interactive)
  (dolist (template (mindstream--list-templates))
    (mindstream-open template)))

(defun mindstream-close (template)
  "Close all open sessions for TEMPLATE."
  (interactive (list
                (mindstream--completing-read-template)))
  (let ((sessions (mindstream--list-template-sessions template)))
    (dolist (dir sessions)
      (mindstream-close-session dir))))

(defun mindstream-close-all ()
  "Close anonymous sessions for _all_ templates."
  (interactive)
  (dolist (template (mindstream--list-templates))
    (mindstream-close template)))

(provide 'mindstream)
;;; mindstream.el ends here
