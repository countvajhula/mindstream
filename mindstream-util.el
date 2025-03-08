;;; mindstream-util.el --- Start writing, stay focused, don't worry -*- lexical-binding: t -*-

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

;; General utilities for Mindstream

;;; Code:

(defun mindstream--unique-name (&optional truncate-to)
  "Generate a unique name.

This is a SHA1 hash. Truncate the length to TRUNCATE-TO, if provided."
  (let ((name (sha1
               (format "%s" (current-time)))))
    (if truncate-to
        (seq-take name truncate-to)
      name)))

(defun mindstream--file-with-extension (extension path)
  "Return the first file with EXTENSION at PATH.

This searches PATH recursively."
  (let ((files (directory-files-recursively path extension)))
    (when files
      (car files))))

;; From: https://stackoverflow.com/a/13473856/323874
(defun mindstream--build-path (root &rest dirs)
  "Joins a series of directories together, like Python's os.path.join.

This concatenates the ROOT path with the sequence of DIRS in the
platform-appropriate way.

  (mindstream--build-path \"/tmp\" \"a\" \"b\" \"c\") => /tmp/a/b/c"

  (if (not dirs)
      root
    (apply #'mindstream--build-path
           (expand-file-name (car dirs) root)
           (cdr dirs))))

(defun mindstream--directory-files (dir &optional full)
  "List files in DIR that aren't hidden or special.

This includes subdirectories.

Return FULL, absolute paths, or relative paths."
  ;; TODO: exclude files that aren't versioned by Git
  (let ((files (seq-filter (lambda (x)
                             ;; e.g. .gitignore
                             (not (string-match-p "^\\." x)))
                           (directory-files dir
                                            nil))))
    (if full
        (seq-map (lambda (d)
                   (expand-file-name d dir))
                 files)
      files)))

(defun mindstream--directory-dirs (dir)
  "List subdirectories in DIR.

This returns absolute paths to the subdirectories."
  (seq-filter (lambda (file)
                (file-directory-p file))
              (mindstream--directory-files dir
                                           ;; always absolute for now
                                           t)))

(defun mindstream--directory-files-recursively (dir)
  "List files in DIR that aren't hidden or special."
  ;; TODO: exclude files that aren't versioned by Git
  (directory-files-recursively dir
                               "^[^\\.]"
                               nil
                               ;; Exclude hidden directories like .git
                               (lambda (f)
                                 (not (string-match-p "/\\." f)))))

(defun mindstream--directory-name (path)
  "The name of the directory at PATH."
  (file-name-nondirectory
   (directory-file-name
    (file-name-directory
     path))))

(defun mindstream--files-sort-by-modified-time (file-a file-b)
  "A binary predicate to use to sort files by date modified.

Sorts FILE-A and FILE-B."
  (let ((time-a (file-attribute-modification-time (file-attributes file-a)))
        (time-b (file-attribute-modification-time (file-attributes file-b))))
    (time-less-p time-b
                 time-a)))

(defun mindstream--files-sort-by-access-time (file-a file-b)
  "A binary predicate to use to sort files by date accessed.

Sorts FILE-A and FILE-B."
  (let ((time-a (file-attribute-access-time (file-attributes file-a)))
        (time-b (file-attribute-access-time (file-attributes file-b))))
    (time-less-p time-b
                 time-a)))

(defun mindstream--for-all-buffers (action)
  "Take ACTION for all open buffers.

This only operates on buffers that are visiting files, and not
non-file buffers, since all buffers of interest for our purposes
correspond to files on disk.

ACTION must take no arguments and should return nothing.  If a return
value is desired, then use a closure with a mutable lexical variable,
and mutate that variable in ACTION."
  (let ((blist (buffer-list)))
    (while blist
      (with-current-buffer (car blist)
        (when buffer-file-name
          (funcall action)))
      (setq blist (cdr blist)))))

(defun mindstream--find-buffer (predicate)
  "Find the first buffer that returns true for PREDICATE.

PREDICATE must take no arguments.  The matching buffer is returned, or
nil if there is no match."
  (let ((blist (buffer-list)))
    (catch 'break
      (while blist
        (with-current-buffer (car blist)
          (when (funcall predicate)
            (throw 'break (current-buffer))))
        (setq blist (cdr blist))))))

(defun mindstream--move-dir (from-dir to-dir)
  "Move folder FROM-DIR to TO-DIR.

If TO-DIR already exists, then move FROM-DIR inside it, otherwise
simply rename FROM-DIR to TO-DIR.

This also updates the visited file names of all open buffers visiting
a file in FROM-DIR to refer to TO-DIR."
  ;; Based on `dired-rename-file' and `dired-rename-subdir'
  (let* ((from-dir (file-name-as-directory
                    (expand-file-name
                     from-dir)))
         (to-dir (expand-file-name
                  to-dir))
         (from-pat from-dir)
         (to-pat (if (file-directory-p to-dir)
                     (concat (file-name-as-directory to-dir)
                             (file-name-as-directory
                              (mindstream--directory-name
                               from-dir)))
                   (file-name-as-directory to-dir))))
    (rename-file from-dir
                 (if (file-directory-p to-dir)
                     (file-name-as-directory to-dir)
                   to-dir)
                 nil)
    ;; Update visited file name of all affected buffers
    (mindstream--for-all-buffers
     (lambda ()
       (when (mindstream--path-in-tree-p buffer-file-name
                                         from-dir)
	     (let ((modflag (buffer-modified-p))
               (to-file (replace-regexp-in-string
                         (concat "^" (regexp-quote from-pat))
			             to-pat
			             buffer-file-name)))
	       (set-visited-file-name to-file)
	       (set-buffer-modified-p modflag)))))))

(defun mindstream--close-buffers-at-path (path)
  "Close all buffers in the PATH tree.

If any buffers have been modified, they will be saved first."
  (mindstream--for-all-buffers
   (lambda ()
     (when (mindstream--path-in-tree-p buffer-file-name
                                       path)
       (when (buffer-modified-p)
         (save-buffer))
       (kill-buffer)))))

(defun mindstream--path-in-tree-p (path dir)
  "Is PATH part of the directory tree starting at DIR?"
  ;; Source: `dired-in-this-tree-p'
  (let (case-fold-search
        (path (expand-file-name path))
        (dir (expand-file-name dir)))
    (string-match-p (concat "^" (regexp-quote dir)) path)))

;; from Emacs source code in org-compat.el
;; this is available natively in Emacs 28+
(defun mindstream--directory-empty-p (dir)
  "Is DIR empty?"
  (and (file-directory-p dir)
       (null (directory-files dir nil directory-files-no-dot-files-regexp t))))

(defun mindstream--get-containing-dir (file &optional full)
  "Get the name of the directory containing FILE.

FILE could be a file or a directory.  If FULL is nil, only the name of
the containing directory is returned, rather than its full path,
otherwise the full (absolute) path is returned."
  (let ((file (if (directory-name-p file)
                  (directory-file-name file)
                file)))
    (let ((result (directory-file-name
                   (file-name-directory
                    file))))
      (if full
          result
        (file-name-base result)))))

(defun mindstream--template-used (session)
  "Name of the template used in the SESSION.

SESSION is expected to be a full path to a session folder.

This can only be used in paths managed by Mindstream, that is,
anonymous and archive paths."
  (let ((session  ; add trailing slash for good measure
         (file-name-as-directory session)))
    ;; sessions are filed under template-name/date/session
    (mindstream--get-containing-dir
     (mindstream--get-containing-dir session
                                     t))))

(provide 'mindstream-util)
;;; mindstream-util.el ends here
