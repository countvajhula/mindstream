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

(defun mindstream--file-with-extension (extension path)
  "Return the first file with EXTENSION at PATH.

This searches PATH recursively."
  (let ((files (directory-files-recursively path extension)))
    (when files
      (car files))))

(defun mindstream--major-mode-for-file-extension (extension)
  "Appropriate major mode for the given file EXTENSION.

This consults Emacs's `auto-mode-alist'."
  (catch 'return
    (dolist (assoc auto-mode-alist)
      (pcase-let ((`(,ext . ,mode) assoc))
        (when (string-match-p ext extension)
          (throw 'return mode))))))

;; From: https://stackoverflow.com/a/13473856/323874
(defun mindstream--joindirs (root &rest dirs)
  "Joins a series of directories together, like Python's os.path.join.

This concatenates the ROOT path with the sequence of DIRS in the
platform-appropriate way.

  (mindstream--joindirs \"/tmp\" \"a\" \"b\" \"c\") => /tmp/a/b/c"

  (if (not dirs)
      root
    (apply #'mindstream--joindirs
           (expand-file-name (car dirs) root)
           (cdr dirs))))

(defun mindstream--directory-files (dir)
  "List files in DIR that aren't hidden or special."
  ;; TODO: exclude files that aren't versioned by Git
  (seq-filter (lambda (x)
                ;; e.g. .gitignore
                (not (string-match-p "^\\." x)))
              (directory-files dir
                               nil)))

(defun mindstream--directory-name (path)
  "The name of the directory at PATH."
  (file-name-nondirectory
   (directory-file-name
    (file-name-directory
     path))))

(provide 'mindstream-util)
;;; mindstream-util.el ends here
