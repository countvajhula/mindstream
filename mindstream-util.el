;;; mindstream-util.el --- Scratch buffer sessions -*- lexical-binding: t -*-

;; Author: Siddhartha Kasivajhula <sid@countvajhula.com>
;; URL: https://github.com/countvajhula/mindstream
;; Version: 0.0
;; Keywords: convenience, files, languages, outlines, tools, vc, wp

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

;; General utilities for Mindstream

;;; Code:

(defun mindstream--file-with-extension (extension path)
  "Return the first file with EXTENSION at PATH."
  (let ((files (directory-files path nil extension)))
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

(provide 'mindstream-util)
;;; mindstream-util.el ends here
