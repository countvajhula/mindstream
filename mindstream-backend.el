;;; mindstream-backend.el --- A scratch buffer -*- lexical-binding: t -*-

;; Author: Siddhartha Kasivajhula <sid@countvajhula.com>
;; URL: https://github.com/countvajhula/mindstream
;; Version: 0.0
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

(defun mindstream--execute-shell-command (command &optional directory)
  "Execute COMMAND at DIRECTORY and return its output."
  (let ((default-directory (or directory
                               (file-name-directory (buffer-file-name)))))
    (shell-command-to-string command)))

(defun mindstream-backend-initialize ()
  "Initialize the backend."
  (mindstream--execute-shell-command "git init" base-path))

(defun mindstream-backend-iterate ()
  "Iterate using the backend (e.g. git)."
  (mindstream--execute-shell-command
   (concat "git add -A"
           " && "
           "git commit -a --allow-empty-message -m ''")))

(provide 'mindstream-backend)
;;; mindstream-backend.el ends here
