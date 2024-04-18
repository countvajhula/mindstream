;;; mindstream-backend.el --- Scratch buffer sessions -*- lexical-binding: t -*-

;; Author: Siddhartha Kasivajhula <sid@countvajhula.com>
;; URL: https://github.com/countvajhula/mindstream
;; Version: 0.0
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

;; Backend (e.g. Git) abstraction for Mindstream

;;; Code:

(defun mindstream--execute-shell-command (command &optional directory)
  "Execute shell COMMAND at DIRECTORY and return its output."
  (let ((default-directory (or directory
                               (file-name-directory (buffer-file-name)))))
    (shell-command-to-string command)))

(defun mindstream-backend-initialize (base-path)
  "Initialize the backend at the path BASE-PATH."
  (mindstream--execute-shell-command "git init" base-path))

(defun mindstream-backend-iterate ()
  "Iterate using the backend (e.g. git)."
  (mindstream--execute-shell-command
   (concat "git add -A"
           " && "
           "git commit -a --allow-empty-message -m ''")))

(provide 'mindstream-backend)
;;; mindstream-backend.el ends here
