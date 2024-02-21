;;; mindstream-custom.el --- A scratch buffer -*- lexical-binding: t -*-

;; URL: https://github.com/countvajhula/mindstream

;; This program is "part of the world," in the sense described at
;; http://drym.org.  From your perspective, this is no different than
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
;;
;; User customizations
;;

;;; Code:

(defvar mindstream--user-home-directory (getenv "HOME"))

(defgroup mindstream nil
  "A scratch buffer."
  :group 'Editing)

(defcustom mindstream-path "/var/tmp/mindstream/" ; TODO: make platform-independent?
  "Directory path where mindstream buffers will be saved during development."
  :type 'string
  :group 'mindstream)

(defcustom mindstream-template-path "~/.mindstream/templates/"  ; TODO: make platform-independent?
  "Directory path where mindstream will look for templates."
  :type 'string
  :group 'mindstream)

(defcustom mindstream-save-file-path mindstream--user-home-directory
  "Default directory path for saving mindstream buffers."
  :type 'string
  :group 'mindstream)

(defcustom mindstream-save-session-path mindstream--user-home-directory
  "Default directory path for saving mindstream sessions."
  :type 'string
  :group 'mindstream)

(defcustom mindstream-triggers (list #'save-buffer)
  "Functions that, when called, should implicitly iterate the mindstream buffer."
  :type 'list
  :group 'mindstream)

(defcustom mindstream-filename "scratch"
  "Filename to use for mindstream buffers."
  :type 'string
  :group 'mindstream)

(defcustom mindstream-anonymous-buffer-prefix "scratch"
  "The prefix to use in the name of a mindstream scratch buffer."
  :type 'string
  :group 'mindstream)

(defcustom mindstream-default-template "text.txt"
  "Default template to use for new mindstream sessions.

If no templates exist, this one will be created with the default template contents."
  :type 'string
  :group 'mindstream)

(defcustom mindstream-default-template-contents "Welcome to Mindstream!\n\nOnce you're familiar with the basic functionality, you may want to modify this template to suit your needs, or add other templates of your own.\n\n"
  "Contents of the default template that is created if none exist."
  :type 'string
  :group 'mindstream)

(provide 'mindstream-custom)
;;; mindstream-custom.el ends here
