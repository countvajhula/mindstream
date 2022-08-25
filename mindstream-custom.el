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

(defgroup mindstream nil
  "A scratch buffer."
  :group 'Editing)

(defcustom mindstream-path "/var/tmp/racket/"
  "Directory path where mindstream buffers will be saved during development."
  :type 'string
  :group 'mindstream)

(defcustom mindstream-path "/var/tmp/racket/" ; TODO: make platform-independent?
  "Directory path where mindstream buffers will be saved during development."
  :type 'string
  :group 'mindstream)

(defcustom mindstream-template-path "~/.mindstream/templates/"  ; TODO: make platform-independent?
  "Directory path where mindstream will look for templates."
  :type 'string
  :group 'mindstream)

(defcustom mindstream-save-file-path user-home-directory
  "Default directory path for saving mindstream buffers."
  :type 'string
  :group 'mindstream)

(defcustom mindstream-save-session-path user-home-directory
  "Default directory path for saving mindstream sessions."
  :type 'string
  :group 'mindstream)

(defcustom mindstream-major-mode 'racket-mode
  "Major mode to use in mindstream buffers."
  :type 'string
  :group 'mindstream)

(defcustom mindstream-file-extension ".rkt"
  "File extension to use for mindstream buffers."
  :type 'string
  :group 'mindstream)

(defcustom mindstream-buffer-name "*scratch - Racket*"
  "The name of the mindstream scratch buffer."
  :type 'string
  :group 'mindstream)

(defcustom mindstream-default-template-name "racket.rkt"
  "Name for the default template that will be created when no templates exist."
  :type 'string
  :group 'mindstream)

(defcustom mindstream-default-template
  (concat (file-name-as-directory mindstream-template-path)
          mindstream-default-template-name)
  "Default template to use for new mindstream sessions."
  :type 'string
  :group 'mindstream)

(provide 'mindstream-custom)
;;; mindstream-custom.el ends here
