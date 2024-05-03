;;; mindstream-custom.el --- Start writing, stay focused, don't worry -*- lexical-binding: t -*-

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
;;
;; User customizations for Mindstream
;;

;;; Code:

(require 'mindstream-util)

(defvar mindstream--user-home-directory (getenv "HOME"))

(defgroup mindstream nil
  "A versioned freewriting session."
  :group 'Editing)

(defcustom mindstream-path
  ;; platform-independent ~/.emacs.d/mindstream/anon
  (mindstream--joindirs user-emacs-directory
                        "mindstream"
                        "anon")
  "Directory where anonymous mindstream sessions will be stored."
  :type 'string
  :group 'mindstream)

(defcustom mindstream-template-path
  ;; platform-independent ~/.emacs.d/mindstream/templates
  (mindstream--joindirs user-emacs-directory
                        "mindstream"
                        "templates")
  "Directory path where mindstream will look for templates."
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

(defcustom mindstream-live-delay 1.5
  "Delay in typing after which the session is iterated."
  :type 'list
  :group 'mindstream)

(defcustom mindstream-live-action nil
  "Periodic action to take while in \"live mode\"."
  :type '(plist :key-type symbol
                :value-type function)
  :group 'mindstream)

(defcustom mindstream-starting-file nil
  "The file to start the session, for each template.

If no file is specified for a template, defaults to `mindstream-file'."
  :type '(plist :key-type string
                :value-type string)
  :group 'mindstream)

(defcustom mindstream-preferred-template nil
  "The preferred template for each major mode.

In cases where you don't indicate a template (e.g.
`mindstream-enter-anonymous-session'), we search the templates folder for a
template that has an extension recognizable to the major mode, and use
the first one we find.  But if you have many templates that share the
same extension, you may prefer to indicate which one is \"preferred\"
for the major mode so that it would be selected."
  :type '(plist :key-type symbol
                :value-type function)
  :group 'mindstream)

(defcustom mindstream-filename "scratch"
  "Filename to use for mindstream buffers."
  :type 'string
  :group 'mindstream)

(defcustom mindstream-anonymous-buffer-prefix "scratch"
  "The prefix to use in the name of a mindstream scratch buffer."
  :type 'string
  :group 'mindstream)

(defcustom mindstream-add-everything t
  "Whether to add all files to the index before commiting on each iteration."
  :type 'boolean
  :group 'mindstream)

(defcustom mindstream-default-template "text"
  "Default template to use for new mindstream sessions.

If no templates exist, this one will be created with the default
template contents."
  :type 'string
  :group 'mindstream)

(defcustom mindstream-default-template-contents "The past is a memory, the future a dream. Now, we write.\n"
  "Contents of the default template that is created if none exist."
  :type 'string
  :group 'mindstream)

(provide 'mindstream-custom)
;;; mindstream-custom.el ends here
