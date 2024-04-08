.. image:: https://github.com/countvajhula/mindstream/actions/workflows/test.yml/badge.svg
    :target: https://github.com/countvajhula/mindstream/actions

mindstream
==========

If you've ever created throwaway files named ``1.txt`` or ``blah.py`` or ``test3.rkt`` to quickly write down some thoughts, try out a new idea, or prototype a new software tool, then you've already experienced the inconvenience of having to first come up with a name for these throwaway files, a small obstruction that is sometimes enough to snuff out that momentary creative spark that could have become a worthy conflagration. You've also probably experienced the cost of not creating such files, when just as the quick and anonymous freewriting session started to grow into something you'd want to keep around, an application error or computer crash caused you to lose it all and be more discouraged than when you began.

Regardless of what you're trying to do, it begins with writing. Writing text, writing code, writing thoughts. Writing isn't just a way to express yourself, but a way to think.

Mindstream removes the barriers so that you can start writing immediately, and so that you can feel secure enough to play with the things you write, knowing that you won't ever lose anything you care about, and at the same time, never have to think again about the things that have served their purpose in the moment.

Mindstream is a lightweight tool that leaves most of the heavy lifting to other packages (including major modes) and technologies (such as Git). It simply uses these together to augment your existing workflows to fill an unmet need.

Every mindstream session begins from a template (an ordinary file) that you provide, and evolves through the stages of your creative process. The session itself is stored as an ordinary file in a unique Git repository at a temporary location on disk. This repository is versioned by commits representing your writing process bounded at natural points -- by default, the points at which your buffer is saved (whether explicitly by you or implicitly on running a command like `racket-run <https://racket-mode.com/#racket_002drun>`_ or ``mindstream-clear``). You can save and load these sessions, too, and pick up right where you left off, allowing quick freewriting sessions to organically grow into robust creative works.

Typical uses of this package are for early stages of prototyping in a software project, or for exploratory programming to understand a new idea, tool, or technology. It's also great for just taking quick notes or freewriting blog posts or content in authoring settings in general.

Installation
============

This package isn't on `MELPA <https://melpa.org/>`_ yet, but you can install a pre-release version using `straight.el <https://github.com/raxod502/straight.el>`_ by putting this somewhere in your :code:`.emacs.d`:

.. code-block:: elisp

  (use-package mindstream
    :straight
    (mindstream
      :type git
      :host github
      :repo "countvajhula/mindstream")
    :config
    (mindstream-mode))

``(mindstream-mode)`` here initializes the package, providing global keybindings that allow you to enter Mindstream sessions from anywhere.

Usage
=====

Before you can use it the first time, you will need to create at least one template at ``~/.mindstream/templates/`` (see below for how to do that). Once you've done that, using Mindstream is as easy as:

1. Run ``mindstream-new`` (default: ``C-c C-r n``) to start a session.
2. Write!

Adding New Session Templates
----------------------------

Mindstream doesn't include any templates out of the box, so you'll probably want to create some for standard scratch sessions you are likely to need, for instance, for programming in your favorite language (perhaps `Racket <https://racket-lang.org/>`_?), or just freewriting some text for your next great novel, following in the keystrokes of Emacs octopuses like Neal Stephenson.

You can add new templates in ``mindstream-template-path`` (default: ``"~/.mindstream/templates/"``) -- ordinary text files with any extension you like (e.g. ``.txt``, ``.rkt``, ``.el``, anything) -- which will then be available as options in ``mindstream-new``.

Saving Sessions
---------------

You can also save scratch sessions that you'd like to keep by using ``mindstream-save-session`` (default binding: ``C-c C-r C-s``). This simply clones the session's Git repo to a more permanent and familiar path that you indicate (as opposed to the anonymous session path which is assumed to be temporary and defaults to ``/var/tmp/mindstream/``), thus preserving the entire session history, allowing it to be navigated and even resumed at any time in the future.

Entering Sessions Even More Quickly
-----------------------------------

``mindstream-switch-to-session-buffer`` (default: ``C-c C-r b``) will take you immediately to a new anonymous session buffer for the current major mode (if you've provided a template for it), without asking you any questions. If an anonymous session already exists, it will take you there rather than create a new one.

See "Design" below to learn more about anonymous sessions.

Live Mode!
----------

Live mode configures Mindstream to automatically take some action that you indicate whenever there is a pause (by default, 1.5 seconds) in typing. Typically, this is used in programming settings to trigger evaluation of the buffer in an accompanying runtime environment.

Live mode is configured by associating each major mode with a desired action to take for sessions in that mode.

For example, use the following config to evaluate your buffer "live" while in Racket Mode:

.. code-block:: elisp

  (plist-put mindstream-live-action-plist
             'racket-mode #'racket-run)

You can "go live" in any Mindstream session with ``M-x mindstream-go-live`` (default: ``C-c C-r C-l``). If no live action is configured for the major mode, it will simply use the default action of saving the buffer.

Go offline with ``M-x mindstream-go-offline`` (default: ``C-c C-r C-o``).

Explore
-------

Try ``M-x mindstream- ...`` to see all the available interactive commands. These are also included as keybindings in two minor modes:

- ``mindstream-mode``, which allows you to enter a Mindstream session from anywhere.
- ``mindstream-session-mode``, which contains useful commands for active sessions, like saving the session and clearing the buffer to restore a blank template.

Mindstream commands are bound by default under the prefix ``C-c C-r``. You can also view all Mindstream commands by running Emacs's ``C-h`` introspection with this prefix, as in ``C-c C-r C-h``.

Customization
=============

As each Mindstream session uses a specific major mode, it inherits all of the customizations you already have (and any that you decide to add) for that mode. There is typically nothing special you need to do beyond this for Mindstream to work seamlessly with all of your workflows when using these modes.

For instance, one common use of Mindstream is as a scratch buffer with Racket Mode. Racket Mode users sometimes `like to have a dedicated REPL <https://racket-mode.com/#Edit-buffers-and-REPL-buffers>`__ to view the output of code they write in a particular buffer, instead of reusing a REPL shared across all buffers. If you're a Racket Mode user, whatever customization you've chosen here would apply to Mindstream session buffers just as they would any buffer, and your Racket Mode sessions may or may not have a dedicated REPL depending on how you've customized this for Racket Mode generally.

But if you happen to want to use a different customization for Mindstream session buffers in a certain major mode than you prefer generally for that major mode, advising the ``mindstream-start-session`` function could be one way to achieve that. For instance, for the customization we have been talking about:

.. code-block:: elisp

  (advice-add 'mindstream-start-session
              :after
              (lambda (&rest _args)
                (setq-local racket-repl-buffer-name "*scratch - Racket REPL*")))

Design
======

Mindstream structures your workflow in sessions, which are version-controlled files. When you first start a session it begins as anonymous, meaning that it doesn't have a name. If the session develops into something worth keeping, you can save it to a preconfigured (or any) location on disk by giving the session a name. A session is stored as a version-controlled folder. You can also save just the file rather than the entire session. With that in mind, here are some properties of the design:

1. There is only one anonymous scratch session active at any time, per major mode.
2. Saving an anonymous session turns it into a named session, and there is no active anonymous session at that point. Named sessions work the same as anonymous sessions aside from having a name and being in a permanent location on disk. A new anonymous session could be started at any time via `mindstream-new`.
3. New sessions always begin as anonymous.
4. Named sessions may be loaded without interfering with the active anonymous session.
5. Any number of named sessions could be active at the same time. There is no global state, so that sessions are self-contained and independent.

Tips
====

Magit
-----

Mindstream sessions are stored as Git repos, so you can use standard Git tools as you might with any repo, including Magit.

Magit is useful to navigate the states in the session and see diffs representing the changes in each state. Of course, Magit can be used for a great many things, and you have that full power available to you to use with Mindstream sessions.

Git-Timemachine
---------------

The git-timemachine Emacs package is a great way to temporally navigate your session. Unlike the usual undo and redo operations which track edits with high granularity, mindstream sessions are bounded by ``save-buffer`` invocations which tend to represent natural, distinct stages in your development. Mindstream doesn't include a built-in way to navigate these states, but you can use the git-timemachine package to do this (in read-only mode).

Previewing
----------

Quick feedback loops are the engines of creative progress. With this in mind, for whatever you're writing, it's valuable to have a way to preview what you've produced in output form. For instance, if you're writing documentation, you should have a keybinding to quickly build the file into HTML or a PDF, or render it within the buffer itself (as LaTeX modes sometimes allow), for you to review as you go. Likewise, if you're writing code, you should have a way to quickly evaluate the contents of your buffer and see the result.

This tip is not about Mindstream specifically but more about a good workflow to develop with the major mode you're using. For instance, with Racket Mode, it would be advisable to bind the command ``racket-run`` so that you can quickly see the output of your code. This command also saves the buffer so that the session history would represent natural points at which you felt the code was worth trying out. Similarly, if you're writing Markdown or reStructuredText, you should explore the features provided by the relevant major modes that would allow you to preview the produced documentation in HTML form with the right keybinding incantation.

Choosing a Session Path
-----------------------

Mindstream stores anonymous sessions under a randomly generated folder name. This allows you to enter a freewriting session without worrying about the messy details of naming and storing files. As a result, it's likely that you will work on dozens, hundreds, or thousands of such sessions over time, of which you will keep only a small minority as saved, named sessions. For the anonymous sessions you don't save, you may prefer to just delete them from time to time rather than have them accumulate. Many operating systems provide standard ways to do this kind of thing -- *temp folders*, usually named ``tmp`` -- which are occasionally cleared automatically by the operating system, without requiring you to manage this. If your operating system provides a good option here, you may prefer to use it.

Your Emacs Folder
~~~~~~~~~~~~~~~~~

By default, anonymous sessions are placed in the ``mindstream/anon`` folder in your Emacs directory (e.g. ``.emacs.d``). This is a safe default, as it is entirely under your control and you can clear this folder (if you wish to) or leave it to its own devices, as you see fit. If you retain this default behavior, you may want to add ``mindstream/anon`` to your ``.gitignore`` for your Emacs directory (assuming you keep your Emacs config versioned and publicly hosted, as many Emacs users do), so that these freewrite sessions aren't publicly visible.

``/var/tmp``
~~~~~~~~~~~~

``/var/tmp`` is a standard path on Unix systems for holding temporary files. Unfortunately, *there is no accepted convention* on its handling. Some systems clear its contents rarely or never, while others clear its contents *on every reboot*. As a primary use for Mindstream is for you to have a reliable place to capture your thoughts with very low overhead, it's important that you should feel relatively secure that if your system were to crash, you would still be able to recover any (anonymous) Mindstream sessions you may have been in the middle of.

So if you'd like to use ``/var/tmp``, first check the contents of this folder and refer to the documentation on your particular system to see how it handles this path. If that behavior is predictable enough for you (e.g. say the folder is cleared only on OS upgrades), then you can use it like this:

.. code-block:: elisp

  :custom
  ...
  (mindstream-path "/var/tmp/mindstream")

Home/tmp
~~~~~~~~

Another option that's similar to this one but more predictable is to define a new path in your home folder for this purpose (say ``~/tmp``), that you are at liberty to periodically clear yourself, and which you could share across all applications for this purpose. If you go with this option, you can use this path in Mindstream like so:

.. code-block:: elisp

  :custom
  ...
  (mindstream-path
   (concat (file-name-as-directory (getenv "HOME"))
           "tmp/mindstream"))

Remember that the path we are configuring here is for *anonymous sessions* only. If you decide to keep a session around and save it via ``mindstream-save`` (default binding: ``C-c C-r C-s``), it would be saved to ``mindstream-save-session-path`` which defaults to your home folder. You can customize this as well, of course:

.. code-block:: elisp

  :custom
  ...
  (mindstream-save-session-path
   (concat (file-name-as-directory (getenv "HOME"))
           "my/mindstream/sessions/path"))

Acknowledgements
================

This package was conceived in `discussion with Greg Hendershott <https://github.com/greghendershott/racket-mode/issues/628>`_.

"Live mode" was inspired by coding demos given by `Matthew Flatt <https://users.cs.utah.edu/~mflatt/>`_ using `DrRacket <https://docs.racket-lang.org/drracket/index.html>`_.

Non-Ownership
=============

This work is not owned by anyone. Please see the `Declaration of Non-Ownership <https://github.com/drym-org/foundation/blob/main/Declaration_of_Non_Ownership.md>`_.
