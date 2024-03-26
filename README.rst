.. image:: https://github.com/countvajhula/mindstream/actions/workflows/test.yml/badge.svg
    :target: https://github.com/countvajhula/mindstream/actions

mindstream
==========

Versioned and sessioned scratch buffers for freewriting text and exploratory programming in your favorite language, in Emacs.

Every mindstream session begins from a template you provide and evolves through the stages of your creative process. The session is stored as a unique Git repository on disk. The Git repo contains commits representing the stages in your development process bounded at natural points -- by default, the points at which your buffer is saved (whether explicitly by you or implicitly on running a command like ``racket-run`` or ``mindstream-clear``). You can save and load these sessions, too.

Typical uses of this package are for early stages of prototyping in a software project, or for exporatory programming to understand a new idea, tool, or technology. It's also great for just taking quick notes or freewriting blog posts or content in authoring settings in general.

Installation
============

This package isn't on `MELPA <https://melpa.org/>`_ yet, but you can install a pre-release version using `straight.el <https://github.com/raxod502/straight.el>`_ by putting this somewhere in your :code:`.emacs.d`:

.. code-block:: elisp

  (use-package mindstream
    :after racket-mode
    :straight
    (mindstream
      :type git
      :host github
      :repo "countvajhula/mindstream")
    :config
    (mindstream-mode))

Usage
=====

If you'd like to try it out, follow these steps:

1. Follow the installation instructions above to install this package using straight.el
2. Ensure ``(mindstream-mode)`` is somewhere in your config (as in the example above). This provides keybindings (default prefix: ``C-c C-r``) for you to enter mindstream sessions and sets up triggers so that session buffers are implicitly versioned.
3. Create a new template at ``~/.mindstream/templates/`` for your favorite programming language (or just plain text).
4. Run ``mindstream-new`` to create a session.
5. Hack away!

Adding New Session Templates
----------------------------

Mindstream doesn't include any templates out of the box, so you'll probably want to create some for standard scratch sessions you are likely to need, for instance, for programming in your favorite language (perhaps Racket?), or just freewriting some text for your next great novel, following in the keystrokes of Emacs octopuses like Neal Stephenson.

You can add new templates in ``mindstream-template-path`` (default: ``"~/.mindstream/templates/"``) -- ordinary text files with any extension you like (e.g. ``.txt``, ``.rkt``, ``.el``, anything) -- which will then be available as options in ``mindstream-new``.

Saving Sessions
---------------

You can also save scratch sessions that you'd like to keep by using ``mindstream-save-session`` (default binding: ``C-c C-r C-s``). This simply clones the session's Git repo to a more permanent and familiar path that you indicate (as opposed to the anonymous session path which is assumed to be temporary and defaults to ``/var/tmp/mindstream/``), thus preserving the entire session history, allowing it to be navigated and even resumed at any time in the future.

Explore
-------

Try ``M-x mindstream- ...`` to see all the available interactive commands. These are also included as keybindings in two minor modes:

- ``mindstream-mode``, which allows you to enter a Mindstream session from anywhere.
- ``mindstream-session-mode``, which contains useful commands for active sessions, like saving the session and clearing the buffer to restore a blank template.

Mindstream commands are bound by default under the prefix ``C-c C-r``.

Design
======

Mindstream structures your workflow in sessions, which are version-controlled files. When you first start a session it begins as anonymous, meaning that it doesn't have a name. If the session develops into something worth keeping, you can save it to a preconfigured (or any) location on disk by giving the session a name. A session is stored as a version-controlled folder. You can also save just the file rather than the entire session. With that in mind, here are some properties of the design:

1. There is only one anonymous scratch session active at any time, per major mode.
2. Saving an anonymous session turns it into a named session, and there is no active anonymous session at that point. Named sessions work the same as anonymous sessions aside from having a name and being in a permanent location on disk. A new anonymous session could be started at any time via `mindstream-new`.
3. New sessions always begin as anonymous.
4. Named sessions may be loaded without interfering with the active anonymous session.
5. Any number of named sessions could be active at the same time. There is no global state, so that named sessions are self-contained and independent.

Tips
====

Magit
-----

Mindstream sessions are stored as Git repos, so you can use standard Git tools as you might with any repo, including Magit.

Magit is useful to navigate the states in the session and see diffs representing the changes in each state. Of course, Magit can be used for a great many things, and you have that full power available to you to use with Mindstream sessions.

Git-Timemachine
---------------

The git-timemachine Emacs package is a great way to temporally navigate your session. Unlike the usual undo and redo operations which track edits with high granularity, mindstream sessions are bounded by ``save-buffer`` invocations which tend to represent natural, distinct stages in your development. Mindstream doesn't include a built-in way to navigate these states, but you can use the git-timemachine package to do this (in read-only mode).

Acknowledgements
================

This package was conceived in `discussion with Greg Hendershott <https://github.com/greghendershott/racket-mode/issues/628>`_.

Non-Ownership
=============

This work is not owned by anyone. Please see the `Declaration of Non-Ownership <https://github.com/drym-org/foundation/blob/main/Declaration_of_Non_Ownership.md>`_.
