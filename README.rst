.. image:: https://github.com/countvajhula/mindstream/actions/workflows/test.yml/badge.svg
    :target: https://github.com/countvajhula/mindstream/actions

mindstream
==========

A rapid prototyping UX for Racket programming in Emacs, based on versioned and sessioned "scratch" buffers.

Every mindstream session is represented as a unique Git repository on disk. The Git repo contains commits representing the stages in your development process, bounded either by ``racket-run`` invocations, or by calls to ``mindstream-clear`` which restores the buffer to a "clear" state, i.e. to its original template form. You can save and load these sessions, too.

In the future, this package may be generalized for use with other languages and in authoring settings in general.

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
    (mindstream-initialize))

Usage
=====

If you'd like to try it out, follow these steps:

1. Follow the installation instructions above to install this package using straight.el
2. Ensure ``(mindstream-initialize)`` is somewhere in your config. This advises Racket Mode's ``racket-run`` to "iterate" the scratch buffer, providing implicit versioning for your Racket scratch buffer.
3. Run ``mindstream-new`` to create a Racket scratch buffer.
4. Hack away!

You can also explore adding new templates in ``mindstream-template-path`` (default: ``"~/.mindstream/templates/"``) -- ordinary Racket files -- which will then be available as options in ``mindstream-new``. You can also save scratch buffers that you'd like to keep, or even entire scratch buffer sessions (which clones the mindstream Git repo to a location you specify).

Try ``M-x mindstream- ...`` to see all the available interactive commands. These are also included as keybindings in a minor mode -- ``mindstream-mode`` -- which is enabled locally in scratch buffers.

Tips
====

Magit
-----

Mindstream sessions are stored as Git repos, so you can use standard Git tools as you might with any repo, including Magit.

Magit is useful to navigate the states in the session and see diffs representing the changes in each state. Of course, Magit can be used for a great many things, and you have that full power available to you to use with Mindstream sessions.

Git-Timemachine
---------------

The git-timemachine Emacs package is a great way to temporally navigate your session. Unlike the usual undo and redo operations which track edits with high granularity, mindstream sessions are bounded by ``racket-run`` invocations which tend to represent natural, distinct stages in your development. Mindstream doesn't include a built-in way to navigate these states, but you can use the git-timemachine package to do this (in read-only mode).

Acknowledgements
================

This package was conceived in `discussion with Greg Hendershott <https://github.com/greghendershott/racket-mode/issues/628>`_.

"License"
==========
This work is "part of the world." You are free to do whatever you like with it and it isn't owned by anybody, not even the creators. Attribution would be appreciated and would help, but it is not strictly necessary nor required. If you'd like to learn more about this way of doing things and how it could lead to a peaceful, efficient, and creative world, and how you can help, visit `drym.org <https://drym.org>`_.
