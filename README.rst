.. image:: https://github.com/countvajhula/mindstream/actions/workflows/melpazoid.yml/badge.svg
    :target: https://github.com/countvajhula/mindstream/actions

.. image:: https://melpa.org/packages/mindstream-badge.svg
    :alt: MELPA
    :target: https://melpa.org/#/mindstream

.. image:: https://img.shields.io/badge/documentation-mindstream-blue
    :target: https://countvajhula.github.io/mindstream/

mindstream
==========

If you've ever created throwaway files named ``1.txt`` or ``blah.py`` or ``test3.rkt`` to quickly write down some thoughts, try out a new idea, or prototype a new software tool, then you've already experienced the inconvenience of having to first come up with a name for these throwaway files, a small obstruction that is sometimes enough to snuff out that momentary creative spark that might have become a worthy conflagration. You've also probably experienced the cost of not creating such files, when just as the quick and anonymous freewriting session started to grow into something you'd want to keep around, an application error or computer crash caused you to lose it all and be more discouraged than when you began.

Regardless of what you're trying to do, it begins with writing. Writing text, writing code, writing thoughts. Writing isn't just a way to express yourself, but a way to think.

Mindstream removes the barriers so that you can start writing immediately, and so that you can feel secure enough to play with the things you write, knowing that you won't ever lose anything you care about, and at the same time, never have to think again about the things that have served their purpose in the moment.

Mindstream is a lightweight tool that leaves most of the heavy lifting to other packages (including major modes) and technologies (such as Git). It simply uses these together to augment your existing workflows to fill an unmet need.

Every mindstream session begins from a template (an ordinary folder) that you provide, and evolves through the stages of your creative process. The session itself is stored as an ordinary folder in a unique Git repository at a temporary location on disk. This repository is versioned by commits representing your writing process bounded at natural points -- by default, the points at which your buffer is saved (whether explicitly by you or implicitly on running a command like `racket-run <https://racket-mode.com/#racket_002drun>`_). In this way, Mindstream saves you the trouble of coming up with extraneous names (e.g. ``draft1.tex``, ``draft2.tex``, ..., ``draft_final2.tex``, ...) and allows you to focus on the task at hand. You can save and load these sessions, too, and pick up right where you left off, allowing quick freewriting sessions to organically grow into robust creative works.

Typical uses of this package are for early stages of prototyping in a software project, or for exploratory programming to understand a new idea, tool, or technology. It's also great for just taking quick notes or freewriting blog posts or content in authoring settings in general.

Installation
============

Mindstream is on `MELPA <https://melpa.org/>`_, so you can install it in the usual way (either via `Straight.el <https://github.com/radian-software/straight.el>`_ (recommended) or Emacs's built-in package.el), assuming you have MELPA in your configured list of package archives.

Place the following config somewhere in your ``.emacs.d``:

.. code-block:: elisp

  (use-package mindstream
    :config
    (mindstream-mode))

``(mindstream-mode)`` initializes the package, providing global keybindings that allow you to enter Mindstream sessions from anywhere, and registering hooks that allow sessions to be versioned when buffers are saved.

Usage
=====

1. Run ``mindstream-new`` (default: ``C-c , n``) to start a session.
2. Write!

Save the file at your regular cadence after making a few changes to it. If you pull up a Magit window, you'll notice that there is a distinct commit recorded in the underlying Git repo for your session each time that you saved the file. You can also create new files at the same path, and changes to any of them would also be similarly tracked.

You may notice that there is only one template available to use for your first session. This is because Mindstream creates a simple text template in ``~/.emacs.d/mindstream/templates/`` if it doesn't find any there. You may want to add more templates that are relevant for you. Learn how to do that (and much more) in the `Mindstream documentation <https://countvajhula.github.io/mindstream/>`_.

Documentation
=============

`Mindstream documentation <https://countvajhula.github.io/mindstream/>`_.

Acknowledgements
================

This package was conceived in `discussion with Greg Hendershott <https://github.com/greghendershott/racket-mode/issues/628>`_.

"Live mode" was inspired by coding demos given by `Matthew Flatt <https://users.cs.utah.edu/~mflatt/>`_ using `DrRacket <https://docs.racket-lang.org/drracket/index.html>`_.

Non-Ownership
=============

This work is not owned by anyone. Please see the `Declaration of Non-Ownership <https://github.com/drym-org/foundation/blob/main/Declaration_of_Non_Ownership.md>`_.
