mindstream
==========

A rapid prototyping UX for Racket programming in Emacs, based on versioned and sessioned "scratch" buffers.

In the future, besides being usable at all (it's still in development!), this may be generalized for use with other languages and authoring settings in general.

Usage
=====

If you'd like to try out this very early and untested version (you courageous pioneer, you!), here's how you can do it:

1. Clone this repo.
2. Open ``mindstream.el`` and ``eval-buffer`` to evaluate the buffer.
3. Evaluate ``(mindstream-initialize)`` in the Emacs scratch buffer (or via ``M-x eval-expression``). This advises Racket Mode's ``racket-run`` to "iterate" the scratch buffer, providing implicit versioning for your Racket scratch buffer.
4. Run ``mindstream-new`` to create a Racket scratch buffer.
5. Hack away!

You can also explore adding new templates in ``mindstream-template-path`` (default: ``"~/.racket-mode/scratch/templates/"``) -- ordinary Racket files -- which will then be available as options in ``mindstream-new``. You can also save scratch buffers that you'd like to keep, or even entire scratch buffer sessions (which are simply saved as a directory containing a series of Racket files representing stages in your development process, bounded either by ``racket-run`` invocations, or by calls to ``mindstream-clear`` which restores the buffer to a "clear" state, i.e. to its original template form).

Try ``M-x mindstream- ...`` to see all the available interactive commands. These will soon be added to a global minor mode so that there will be convenient and customizable keybindings for them.

The ``mindstream-next`` and ``mindstream-previous`` features work just for navigation of your development history, but aren't properly modeled yet -- so, don't expect to be able to use them for anything other than read-only / save-file purposes.

Acknowledgements
================

This package was conceived in `discussion with Greg Hendershott <https://github.com/greghendershott/racket-mode/issues/628>`_.

"License"
==========
This work is "part of the world." You are free to do whatever you like with it and it isn't owned by anybody, not even the creators. Attribution would be appreciated and would help, but it is not strictly necessary nor required. If you'd like to learn more about this way of doing things and how it could lead to a peaceful, efficient, and creative world, and how you can help, visit `drym.org <https://drym.org>`_.
