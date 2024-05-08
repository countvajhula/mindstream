#lang scribble/manual

@(define-syntax-rule (keybinding arg)
   (bold (code arg)))

@title{mindstream}

If you've ever created throwaway files named @code{1.txt} or @code{blah.py} or @code{test3.rkt} to quickly write down some thoughts, try out a new idea, or prototype a new software tool, then you've already experienced the inconvenience of having to first come up with a name for these throwaway files, a small obstruction that is sometimes enough to snuff out that momentary creative spark that might have become a worthy conflagration. You've also probably experienced the cost of not creating such files, when just as the quick and anonymous freewriting session started to grow into something you'd want to keep around, an application error or computer crash caused you to lose it all and be more discouraged than when you began.

Regardless of what you're trying to do, it begins with writing. Writing text, writing code, writing thoughts. Writing isn't just a way to express yourself, but a way to think.

Mindstream removes the barriers so that you can start writing immediately, and so that you can feel secure enough to play with the things you write, knowing that you won't ever lose anything you care about, and at the same time, never have to think again about the things that have served their purpose in the moment.

Mindstream is a lightweight tool that leaves most of the heavy lifting to other packages (including major modes) and technologies (such as Git). It simply uses these together to augment your existing workflows to fill an unmet need.

Every mindstream session begins from a template (an ordinary folder) that you provide, and evolves through the stages of your creative process. The session itself is stored as an ordinary folder in a unique Git repository at a temporary location on disk. This repository is versioned by commits representing your writing process bounded at natural points -- by default, the points at which your buffer is saved (whether explicitly by you or implicitly on running a command like @hyperlink["https://racket-mode.com/#racket_002drun"]{racket-run}). In this way, Mindstream saves you the trouble of coming up with extraneous names (e.g. @code{draft1.tex}, @code{draft2.tex}, ..., @code{draft_final2.tex}, ...) and allows you to focus on the task at hand. You can save and load these sessions, too, and pick up right where you left off, allowing quick freewriting sessions to organically grow into robust creative works.

Typical uses of this package are for early stages of prototyping in a software project, or for exploratory programming to understand a new idea, tool, or technology. It's also great for just taking quick notes or freewriting blog posts or content in authoring settings in general.

@table-of-contents[]

@section{Installation}

Mindstream is on @hyperlink["https://melpa.org/"]{MELPA}, so you can install it in the usual way (either via @hyperlink["https://github.com/radian-software/straight.el"]{Straight.el} (recommended) or Emacs's built-in package.el), assuming you have MELPA in your configured list of package archives.

Place the following config somewhere in your @code{.emacs.d}:

@codeblock{
  (use-package mindstream
    :config
    (mindstream-mode))
}

@code{(mindstream-mode)} initializes the package, providing global keybindings that allow you to enter Mindstream sessions from anywhere, and registering hooks that allow sessions to be versioned when buffers are saved.

@section{Usage}

@itemlist[#:style 'ordered
    @item{Run @code{mindstream-new} (default: @keybinding{C-c , n}) to start a session.}
    @item{Write!}
]

Save the file at your regular cadence after making a few changes to it. If you pull up a Magit window, you'll notice that there is a distinct commit recorded in the underlying Git repo for your session each time that you saved the file. You can also create new files at the same path, and changes to any of them would also be similarly tracked.

You may notice that there is only one template available to use for your first session. This is because Mindstream creates a simple text template in @code{~/.emacs.d/mindstream/templates/} if it doesn't find any there. You may want to add more templates that are relevant for you. Let's see how to do that.

@subsection{Adding New Session Templates}

Mindstream doesn't include any templates out of the box, so you'll probably want to create some for standard sessions you are likely to need, for instance, for programming in your favorite language (perhaps @hyperlink["https://racket-lang.org/"]{Racket}?), or just freewriting some text for your next great novel, following in the keystrokes of Emacs octopuses like Neal Stephenson.

To add a new template, visit @code{mindstream-template-path} (default: @code{"~/.mindstream/templates/"}) in your filemanager of choice (e.g. Emacs's @code{dired}, or just a command line), and create a folder there with the name of your template (e.g. @code{racket}). The folder should contain an ordinary file (or multiple files) with the appropriate extension (e.g. @code{.rkt} -- it could be anything at all that you typically use Emacs to edit). This template will now be available as an option in @code{mindstream-new}.

@subsection{Saving Sessions}

You can also save scratch sessions that you'd like to keep by using @code{mindstream-save-session} (default binding: @keybinding{C-c , C-s}). This simply clones the session's Git repo to a more permanent and familiar path that you indicate (as opposed to the anonymous session path which is assumed to be temporary and defaults to @code{~/.emacs.d/mindstream/anon}), thus preserving the entire session history, allowing it to be navigated and even resumed at any time in the future.

@subsection{Entering Sessions Even More Quickly}

@code{mindstream-enter-anonymous-session} (default: @keybinding{C-c , b}) will take you immediately to a new anonymous session for the current major mode, without asking you any questions. If an anonymous session already exists, it will take you there rather than create a new one. In creating a new session, it will use the first template it finds that is recognizable to your current major mode.

If you've got more than one template for a particular major mode, you may want to indicate which one is preferred rather than leave it to chance or accidents of alphabetical order. You can do this by associating each major mode with the name of the preferred template. For example:

@codeblock{
  :custom
  ...
  (mindstream-preferred-template '(racket-mode "racket.rkt"))
}

This customization is only relevant when using @code{mindstream-enter-anonymous-session}, as you would select the template yourself when using @code{mindstream-new}.

See "Design" below to learn more about anonymous sessions.

@subsection{Live Mode!}

Live mode configures Mindstream to automatically take some action that you indicate whenever there is a pause (by default, 1.5 seconds) in typing. Typically, this is used in programming settings to trigger evaluation of the buffer in an accompanying runtime environment.

Live mode is configured by associating each major mode with a desired action to take for sessions in that mode.

For example, use the following config to evaluate your buffer "live" while in Racket Mode:

@codeblock{
  :custom
  ...
  (mindstream-live-action '(racket-mode racket-run))
}

You can "go live" in any Mindstream session with @keybinding{M-x mindstream-go-live} (default: @keybinding{C-c , C-l}). If no live action is configured for the major mode, it will simply use the default action of saving the buffer.

Currently, live mode is only supported for individual buffers rather than for the session as a whole, so you would need to "go live" in each session buffer individually.

Go offline with @keybinding{M-x mindstream-go-offline} (default: @keybinding{C-c , C-o}).

@subsection{Mindstream Anywhere}

If you have an existing, ordinary file or directory that you were working on at some point, and if you want to continue working on it in a mindstream session, that's easy enough to do. Simply follow these steps:

@itemlist[#:style 'ordered
  @item{Create a new folder (give it a representative name, as you would any Mindstream session) and move the file(s) into it.}
  @item{At the command line in that folder, run @code{git init}.}
  @item{Open the file in Emacs in the usual way and @keybinding{M-x mindstream-begin-session}.}
]

Mindstream sessions are just ordinary Git repositories. If you wanted to, you could use Mindstream in any Git repo simply by @keybinding{M-x mindstream-begin-session} after opening a file in the repo, but this isn't a well-supported use case for the moment (e.g. it would result in a lot of commits, and you would most likely want to manually squash them).

@subsection{Explore}

Try @keybinding{M-x mindstream- ...} to see all the available interactive commands. These are also included as keybindings in a minor mode, @code{mindstream-mode}, which allows you to enter a Mindstream session from anywhere, and contains useful commands for active sessions like saving the session, "going live," and so on.

Mindstream commands are bound by default under the prefix @keybinding{C-c , ...}. You can view all Mindstream commands by using Emacs's @keybinding{C-h} introspection with this prefix, as in @keybinding{C-c , C-h}.

@section{Customization}

As each Mindstream session uses a specific major mode, it inherits all of the customizations you already have (and any that you decide to add) for that mode. There is typically nothing special you need to do beyond this for Mindstream to work seamlessly with all of your workflows when using these modes.

For instance, one common use of Mindstream is as a scratch buffer with Racket Mode. Racket Mode users sometimes @hyperlink["https://racket-mode.com/#Edit-buffers-and-REPL-buffers"]{like to have a dedicated REPL} to view the output of code they write in a particular buffer, instead of reusing a REPL shared across all buffers. If you're a Racket Mode user, whatever customization you've chosen here would apply to Mindstream session buffers just as they would any buffer, and your Racket Mode sessions may or may not have a dedicated REPL depending on how you've customized this for Racket Mode generally.

But if you happen to want to use a different customization for Mindstream session buffers in a certain major mode than you prefer generally for that major mode, advising the @code{mindstream-new} function could be one way to achieve that. For instance, for the customization we have been talking about:

@codeblock{
  (advice-add 'mindstream-new
              :after
              (lambda (&rest _args)
                (setq-local racket-repl-buffer-name "*scratch - Racket REPL*")))
}

@section{Design}

Mindstream structures your workflow in sessions, which are version-controlled files. When you first start a session it begins as anonymous, meaning that it doesn't have a name. If the session develops into something worth keeping, you can save it to a preconfigured (or any) location on disk by giving the session a name. A session is stored as a version-controlled folder. You can also save just the file rather than the entire session. With that in mind, here are some properties of the design:

@itemlist[#:style 'ordered
  @item{There is only one anonymous scratch session active at any time, per major mode.}
  @item{Saving an anonymous session turns it into a named session, and there is no active anonymous session at that point. Named sessions work the same as anonymous sessions aside from having a name and being in a permanent location on disk. A new anonymous session could be started at any time via @code{mindstream-new}.}
  @item{New sessions always begin as anonymous.}
  @item{Named sessions may be loaded without interfering with the active anonymous session.}
  @item{Any number of named sessions could be active at the same time. Sessions are self-contained and independent.}
]

@section{Tips}

@subsection{Magit}

Mindstream sessions are stored as Git repos, so you can use standard Git tools as you might with any repo, including Magit.

Magit is useful to navigate the states in the session and see diffs representing the changes in each state. Of course, Magit can be used for a great many things, and you have that full power available to you to use with Mindstream sessions.

@subsection{Git-Timemachine}

The git-timemachine Emacs package is a great way to temporally navigate your session. Unlike the usual undo and redo operations which track edits with high granularity, mindstream sessions are bounded by @code{save-buffer} invocations which tend to represent natural, distinct stages in your development. Mindstream doesn't include a built-in way to navigate these states, but you can use the git-timemachine package to do this (in read-only mode).

@subsection{Previewing}

Quick feedback loops are the engines of creative progress. With this in mind, for whatever you're writing, it's valuable to have a way to preview what you've produced in output form. For instance, if you're writing documentation, you should have a keybinding to quickly build the file into HTML or a PDF, or render it within the buffer itself (as LaTeX modes sometimes allow), for you to review as you go. Likewise, if you're writing code, you should have a way to quickly evaluate the contents of your buffer and see the result.

This tip is not about Mindstream specifically but more about a good workflow to develop with the major mode you're using. For instance, with Racket Mode, it would be advisable to bind the command @code{racket-run} so that you can quickly see the output of your code. This command also saves the buffer so that the session history would represent natural points at which you felt the code was worth trying out. Similarly, if you're writing Markdown or reStructuredText, you should explore the features provided by the relevant major modes that would allow you to preview the produced documentation in HTML form with the right keybinding incantation.

@subsection{Marking Significant Versions}

Mindstream sessions can have @emph{a lot} of commits, and they ensure that you never need to name your document at different stages of development out of fear that you'll lose important work. But if you happen to be writing a book, say, you might like to mark specific points as being significant in some way, so that if you ever have to search through earlier versions of your work, you'll know where to look. Traditionally, you might rename your file something like @code{draft_first_revision_final.tex}. From Mindstream's perspective, this a confusion of space and time. We seek to capture a moment in time, but we use a distinct place in space (a new file) for this purpose. In Mindstream, it'd be better to use a standard feature of Git, @emph{tagging}, to achieve the same result. That is, when you arrive at a version you think is significant, simply tag the current commit (using a Git client of your choice, such as Magit) with a name and a message. It's a much more useful way to do it than @code{draft_final_final_....tex}!

@subsection{Choosing a Session Path}

Mindstream stores anonymous sessions under a randomly generated folder name. This allows you to enter a freewriting session without worrying about the messy details of naming and storing files. As a result, it's likely that you will work on dozens, hundreds, or thousands of such sessions over time, of which you will keep only a small minority as saved, named sessions. For the anonymous sessions you don't save, you may prefer to just delete them from time to time rather than have them accumulate. Many operating systems provide standard ways to do this kind of thing -- @emph{temp folders}, usually named @code{tmp} -- which are occasionally cleared automatically by the operating system, without requiring you to manage this. If your operating system provides a good option here, you may prefer to use it.

@subsubsection{Your Emacs Folder}

By default, anonymous sessions are placed in the @code{mindstream/anon} folder in your Emacs directory (e.g. @code{.emacs.d}). This is a safe default, as it is entirely under your control and you can clear this folder (if you wish to) or leave it to its own devices, as you see fit. If you retain this default behavior, you may want to add @code{mindstream/anon} to your @code{.gitignore} for your Emacs directory (assuming you keep your Emacs config versioned and publicly hosted, as many Emacs users do), so that these freewrite sessions aren't publicly visible.

@subsubsection{@code{/var/tmp}}

@code{/var/tmp} is a standard path on Unix systems for holding temporary files. Unfortunately, @emph{there is no accepted convention} on its handling. Some systems clear its contents rarely or never, while others clear its contents @emph{on every reboot}. As a primary use for Mindstream is for you to have a reliable place to capture your thoughts with very low overhead, it's important that you should feel relatively secure that if your system were to crash, you would still be able to recover any (anonymous) Mindstream sessions you may have been in the middle of.

So if you'd like to use @code{/var/tmp}, first check the contents of this folder and refer to the documentation on your particular system to see how it handles this path. If that behavior is predictable enough for you (e.g. say the folder is cleared only on OS upgrades), then you can use it like this:

@codeblock{
  :custom
  ...
  (mindstream-path "/var/tmp/mindstream")
}

@subsubsection{Home/tmp}

Another option that's similar to this one but more predictable is to define a new path in your home folder for this purpose (say @code{~/tmp}), that you are at liberty to periodically clear yourself, and which you could share across all applications for this purpose. If you go with this option, you can use this path in Mindstream like so:

@codeblock{
  :custom
  ...
  (mindstream-path
   (concat (file-name-as-directory (getenv "HOME"))
           "tmp/mindstream"))
}

Remember that the path we are configuring here is for @emph{anonymous sessions} only. If you decide to keep a session around and save it via @code{mindstream-save} (default binding: @keybinding{C-c , C-s}), it would be saved to @code{mindstream-save-session-path} which defaults to your home folder. You can customize this as well, of course:

@codeblock{
  :custom
  ...
  (mindstream-save-session-path
   (concat (file-name-as-directory (getenv "HOME"))
           "my/mindstream/sessions/path"))
}

@subsection{Organizing Sessions}

Over time, you may want to rename some sessions, delete others, or reorganize them in some way so you can keep track of them more easily. What special features does Mindstream provide for this? @emph{None}! Remember, Mindstream sessions are just @emph{ordinary folders} containing @emph{ordinary Git repositories}. You can use normal Emacs or shell tools to rename, delete, organize them as you see fit, and you would still be able to load them in Mindstream as usual (once you navigate to what may be their new locations).

@section{Acknowledgements}

This package was conceived in @hyperlink["https://github.com/greghendershott/racket-mode/issues/628"]{discussion with Greg Hendershott}.

"Live mode" was inspired by coding demos given by @hyperlink["https://users.cs.utah.edu/~mflatt/"]{Matthew Flatt} using @hyperlink["https://docs.racket-lang.org/drracket/index.html"]{DrRacket}.

@section{Non-Ownership}

This work is not owned by anyone. Please see the @hyperlink["https://github.com/drym-org/foundation/blob/main/Declaration_of_Non_Ownership.md"]{Declaration of Non-Ownership}.
