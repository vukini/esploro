# Esploro

A file explorer in Common Lisp, for desktops that are Lisp themselves: [StumpWM](https://stumpwm.github.io/), with Emacs beside it. *Esploro* is Esperanto for "exploration".

It doesn't try to out-list dired. It does what a file manager can't when the window manager, the editor and the explorer all speak one language:

- **It knows which window has a file open.** Beside each file, the windows that have it ("open in Emacs on 2 (unsaved)"); opening a file that's already open goes to its window instead of opening it twice. From StumpWM (the windows and their processes), `/proc` (what they hold open, were started on, are working in) and Emacs (its buffers).
- **Plan, then apply.** Moving, copying, renaming and deleting go into a plan first, a list of Lisp forms you can read, edit in Emacs, and apply in one go. Deleting goes to the Trash (the freedesktop one), and every applied plan can be undone. A plan is checked whole before anything changes, and may only do those five things, whoever wrote it, you or an agent.
- **A command is defined once.** `define-file-command` names the kinds of file it's for; McCLIM's presentations then offer it on those files' menus. The same commands are meant for rofi, Emacs, StumpWM keys and agents next.

The ideas, and what comes later, are in [DESIGN.md](DESIGN.md).

## Running it

Needs SBCL and [Quicklisp](https://www.quicklisp.org/) (for McCLIM, fetched the first time).

```sh
make                 # builds ./esploro, one program that opens at once
./esploro [FOLDER]   # the window
./esploro --where [PATH]   # which windows have PATH open, or every open file
make install         # into ~/.local/bin
make test            # the core's tests: SBCL alone, no X
```

In the window, `?` or F1 shows help in the side pane. Click a file to open it (or go to the window that has it), click a folder to go in, shift-click to mark, right-click for what can be done with it; Alt+Up goes up, F5 reads the folder again, Ctrl+z undoes. Changes collect in the plan on the right, applied or edited in Emacs with a click; the bottom line takes commands by name (`Go ~/src`, `New Folder`, `Move Marked`, `Undo`, `Toggle Hidden`).

It knows the windows when StumpWM runs Swank (on 127.0.0.1:4004; `ESPLORO_SWANK_PORT` changes it, and the password in `~/.slime-secret` is sent when there is one), and Emacs's buffers when Emacs runs its server. Without them it is a plain explorer. [Vikix](https://vikix.dev) sets both up.

## Writing a command

```lisp
(esploro:define-file-command shrink ((path :image) :changes t)
  "Plan a smaller copy beside it."
  (list (list :copy path (esploro::free-name path " small"))))

(esploro:define-file-command show-in-nsxiv ((path :image))
  "Show it in nsxiv."
  (esploro:launch "nsxiv" path))
```

Kinds: `:folder`, `:image`, `:video`, `:audio`, `:pdf`, `:text`, `:lisp` (also `:text`), `:archive`, `:file` (anything but a folder), `t` (anything). A command with `:changes t` returns plan steps instead of touching files.

## Licence

MIT.
