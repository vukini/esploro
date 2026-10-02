# Esploro

A file explorer for desktops that are Lisp themselves: [StumpWM](https://stumpwm.github.io/), with Emacs beside it. Its window is a frame of Emacs, built on dired; its core, which does every change to files, is Common Lisp. *Esploro* is Esperanto for "exploration".

It does what a file manager can't when the window manager, the editor and the explorer all speak one language:

- **It knows which window has a file open.** Beside each file, the windows that have it ("open in Emacs on 2 (unsaved)"); opening a file that's already open goes to its window instead of opening it twice. From StumpWM (the windows and their processes), `/proc` (what they hold open, were started on, are working in) and Emacs (its buffers).
- **Every change can be undone.** Copying, moving, pasting, renaming, a new folder, the Trash and files dropped in are each a plan, a list of Lisp forms, checked whole before anything changes and done in the background. The Trash is the freedesktop one, and every plan is kept in a journal, so undo puts things back. A plan may only do those few things, whoever wrote it, you or an agent.
- **A command is defined once.** `define-file-command` names the kinds of file it's for; the same commands are meant for the window's menus, rofi, StumpWM keys and agents next.

And what a file manager is expected to do, with the mouse as much as the keys: a menu bar and a tool bar, right-click menus, click to select and double-click to open, Ctrl+click and Shift+click, dragging files out to other programs and dropping them in, places down the side (home, your folders, drives, GTK's bookmarks, the Trash), back and forward, sorting, a filter, finding below, the Trash to look in and restore from, and copy and paste that other file managers understand.

The ideas, and what comes later, are in [DESIGN.md](DESIGN.md). The first window, in McCLIM, is kept on the branch `mcclim`.

## Running it

The core needs SBCL alone; the window, Emacs 29 or later (and `fd` to find below, `xclip` for copy and paste with other programs).

```sh
make                 # builds ./esploro, the core's command (seconds)
make install         # into ~/.local/bin
make test            # the core's tests (SBCL) and the window's (Emacs in batch)
```

Then, in Emacs:

```elisp
(add-to-list 'load-path "~/src/esploro/emacs")
(require 'esploro)
```

`M-x esploro` opens its frame; `esploro FOLDER` from a shell or a key does the same through Emacs's server. In it, `?` shows the keys and the mouse. `esploro --where [PATH]` says which windows have a file open.

It knows the windows when StumpWM runs Swank (on 127.0.0.1:4004; `ESPLORO_SWANK_PORT` changes it, and the password in `~/.slime-secret` is sent when there is one). Without it, it is a plain explorer. [Vikix](https://vikix.dev) sets it all up.

## Writing a command

```lisp
(esploro:define-file-command shrink ((path :image) :changes t)
  "Plan a smaller copy beside it."
  (list (list :copy path (esploro::free-name path " small"))))

(esploro:define-file-command show-in-nsxiv ((path :image))
  "Show it in nsxiv."
  (esploro:launch "nsxiv" path))
```

Commands live in the core; the window's menus offering them is the next step. Kinds: `:folder`, `:image`, `:video`, `:audio`, `:pdf`, `:text`, `:lisp` (also `:text`), `:archive`, `:file` (anything but a folder), `t` (anything). A command with `:changes t` returns plan steps instead of touching files.

## Licence

MIT.
