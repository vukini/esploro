# Esploro

Esploro (Esperanto for "exploration") is a file explorer written in Common Lisp with McCLIM, for desktops that are themselves Lisp: StumpWM, with Emacs beside it. It began as the first of the Lisp apps planned in Vikix's `IDEAS.md` (Lisp apps for the Lisp desktop), and the showcase one.

Decided with Vid on 2026-10-01: its own project (github.com/vukini/esploro), so it has its own releases and anyone on StumpWM can use it. Vikix installs it as it installs Lem: a pinned commit, built by `vikix lisp-apps setup`.

**What it needs, and what's optional.** The core needs only SBCL. The window needs McCLIM (Quicklisp). StumpWM and Emacs make it more, never less: without StumpWM's Swank it doesn't know the windows, without an Emacs server it doesn't know the buffers, and it still works as an explorer. How it reaches StumpWM is one small file of its own (a Swank connection, the password in `~/.slime-secret` when there is one), not Vikix's `vikix eval`, so it doesn't need Vikix.

## Why another file explorer

It shouldn't try to out-list dired: Emacs already lists, marks and renames (wdired) very well. Esploro's reason to exist is that it is the one program that understands files, windows, workspaces and agents together. Elsewhere those live in separate processes that speak different languages; here StumpWM, Esploro and Emacs all speak Lisp, and can share the same objects.

## The ideas

1. **It knows which window has a file open.** StumpWM knows the windows; `/proc` knows each process's open files, its arguments and (for a shell) its folder; Emacs knows its buffers. So a file shows where it's open ("in Emacs on workspace 2", "in mpv"), and opening a file that's already open jumps to that window instead of starting a second copy. The other way too: from any window, reveal the file behind it.
2. **Workspace means project.** Esploro opens in the folder of the workspace you're on. Dropping a file on a workspace opens it there. Closing a project lists the files it still has open, unsaved ones first.
3. **Plan, then apply.** Moving, renaming, copying and deleting don't happen at once. Each becomes a Lisp form in a plan, `(:move "/from" "/to")` (paths as plain strings: CL pathnames trip on names with `*` or `[`). You look at the plan, edit it in Emacs as text (wdired, but for every operation), then apply it. Deleting goes to the Trash, and every applied plan keeps its inverse, so `undo` puts things back. Agents use the same road: they never touch files, they propose a plan, which you read, and whose steps are checked against a short list of what a plan may do.
4. **Define a command once, and it's everywhere.** `(define-file-command shrink ((f image)) …)` shows up in Esploro's menu for images (McCLIM presentations know the thing on screen is an image), in a rofi menu, in Emacs (embark on a file name), on a StumpWM key, and in the MCP server as a tool for agents. Other file managers have plugins that work only inside them.
5. **Folders can be queries.** A "folder" can be a saved s-expression, `(and (type image) (modified this-week) (in-project "vikix"))`, that updates live, can be edited and combined. Spotlight has smart folders; none can be opened up, edited, or used in a rule.
6. **The selection is a shared value.** The selection is a Lisp list that StumpWM, Emacs and agents can read. Select files in Esploro and `(mapc #'shrink *selection*)` in the REPL; mark files in dired and they're selected in Esploro. "These files" means the same everywhere.
7. **Repeat what I just did.** The plan journal is Lisp forms, so a sequence of actions can become a named command, replayed on a new selection. The apprentice (`IDEAS.md`) could suggest these.

## Phase 0: the core, then a window

Ideas 1, 3 and 4 first: each useful alone, none a copy of dired, and together they show why the app is in Lisp. Queries (5) come after.

The core is a library with no window, `esploro/core`, needing only SBCL (UIOP and sb-posix come with it), so its tests run on GitHub without McCLIM or X:

- `files.lisp`: a folder's entries (name, kind, size, time); the kind (folder, image, video, audio, pdf, text, lisp, archive, file) is what presentations and commands go by.
- `plan.lisp`: plans as lists of steps, `(:move FROM TO)`, `(:copy FROM TO)`, `(:rename FROM TO)`, `(:mkdir DIR)`, `(:trash FILE)`. A step outside that list is refused, so a plan from an agent or from an edit in Emacs can't do anything else. Checked before anything changes (sources exist, targets free), applied in order, and recorded with its inverse in a journal (`~/.local/state/esploro/`), which `undo` replays backwards. The Trash is the freedesktop one (`~/.local/share/Trash`), so other programs see and restore it too.
- `commands.lisp`: `define-file-command`, a registry of commands by kind. A command either acts now (open, reveal) or returns steps for the plan.
- `stumpwm.lisp`: a Swank client for the running StumpWM (send a form, read the answer); nothing when StumpWM isn't there.
- `where.lisp`: where each file is open. Windows come from StumpWM, each with its process (`_NET_WM_PID`); the process and its children give their open files, arguments and folders; Emacs gives its buffers' files through `emacsclient`. Focusing a window goes back through StumpWM.

Then the window, `esploro` (McCLIM): the folder's list, each entry a presentation of its file, so the right-click menu is the commands for its kind; marks; the plan in a pane beside it, applied with one key, edited in Emacs with another; "open in" shown beside each open file; Enter on an open file goes to its window. Built as one program with `make` (`save-lisp-and-die`), so it opens at once. In Vikix, `vikix lisp-apps setup` builds it, and it's `esploro` on the command line and in Super+d.

## Where it is (2026-10-01)

Phase 0's core is done and tested (`make test`, 63 checks): folders, plans checked on paper then applied with restarts when a step fails (retry, skip, stop, put back), the freedesktop Trash, the journal and undo, the Swank client, the window map (StumpWM + `/proc` + Emacs), and `define-file-command` with the first commands (open in Emacs, terminal here, open with default, duplicate, trash). The window lists a folder with presentations, marks, the plan pane with Apply / Edit / Clear / Undo, "open in" beside open files, and the restarts as a menu when a step fails. `esploro --where` answers from the command line.

Not yet: tried by hand at length (it was driven by a script so far), the theme, keys for the common commands, Vikix installing it.

## Later

- The other doors for commands: rofi, Emacs (embark), StumpWM keys, the MCP server.
- Folders as queries, and saving them.
- The shared selection, with dired.
- Workspace means project: opening in the workspace's folder, dropping on a workspace.
- Recording a plan as a named command.
- Thumbnails, and drag and drop.

## The risk

McCLIM looks dated and may be slow on large folders. If Phase 0 feels clunky, keep the core (commands defined once, plans, the window link) as a library and put the front in Emacs: a dired that StumpWM drives. The idea survives with a cheaper face, which is why the core has no window in it.
