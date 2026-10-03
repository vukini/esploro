# Esploro: to do

Ideas not built yet, roughly in the order they'd help. Each says what it
is, how it would work, and its size. When one is built, it leaves this
list (DESIGN.md says what was done).

## File-manager basics

3. **Recent files.** A "Recent" place: what you opened lately, from
   Emacs's recentf and other programs' `~/.local/share/recently-used.xbel`,
   newest first, shown as a list like a search's. *Small.*

4. **What's taking space.** Folder sizes, biggest first (like ncdu), to
   clean up from: a view of a folder's subfolders and files by size, made
   in the background, going into one to see its own. *Medium.*

5. **Duplicates.** Byte-identical copies under a folder (by size, then a
   hash), shown in groups, and a plan proposed to trash the extras (the
   oldest kept), for review like any plan. `sorting.md` already says exact
   duplicates may go. *Medium.*

## Fitting how Vid works

6. **Git status in ~/src.** In a repository, files marked as changed,
   new or ignored in the list (and the grid), and the branch with
   ahead/behind in the top line. *Medium.*

7. **Dropbox status.** A synced, syncing or not-synced mark beside files
   in the Dropbox folders (`dropbox filestatus`), and which folders are
   kept on this machine (selective sync). *Small to medium.*

8. **The journal as a panel.** Every applied plan, newest first, in words
   ("moved 3 files into ~/Work"), with undo for any one of them when the
   files allow, not only the last. *Medium.*

9. **Remote folders.** A server's folders over SSH like local ones
   (TRAMP shows them; plans, the Trash and undo would need to work there
   too). *Larger.*

## Parked

- **Dropping files on a workspace's number in the bar.** Needs XDND (drag
  and drop) in StumpWM's mode line. Open on Workspace (M-o) does it from a
  menu meanwhile.
- **More habits:** renames you keep making, files you keep trashing,
  folders you keep opening at a time of day.
