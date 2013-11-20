ztree
=====

Ztree is a project dedicated to implementation of several text-tree applications inside Emacs. It consists of 2 subprojects: **ztree-dir** and **ztree-diff**.

ztree-dir
---------
**ztree-dir** is a simple text-mode directory tree for Emacs. See screenshots below for the GUI and the terminal versions of the **ztree-dir**.

Add the following to your .emacs file:

```scheme
(push (substitute-in-file-name "path-to-ztree-directory") load-path)
(require 'ztree-dir)
```

Call the `ztree-dir` interactive function:

```
M-x ztree-dir
```

Open/close directories with double-click, `RET` or `Space` keys. To jump to the parent directory, hit the `Backspace` key.


![ztree emacsapp](https://github.com/fourier/ztree/raw/screenshots/screenshots/emacs_app.png "Emacs App with ztree-dir")

![ztree emacsx11](https://github.com/fourier/ztree/raw/screenshots/screenshots/emacs_xterm.png "Emacs in xterm with ztree-dir")

ztree-diff
==========
**ztree-diff** is a directory-diff tool for Emacs inspired by commercial tools like Beyond Compare or Araxis Merge. It supports showing the difference between two directories; copying between directories, calling **Ediff** for not matching files, hiding/showing equal files/directories.

The comparison itself performed with the external **GNU diff** tool, so make sure to have one in the executable path.

As above add the following to your .emacs file:

```scheme
(push (substitute-in-file-name "path-to-ztree-directory") load-path)
(require 'ztree-diff)
```

Call the `ztree-diff` interactive function:

```
M-x ztree-diff
```
Then you need to specify the left and right directories to compare.

###Hotkeys supported
The basic hotkeys are the same as in the **ztree-dir**. Additionally:
 * `RET` or `Space` on different files starts the **Ediff**
 * `TAB` to fast switch between panels
 * `h` key toggle show/hide identical files/directories
 * `C` key to copy current file or directory to the left or right panel
 * `F5` forces the full rescan.

![ztreediff emacsx11](https://github.com/fourier/ztree/raw/screenshots/screenshots/emacs_diff_xterm.png "Emacs in xterm with ztree-diff")
