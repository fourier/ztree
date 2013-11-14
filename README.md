ztree
=====

Simple text-mode directory tree for Emacs. See screenshots below for GUI and the terminal versions of the zterm.

![ztree emacsapp](https://github.com/fourier/ztree/raw/screenshots/screenshots/emacs_app.png "Emacs App with Ztree")

![ztree emacsx11](https://github.com/fourier/ztree/raw/screenshots/screenshots/emacs_xterm.png "Emacs in xterm with Ztree")

Add the following to your .emacs file:

```scheme
(push (substitute-in-file-name "path-to-ztree-directory") load-path)
(require 'ztree)
```

Call the `ztree` interactive function:

```
M-x ztree
```

Open/close directories with double-click, `RET` or `Space` keys. To jump to the parent directory, hit the `Backspace` key.
