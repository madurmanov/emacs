# Emacs

![Emacs](./screenshot.png)


## Contents

- [Auto save buffer](#auto-save-buffer)
- [Bookmarks](#bookmarks)
- [Desktop](#desktop)
- [Dimnish](#dimnish)
- [Evil](#evil)


### Auto save buffer

Set of functions which helps to auto save buffer when focus is lost.
Buffer auto saves by following events:
- switch to buffer
- other window
- windmove-up
- windmove-down
- windmove-left
- windmove-right


### Bookmarks

Set of keybindings for use build-in Emacs bookmarks.
Bookmarks auto save into `.emacs.bookmarks` in the [Emacs start directory](#emacs-start-directory) after exit.
If Emacs start directory contain `.emacs.bookmarks` saved before that he will load at start.

Keybindings:
```
[M-1] - List of bookmarks
[M-2] - Set bookmark
[M-3] - Delete bookmark
```


### Desktop

Save Emacs working state into `.emacs.desktop` in the [Emacs start directory](#emacs-start-directory) before exit.


### Diminish

Diminish make short minor mode's names abbreviations displays in the mode line.


### Evil

Evil mode it is port of the vim's keybinding layer. Evil used in the all modes.
Some modes ignoring evil mode. For make possible to use evil mode in the certain
modes need to add this mode into evil init file to special section marked as `force evil`.
Search in the evil mode configurated by `evil-search` and `evil-visualstart`.

Keybindings:
```
default search:
[/] - search entered text
[n] - next result
[N] - previous result

visualstar search:
[M-8] - search selected text
[M-*] - reset highlighted result
[*] - next result
[#] - previous result
```


## References


### Emacs start directory

Save into Emacs start directory needed to have opportunity work with different projects.


## License

**Emacs** is released under the MIT License. See the bundled `LICENSE.md` for details.
