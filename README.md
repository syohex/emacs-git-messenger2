# git-messenger.el

`git-messenger2.el` is Emacs port of [git-messenger.vim](https://github.com/rhysd/git-messenger.vim).

`git-messenger2.el` provides function that popup commit message at current line.
This is useful when you want to know why this line was changed.


## Screenshot

![Screenshot of git-messenger.el](image/git-messenger.png)


## Dependency

* [popup](https://github.com/auto-complete/popup-el)


## Supported VCS

- Git
- Subversion
- Mercurial


## Commands

### `git-messenger2-popup-message`

Pop up last commit message at current line. Show detail message, Commit ID, Author,
Date and commit message with `C-u` prefix

![Screenshot of git-messenger with prefix argument](image/git-messenger-detail.png)


## Key Bindings

You can modify key bindings by customizing `git-messenger2-map`.

| Key                  | Command                                                 |
|:--------------------:|:--------------------------------------------------------|
| `M-w`                | Copy commit message and quit                            |
| `c`                  | Copy commit ID and quit                                 |
| `d`                  | Pop up `git diff` of last change of this line           |
| `s`                  | Pop up `git show --stat` of last change of this line    |
| `S`                  | Pop up `git show --stat -p` of last change of this line |
| `q`                  | Quit                                                    |


## Customize

### `git-messenger2-show-detail`(Default `nil`)

Always show detail message if this value is `t`.

### `git-messenger2-handled-backends`(Default `'(git svn)`)

Handled VCS which `git-messenger` uses.
Entries in this list will be tried in order to determine whether a
file is under that sort of version control.

## Hooks

### `git-messenger2-before-popup-hook`

Run before popup commit message. Hook function take one argument, commit message.

### `git-messenger2-after-popup-hook`

Run after popup commit message. Hook function take one argument, commit message.

### `git-messenger2-popup-buffer-hook`

Run after popup buffer.


## Global Variables

You may be able to use these variables useful in commands of `git-messenger2-map`.

#### `git-messenger2-last-message`

Last popup-ed commit message

#### `git-messenger2-last-commit-id`

Last popup-ed commit ID


## Sample Configuration

```lisp
(require 'git-messenger2) ;; You need not to load if you install with package.el
(global-set-key (kbd "C-x v p") 'git-messenger2-popup-message)

(define-key git-messenger2-map (kbd "m") 'git-messenger2-copy-message)
```
