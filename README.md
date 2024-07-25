# helm-bm.el [![licence][gplv3-badge]][gplv3-link] [![melpa badge][melpa-badge]][melpa-link] [![melpa stable badge][melpa-stable-badge]][melpa-stable-link]

[helm] sources for [bm.el][bm].

## Requirements

- [helm]
- [bm]
- [cl-lib]

## Installation

You can install `helm-bm.el` from the [MELPA](http://melpa.milkbox.net/) repository with
#### <kbd>M-x</kbd> `package-install`.

Or add this file in your `load-path` and the following to your Emacs init file:

    (autoload 'helm-bm "helm-bm" nil t) ;; Not necessary if using ELPA package.

## Configuration

Bind helm-bm to a key of your choice, e.g.

    (global-set-key (kbd "C-c b") 'helm-bm)

Show bookmarks from all buffers or only from current buffer according
to `bm-cycle-all-buffers` value.


## Basic usage

#### Bookmark some places with <kbd>M-x</kbd> `bm-toggle`

Then

#### <kbd>M-x</kbd> `helm-bm`

Show bookmarks of [bm].el with `helm`.


[helm]:https://github.com/emacs-helm/helm
[bm]:https://github.com/joodland/bm
[cl-lib]:http://elpa.gnu.org/packages/cl-lib.html
[travis-badge]: https://travis-ci.org/emacs-helm/helm-bm.svg
[travis-link]: https://travis-ci.org/emacs-helm/helm-bm
[melpa-link]: http://melpa.org/#/helm-bm
[melpa-stable-link]: http://stable.melpa.org/#/helm-bm
[melpa-badge]: http://melpa.org/packages/helm-bm-badge.svg
[melpa-stable-badge]: http://stable.melpa.org/packages/helm-bm-badge.svg
[gplv3-badge]:http://img.shields.io/badge/license-GPLv3-blue.svg
[gplv3-link]:https://www.gnu.org/copyleft/gpl.html
