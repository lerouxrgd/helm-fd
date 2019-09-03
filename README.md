# helm-fd

[![MELPA](https://melpa.org/packages/helm-fd-badge.svg)](https://melpa.org/#/helm-fd)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

Same as [helm-find][] but using [fd][] instead of `find`, therefore
you have to install it beforehand. `fd` is written in Rust and is much
faster than `find`.

The main function is `helm-fd` and mimics `helm-find`. A project
scoped search function `helm-fd-project` is also provided. By default,
`fd` will ignore hidden directories/files and patterns from
`.gitignore` file when present.

## Setup

``` emacs-lisp
(use-package helm-fd
  :bind (:map helm-command-map
              ("/" . helm-fd)))
```

[helm-find]: https://github.com/emacs-helm/helm/blob/master/helm-find.el
[fd]: https://github.com/sharkdp/fd
