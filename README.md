# hack-mode
A major-mode for [ Hack ](https://hacklang.org/) code.

## Installation

Load this package into your elisp load-path.

```emacs-lisp
(require 'hack-mode)

; hh_server can choke if you are using symlinks
(setq find-file-visit-truename t)
```
