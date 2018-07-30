# hack-mode
A major-mode for [ Hack ](https://hacklang.org/) code.

## Installation

Load this package into your elisp load-path.

```emacs-lisp
(require 'hack-mode)

;; Turn on all the cool LSP features!
(add-hook
  'hack-mode-hook
  (lambda ()
    (flycheck-mode t)
    (company-mode t))))
```
