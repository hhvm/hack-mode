# hack-mode
A major-mode for [ Hack ](https://hacklang.org/) code.

## Installation

Load this package into your elisp load-path.

```emacs-lisp
(require 'hack-mode)

;; Set up hack-mode for the relevant files
(add-to-list 'auto-mode-alist '("\\.php\\'" . hack-mode))
(add-to-list 'auto-mode-alist '("\\.hhi\\'" . hack-mode))

(add-hook
  'hack-mode-hook
  (lambda ()
    ;; Turn on all the cool LSP features!
    (lsp-hack-enable)
    (flycheck-mode t)
    (company-mode t))))
```
