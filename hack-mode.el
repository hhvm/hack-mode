;;; hack-mode.el --- Major mode for the Hack programming language -*- lexical-binding: t -*-

;; Copyright (C) 2018  Facebook, Inc.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Author: John Allen <jallen@fb.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/hhvm/hack-mode

;;; Commentary:
;;
;; Implements `hack-mode' for the Hack programming language.  This
;; includes basic support for highlighting and indentation.
;;

;;; Code:
(require 'hack-xhp-indent)

(defgroup hack nil
  "Major mode `hack-mode' for editing Hack code."
  :prefix "hack-"
  :group 'languages)

(defcustom hack-client-program-name "hh_client"
  "The command to run to communicate with hh_server."
  :group 'hack-mode)

(defcustom hack-indent-offset 2
  "Indentation amount (in spaces) for Hack code."
  :group 'hack-mode)

(defvar hack-font-lock-keywords
  `(
    ;; <?hh must be on the first line. The only thing it may be preceded
    ;; by is a shebang. See hphp.ll.
    ;; TODO: handle //strict, //decl.
    (,(rx
       buffer-start
       "<?hh")
     . font-lock-keyword-face)
    ;; Keywords, based on hphp.ll.
    ;; TODO: what about ... and ?? tokens?
    (,(regexp-opt
       '("exit"
         "die"
         "const"
         "return"
         "yield"
         "from"
         "try"
         "catch"
         "finally"
         "throw"
         "using"
         "if"
         "else"
         "endif"
         "while"
         "endwhile"
         "do"
         "for"
         "endfor"
         "foreach"
         "endforeach"
         "declare"
         "enddeclare"
         "instanceof"
         "as"
         "super"
         "switch"
         "endswitch"
         "case"
         "default"
         "break"
         "continue"
         "goto"
         "echo"
         "print"
         "class"
         "interface"
         "trait"
         "insteadof"
         "extends"
         "implements"
         "enum"
         "attribute"
         "category"
         "children"
         "required"
         "function"
         "new"
         "clone"
         "var"
         "callable"
         "eval"
         "include"
         "include_once"
         "require"
         "require_once"
         "namespace"
         "use"
         "global"
         "isset"
         "empty"
         "__halt_compiler"
         "__compiler_halt_offset__"
         "static"
         "abstract"
         "final"
         "private"
         "protected"
         "public"
         "unset"
         "==>"
         "list"
         "array"
         "OR"
         "AND"
         "XOR"
         "dict"
         "keyset"
         "shape"
         "type"
         "newtype"
         "where"
         "await"
         "vec"
         "varray"
         "darray"
         "inout"
         "async"
         "tuple"
         "__CLASS__"
         "__TRAIT__"
         "__FUNCTION__"
         "__METHOD__"
         "__LINE__"
         "__FILE__"
         "__DIR__"
         "__NAMESPACE__"
         "__COMPILER_FRONTEND__")
       'symbols)
     . font-lock-keyword-face)
    ;; Type definitions.
    (,(rx
       (? "?")
       symbol-start
       (or
        ;; Built-in types, based on naming_special_names.ml.
        "void"
        "resource"
        "num"
        "arraykey"
        "noreturn"
        "mixed"
        "nonnull"
        "this"
        "dynamic"
        "int"
        "bool"
        "float"
        "string"
        "array"
        "darray"
        "varray"
        "varray_or_darray"
        "integer"
        "boolean"
        "double"
        "real"
        "callable"
        "object"
        "unset"
        ;; User-defined type.
        (seq
         (any upper)
         (* (or (syntax word) (syntax symbol)))))
       symbol-end)
     . font-lock-type-face)
    ;; We also highlight _ as a type, but don't highlight ?_.
    (,(regexp-opt '("_") 'symbols)
     . font-lock-type_face)
    (,(regexp-opt
       '("null"
         "true"
         "false")
       'symbols)
     . font-lock-constant-face)

    ;; $$ is a special variable used with the pipe operator
    ;; |>. Highlight the entire string, to avoid confusion with $s.
    (,(rx symbol-start "$$")
     . font-lock-variable-name-face)
    ;; Highlight variables that start with $, e.g. $foo. Don't
    ;; highlight the $, to make the name easier to read (consistent with php-mode).
    (,(rx symbol-start "$" (group (+ (or (syntax word) (syntax symbol)))) symbol-end)
     1 font-lock-variable-name-face)

    ;; Constants are all in upper case, and cannot start with a
    ;; digit. We use font-lock-variable-name-face for consistency with
    ;; c-mode.
    (,(rx symbol-start
          (any upper "_")
          (+ (any upper "_" digit))
          symbol-end)
     . font-lock-variable-name-face)

    ;; Highlight function names.
    (,(rx symbol-start
          "function"
          symbol-end
          (+ space)
          (group
           symbol-start
           (+ (or (syntax word) (syntax symbol)))
           symbol-end))
     1 font-lock-function-name-face)
    ;; FALLTHROUGH and UNSAFE comments, based on full_fidelity_lexer.ml.
    ;; TODO: UNSAFE_EXPR according to https://docs.hhvm.com/hack/typechecker/special
    (,(rx "//"
          (+ space)
          (or "FALLTHROUGH" "UNSAFE"))
     . font-lock-function-name-face)))

(defvar hack-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; - and < are not symbol constituents.
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)

    ;; Treat + as punctuation.
    (modify-syntax-entry ?+ "." table)

    ;; Treat \ as punctuation, so we can navigate between different
    ;; parts of a namespace reference.
    (modify-syntax-entry ?\\ "." table)

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?' "\"" table)
    ;; Comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\^m "> b" table)

    table))

;; hh_server can choke if you symlink your www root
(setq find-file-visit-truename t)

;; Ensure that we use `hack-mode' for .php files, but put the
;; association at the end of the list so `php-mode' wins (if installed).
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.php$" . hack-mode) t)

;; These extensions are hack-specific.
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hhi$" . hack-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hack$" . hack-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hck$" . hack-mode))

;;;###autoload
(define-derived-mode hack-mode prog-mode "Hack"
  "Major mode for editing Hack code.

\\{hack-mode-map\\}"
  (setq-local font-lock-defaults '(hack-font-lock-keywords))
  (setq-local compile-command (concat hack-client-program-name " --from emacs"))
  (setq-local indent-line-function #'hack-xhp-indent-line)
  (setq-local comment-start "// "))

(provide 'hack-mode)
;;; hack-mode.el ends here
