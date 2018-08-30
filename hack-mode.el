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
(require 'font-lock)
(require 'cc-mode)
(require 'cc-langs)
(eval-when-compile
  (require 'regexp-opt))

(defgroup hack nil
  "Major mode `hack-mode' for editing Hack code."
  :prefix "hack-"
  :group 'languages)

(defcustom hack-client-program-name "hh_client"
  "The command to run to communicate with hh_server."
  :group 'hack-mode)

(eval-and-compile
  (c-add-language 'hack-mode 'c-mode))

(c-lang-defconst c-recognize-post-brace-list-type-p hack t)
(c-lang-defconst c-recognize-colon-labels hack t)
(c-lang-defconst c-recognize-knr-p hack t)
(c-lang-defconst qc-recognize-typeless-decls hack t)
(c-lang-defconst c-recognize-<>-arglists hack t)

(c-lang-defconst c-<>-notable-chars-re hack "[<*[];{},|&>)]")

;; preprocessor stuff from c++ we turn off
(c-lang-defconst c-opt-cpp-symbol hack nil)
(c-lang-defconst c-opt-cpp-prefix hack nil)
(c-lang-defconst c-anchored-cpp-prefix hack nil)
(c-lang-defconst c-cpp-message-directives hack nil)
(c-lang-defconst c-cpp-include-directives hack nil)
(c-lang-defconst c-cpp-expr-directives hack nil)
(c-lang-defconst c-cpp-expr-functions hack nil)

;; Don't look for preprocessor fontification, like C does
(c-lang-defconst c-before-font-lock-functions
  hack 'c-change-expand-fl-region)

;; I think...
;; (c-lang-defconst c-string-escaped-newlines hack t)
;; (c-lang-defconst comment-end-can-be-escaped hack t)
(c-lang-defconst c-multiline-string-start-char hack t)

(c-lang-defconst c-type-prefix-kwds
  hack '("instanceof"))

(c-lang-defconst c-decl-hangon-kwds hack nil)

(c-lang-defconst c-opt-cpp-prefix
  hack "<\\?\\(?:php\\|hh\\)\\s-*?")
(c-lang-defconst c-opt-cpp-start
  hack "<\\?\\(?:php\\|hh\\)\\s-*?")

;; TODO, special fontification
;; Hack treats comments of the forms // strict and // FALLTHROUGH in a special way.

(c-lang-defconst c-basic-matchers-before
  hack
  `(;; Fontify keyword constants.
    ,@(when (c-lang-const c-constant-kwds)
        (let ((re (c-make-keywords-re nil (c-lang-const c-constant-kwds))))
          `((eval . (list ,(concat "\\<\\(" re "\\)\\>")
                          1 c-constant-face-name)))))

    ;; Fontify leading identifiers in fully qualified names like
    ;; "foo::bar" in languages that supports such things.
    ,@(when (c-lang-const c-opt-identifier-concat-key)
        `((,(byte-compile
             ;; Must use a function here since we match longer than
             ;; we want to move before doing a new search.  This is
             ;; not necessary for XEmacs since it restarts the
             ;; search from the end of the first highlighted
             ;; submatch (something that causes problems in other
             ;; places).
             `(lambda (limit)
                (while (re-search-forward
                        ,(concat "\\(\\<" ; 1
                                 "\\(" (c-lang-const c-symbol-key) "\\)" ; 2
                                 (c-lang-const c-simple-ws) "*"
                                 (c-lang-const c-opt-identifier-concat-key)
                                 (c-lang-const c-simple-ws) "*"
                                 "\\)"
                                 "\\("
                                 (c-lang-const c-opt-after-id-concat-key)
                                 "\\)")
                        limit t)
                  (unless (progn
                            (goto-char (match-beginning 0))
                            (c-skip-comments-and-strings limit))
                    (or (get-text-property (match-beginning 2) 'face)
                        (c-put-font-lock-face (match-beginning 2)
                                              (match-end 2)
                                              c-reference-face-name))
                    (goto-char (match-end 1)))))))))

    ;; Fontify all keywords except the primitive types.
    (,(concat "\\<" (c-lang-const c-regular-keywords-regexp))
     1 font-lock-keyword-face)

    ;; TODO, fontify ~ or other things
    ;; fontify bangs!
    (eval . (list "\\(!\\)[^=]" 1 c-negation-char-face-name))
    ))

(c-lang-defconst c-simple-decl-matchers
  "Simple font lock matchers for types and declarations.  These are used
on level 2 only and so aren't combined with `c-complex-decl-matchers'."

  hack `(
      ;; Fontify all type names and the identifiers in the
      ;; declarations they might start.  Use eval here since
      ;; `c-known-type-key' gets its value from
      ;; `*-font-lock-extra-types' on mode init.
      (eval . (list ,(c-make-font-lock-search-function
                      'c-known-type-key
                      '(1 'font-lock-type-face t)
                      '((c-font-lock-declarators limit t nil)
                        (save-match-data
                          (goto-char (match-end 1))
                          (c-forward-syntactic-ws))
                        (goto-char (match-end 1))))))

      ;; Fontify types preceded by `c-type-prefix-kwds' and the
      ;; identifiers in the declarations they might start.
      ,@(when (c-lang-const c-type-prefix-kwds)
          (let* ((prefix-re (c-make-keywords-re nil
                              (c-lang-const c-type-prefix-kwds)))
                 (type-match (+ 2
                                (regexp-opt-depth prefix-re)
                                (c-lang-const c-simple-ws-depth))))
            `((,(c-make-font-lock-search-function
                 (concat "\\<\\(" prefix-re "\\)" ; 1
                         (c-lang-const c-simple-ws) "+"
                         (concat "\\("	; 2 + prefix-re + c-simple-ws
                                 (c-lang-const c-symbol-key)
                                 "\\)"))
                 `(,type-match
                   'font-lock-type-face t)
                 `((c-font-lock-declarators limit t nil)
                   (save-match-data
                     (goto-char (match-end ,type-match))
                     (c-forward-syntactic-ws))
                   (goto-char (match-end ,type-match))))))))

      ;; Fontify special declarations that lacks a type.
      ,@(when (c-lang-const c-typeless-decl-kwds)
          `((,(c-make-font-lock-search-function
               (concat "\\<\\("
                       (regexp-opt (c-lang-const c-typeless-decl-kwds))
                       "\\)\\>")
               '((c-font-lock-declarators limit t nil)
                 (save-match-data
                   (goto-char (match-end 1))
                   (c-forward-syntactic-ws))
                 (goto-char (match-end 1)))))))

      ;; Fontify generic colon labels in languages that support them.
      ,@(when (c-lang-const c-recognize-colon-labels)
          `(c-font-lock-labels))))

(c-lang-defconst c-basic-matchers-after

  hack `(
         ;; Really super simple variable highlighting
         ("\\$\\(this\\)" 1 c-reference-face-name)
         (eval . (list ,(concat "\\$\\(" (c-lang-const c-symbol-key) "\\)") 1 font-lock-variable-name-face))

         ;; hack enums are separated by ; not by ,
         ;; this seems be be the problem as we are relying on
         ;; (c-font-lock-declarators limit t nil)

         ;; ,@(when (c-lang-const c-brace-id-list-kwds)
         ;;     ;; Fontify the remaining identifiers inside an enum list when we start
         ;;     ;; inside it.
         ;;     `(hack-font-lock-enum-tail
         ;;       ;; Fontify the identifiers inside enum lists.  (The enum type
         ;;       ;; name is handled by `c-simple-decl-matchers' or
         ;;       ;; `c-complex-decl-matchers' below.
         ;;       (,(c-make-font-lock-search-function
         ;;          (concat
         ;;           "\\<\\("
         ;;           (c-make-keywords-re nil (c-lang-const c-brace-id-list-kwds))
         ;;           "\\)\\>"
         ;;           ;; Disallow various common punctuation chars that can't come
         ;;           ;; before the '{' of the enum list, to avoid searching too far.
         ;;           "[^][{}();/#=]*"
         ;;           "{")
         ;;          '((c-font-lock-declarators limit t nil)
         ;;            (save-match-data
         ;;              (goto-char (match-end 0))
         ;;              (c-put-char-property (1- (point)) 'c-type
         ;;                                   'c-decl-id-start)
         ;;              (c-forward-syntactic-ws))
         ;;            (goto-char (match-end 0)))))))

         ;; Fontify the clauses after various keywords.
         ,@(when (or (c-lang-const c-type-list-kwds)
                     (c-lang-const c-ref-list-kwds)
                     (c-lang-const c-colon-type-list-kwds))
             `((,(c-make-font-lock-BO-decl-search-function
                  (concat "\\<\\("
                          (c-make-keywords-re nil
                            (append (c-lang-const c-type-list-kwds)
                                    (c-lang-const c-ref-list-kwds)
                                    (c-lang-const c-colon-type-list-kwds)))
                          "\\)\\>")
                  '((c-fontify-types-and-refs ((c-promote-possible-types t))
                      (c-forward-keyword-clause 1)
                      (if (> (point) limit) (goto-char limit))))))))

         ))

(defun hack-font-lock-enum-tail (limit)
  "Fontify an enum's identifiers when POINT is within the enum's brace block.

  This function will be called from font-lock for a region bounded by POINT
  and LIMIT, as though it were to identify a keyword for
  ‘font-lock-keyword-face’.  It always returns NIL to inhibit this and
  prevent a repeat invocation.  See elisp/lispref page 'Search-based
  Fontification'.

  NOTE: this is modified from cc-fonts to support that entries in
  Hack enums are separated by ; rather than ,"
  (let* ((paren-state (c-parse-state))
         (encl-pos (c-most-enclosing-brace paren-state)))
    (when (and
           encl-pos
           (eq (char-after encl-pos) ?\{)
           (save-excursion
             (goto-char encl-pos)
             (c-backward-over-enum-header)))
      (c-syntactic-skip-backward "^{;" nil t)
      (c-put-char-property (1- (point)) 'c-type 'c-decl-id-start)

      (c-forward-syntactic-ws)

      (c-font-lock-declarators limit t nil)))
  nil)

(c-lang-defconst c-operators
  hack '(
         (prefix "~")
         (prefix "...")
         (right-assoc "::")

         ;; (right-assoc "=>")

         (postfix-if-paren "<" ">")
         (left-assoc "->" "?->" "->:" "|>" "==>")
         (postfix "++" "--" "[" "]" "(" ")")

         (prefix "++" "--" "+" "-" "!" "~" "?"
                 "new" "clone"
                 "(" ")" ;; Cast
                 )

         (left-assoc "*" "/" "%")
         (left-assoc "+" "-" ".")

         (left-assoc "<<" ">>")

         (left-assoc "<" "<=" ">" ">=" "instanceof" "as")
         (left-assoc "==" "!=" "|===" "!==")

         (left-assoc "&")
         (left-assoc "^")
         (left-assoc "|")

         (left-assoc "&&")
         (left-assoc "||")

         (right-assoc "$$") ;; putting this in here, but not sure about it

         (right-assoc "??")
         (left-assoc "?:")

         (right-assoc "=" "+=" "-=" "*=" "**=" "/=" ".=" "%=" "&=" "|=" "^=" "<<=" ">>=")

         (left-assoc "and")
         (left-assoc "xor")
         (left-assoc "or")

         (prefix "throw")
         (left-assoc ","))
  )

(c-lang-defconst c-identifier-ops
  hack '((prefix "$") ;; the all-mighty dollar
         (prefix "?") ;; nullable
         (prefix "+") ;; in class decl <>
         (left-assoc "::") ;; classname joiner
         (left-assoc "\\") ;; namespace joiner
         )
  )

(c-lang-defconst c-type-modifier-kwds hack nil)
(c-lang-defconst c-primitive-type-kwds
  ;; https://docs.hhvm.com/hack/types/type-system
  ;; https://github.com/jra3/hack-langspec/blob/master/spec/05-types.md
  hack '("arraykey" "bool" "float" "int" "num" "resource"
         "string" "this" "void"

         "mixed"

         ;; not primitive, but pretty close ;)
         "array" "vec" "dict" "keyset"
         )
  )

(c-lang-defconst c-primitive-type-prefix-kwds hack nil)

(c-lang-defconst c-brace-list-decl-kwds
  hack '("enum"

         ;; Weird syntax on these
         "Vector" "ImmVector"
         "Map" "ImmMap"
         "Set" "ImmSet"
         ))

(c-lang-defconst c-typedef-kwds
  ;; https://github.com/jra3/hack-langspec/blob/master/spec/05-type.md
  hack '("type" "newtype"))

(c-lang-defconst c-class-decl-kwds
  hack '("class" "interface" "trait"))

(c-lang-defconst c-other-block-decl-kwds
  hack '(;; "function"
         "namespace"))

(c-lang-defconst c-modifier-kwds
  hack '("abstract" "static" "const" "global" "final" "async"
         ;; XHP stuff... not sure if there is a better option
         "attribute" "children"))

(c-lang-defconst c-protection-kwds
  hack '("public" "private" "protected"))

(c-lang-defconst c-postfix-decl-spec-kwds
  hack '("extends" "implements"))

(c-lang-defconst c-type-list-kwds
  hack '("implements" "use" "require" "instanceof"))

(c-lang-defconst c-ref-list-kwds
  hack '("namespace"))  ;; may be unecessary

(c-lang-defconst c-colon-type-list-re hack "[^][{};,/#=:]*:")
(c-lang-defconst c-colon-type-list-kwds
  hack '("enum" "function"))

(c-lang-defconst c-paren-nontype-kwds
  hack '("exit" "die" "self"))

(c-lang-defconst c-block-stmt-1-kwds
  ;; Statement keywords followed directly by a substatement.
  hack '("do" "else" "finally" "try"))

(c-lang-defconst c-block-stmt-2-kwds
  ;; Statement keywords followed by a paren sexp and then by a substatement.
  hack '("for" "if" "elseif" "while" "switch" "foreach" "catch"))

(c-lang-defconst c-simple-stmt-kwds
  hack '("break" "continue" "return" "yield" "namespace" "echo"
         "include" "include_once"
         "require" "require_once"
         "await"
         ))

(c-lang-defconst c-<>-type-kwds
  hack '( ;; new hotness
         "vec" "keyset" "dict"

         ;; old school
         "Vector" "ImmVector"
         "Map" "ImmMap"
         "Set" "ImmSet"

         ;; dead school
         "array"))

(c-lang-defconst c-label-kwds
  ;; Keywords introducing colon terminated labels in blocks.
  hack '("case" "default"))


;; Hack does not support goto, continue n, or break n.
(c-lang-defconst c-before-label-kwds hack '())

(c-lang-defconst c-constant-kwds
  ;; Keywords for constants.
  hack '("null" "true" "false"
         "STDIN" "STDOUT" "STDERR"

         ;; https://github.com/jra3/hack-langspec/blob/master/spec/06-constants.md#context-dependent-constants
         "__CLASS__" "__DIR__" "__FILE__" "__FUNCTION__"
         "__LINE__" "__METHOD__" "__NAMESPACE__" "__TRAIT__"

         ;; https://github.com/jra3/hack-langspec/blob/master/spec/06-constants.md#core-predefined-constants
         "E_ALL" "E_COMPILE_ERROR" "E_COMPILE_WARNING" "E_CORE_ERROR"
         "E_CORE_WARNING" "E_DEPRECATED" "E_ERROR" "E_NOTICE" "E_PARSE"
         "E_RECOVERABLE_ERROR" "E_STRICT" "E_USER_DEPRECATED"
         "E_USER_ERROR" "E_USER_NOTICE" "E_USER_WARNING" "E_WARNING"
         "E_USER_DEPRECATED" "INF" "M_E" "M_PI" "NAN" "PHP_EOL"
         "PHP_INT_MAX" "PHP_INT_MIN" "PHP_INT_SIZE"))

(c-lang-defconst c-inexpr-block-kwds
  hack '("function" ;; Classic PHP anonymous functions
         "async" ;; async blocks

         "Vector" "ImmVector"
         "Map" "ImmMap"
         "Set" "ImmSet"
         ))

(c-lang-defconst c-other-keywords
  ;; https://github.com/hhvm/hack-langspec/blob/master/spec/09-lexical-structure.md#keywords
  hack '("await" "classname" "insteadof" "noreturn"
         "parent" "self" "static" "shape" "using"
         "echo" "tuple" "list" "empty" "isset" "unset"
         "fun" "inst_meth" "inst_meth" "meth_caller"
         ))

;; Fix indentation for enums
(c-lang-defconst c-opt-block-decls-with-vars-key hack nil)

;; TODO
;; (defvar c-promote-possible-types nil)

(defcustom hack-font-lock-extra-types
  '(
    "__Override"
    "__ConsistentConstruct"
    "__Memoize"
    "__Deprecated"
    "__MockClass"
    "__IsFoldable"
    "__Native"
    )
  "*List of extra types (aside from the type keywords) in hack mode.
Each list item should be a regexp matching a single identifier."
  :group 'hack-mode
  :type 'list
  )

(c-lang-defconst c-font-lock-extra-types
  hack hack-font-lock-extra-types)

(c-lang-defconst c-matchers-2
  hack (append
        (c-lang-const c-matchers-1)
        (c-lang-const c-basic-matchers-before)
        (c-lang-const c-simple-decl-matchers)
        (c-lang-const c-basic-matchers-after)
        ))

(c-lang-defconst c-matchers-3
  hack (append
        (c-lang-const c-matchers-1)
        (c-lang-const c-basic-matchers-before)
        (c-lang-const c-complex-decl-matchers)
        (c-lang-const c-basic-matchers-after)
        ))

(defconst hack-font-lock-keywords-1 (c-lang-const c-matchers-1 hack)
  "Minimal highlighting for hack mode.")

(defconst hack-font-lock-keywords-2 (c-lang-const c-matchers-2 hack)
  "Fast normal highlighting for hack mode.")

(defconst hack-font-lock-keywords-3 (c-lang-const c-matchers-2 hack)
  "Accurate normal highlighting for hack mode.")

(defvar hack-font-lock-keywords hack-font-lock-keywords-2
  "Default expressions to highlight in hack mode.")

(defvar hack-mode-syntax-table nil
  "Syntax table used in ‘hack-mode’ buffers.")
(or hack-mode-syntax-table
    (setq hack-mode-syntax-table
          (let ((table (funcall (c-lang-const c-make-mode-syntax-table hack))))

            ;; '#' style comments
            (modify-syntax-entry ?# "< b" table)

            ;; '/* ... */' and '//' style comments
            (modify-syntax-entry ?/ ". 124b" table)
            (modify-syntax-entry ?* ". 23" table)

            ;; newlines end '//' and '#' comments
            (modify-syntax-entry ?\n "> b" table)

            ;; $ and type prefixed to be treated as a 'Character quote'
            (modify-syntax-entry ?$ "/" table)
            ;; (modify-syntax-entry ?? "/" table)
            ;; (modify-syntax-entry ?+ "/" table)

            ;; Single quotes and backticks highlighted as strings
            (modify-syntax-entry ?' "\"" table)
            (modify-syntax-entry ?` "\"" table)

            ;; Give underscore "word" syntax (todo, should be symbol?)
            (modify-syntax-entry ?_ "w" table)

            ;; ;; Horrible hack to keep annotations from screwing up
            ;; ;; indentation of method decls and classes
            ;; (modify-syntax-entry ?< "_" table)
            ;; (modify-syntax-entry ?> "_" table)

            ;; xhp symbol syntax
            ;; (modify-syntax-entry ?- "_" table)
            ;; (modify-syntax-entry ?: "_" table)

            table)))

(defvar hack-mode-abbrev-table nil
  "Abbreviation table used in ‘hack-mode’ buffers.")
(c-define-abbrev-table 'hack-mode-abbrev-table
  ;; Use the abbrevs table to trigger indentation actions
  ;; on keywords that, if they occur first on a line, might alter the
  ;; syntactic context.
  ;; Syntax for abbrevs is:
  ;; ( pattern replacement command initial-count)
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)
    ("finally" "finally" c-electric-continued-statement 0)))

(defvar hack-mode-map ()
  "Keymap used in ‘hack-mode’ buffers.")
(if hack-mode-map
    nil
  (setq hack-mode-map (c-make-inherited-keymap))

  ;; Workaround to make our custom xhp indentation work with c-mode
  (substitute-key-definition
   'c-indent-line-or-region
   'indent-for-tab-command
   hack-mode-map)
  )

(defun hack-lineup-arglist-intro (langelem)
  "Indent code at the beginning of function arglists.
Argument LANGELEM the location of the start of the arglist"
  (save-excursion
    (goto-char (cdr langelem))
    (vector (+ (current-column) c-basic-offset))))

(defun hack-lineup-arglist-cont (langelem)
  "Indent chained function calls inside of arg lists
Argument LANGELEM the location of the start of the arglist"
  (save-excursion
    (back-to-indentation)  ;; first non-whitespace char
    (let ((fn-cont (looking-at "->")))
      (goto-char (cdr langelem))
      (if fn-cont
          (vector (+ (current-column) c-basic-offset))
        (vector (current-column))))))

(defun hack-lineup-arglist-close (langelem)
  "Indent code at the close of function arglists.
Argument LANGELEM the location of the close of the arglist"
  (save-excursion
    (goto-char (cdr langelem))
    (vector (current-column))))

(defconst hack-style
  '((c-basic-offset . 2)
    (c-offsets-alist . ((stream-op . 0) ;; << and >> are not stream ops...
                        (access-label . 0)
                        (arglist-intro . hack-lineup-arglist-intro)
                        (arglist-cont . hack-lineup-arglist-cont)
                        (arglist-close . hack-lineup-arglist-close)
                        )))
  "Default Hack Programming style.")
(c-add-style "hack" hack-style)

;; hh_server can choke if you symlink your www root
(setq find-file-visit-truename t)

;;;###autoload
(define-derived-mode hack-mode c-mode "Hack"
  "A major mode for Hack files\n\n\\{hack-mode-map}"
  (c-initialize-cc-mode t)
  (c-init-language-vars hack-mode)
  (c-common-init 'hack-mode)
  (run-hooks 'c-mode-common-hook)

  (c-set-style "hack")
  (setq-local font-lock-maximum-decoration t)
  (setq-local case-fold-search t)
  (setq-local compile-command (concat hack-client-program-name " --from emacs"))
  (setq-local indent-line-function #'hack-xhp-indent-line)

  (run-hooks 'hack-mode-hook)
  (c-update-modeline)
  )

(provide 'hack-mode)
;;; hack-mode.el ends here
