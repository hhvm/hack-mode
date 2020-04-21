;; Copyright (c) Facebook, Inc. and its affiliates. All Rights Reserved.

(require 'ert)
(require 'hack-mode)

(defun hack--search-up-to (char)
  "Search forward for the next occurrence of CHAR, and put point before it."
  (search-forward char)
  (backward-char))

(ert-deftest hack-fill-paragraph-comment ()
  "Comments with // should be filled correctly."
  (with-temp-buffer
    (hack-mode)
    (insert "// foo bar baaaaaaaz foo bar baaaaaaaz foo bar baaaaaaaz foo bar baaaaaaaz foo bar baaaaaaaz foo bar baaaaaaaz")
    (goto-char (point-min))

    (fill-paragraph)
    (should
     (equal
      (buffer-string)
      "// foo bar baaaaaaaz foo bar baaaaaaaz foo bar baaaaaaaz foo bar
// baaaaaaaz foo bar baaaaaaaz foo bar baaaaaaaz"))))

(ert-deftest hack-fill-paragraph-docblock ()
  "Comments with /* should be filled correctly."
  (with-temp-buffer
    (hack-mode)
    (insert "/**\n * Thing Thing Thing Thing Thing Thing Thing Thing Thing Thing Thing Thing\n *\n * This runs very quickly\n */")
    (goto-char (point-min))
    (forward-line)

    (fill-paragraph)
    (should
     (equal
      (buffer-string)
      "/**
 * Thing Thing Thing Thing Thing Thing Thing Thing Thing Thing Thing
 * Thing
 *
 * This runs very quickly
 */" ))))

(ert-deftest hack-indent-parens ()
  "Ensure we indent simple hack code correctly."
  (let ((src "<?hh

function foo(): int {
  if (true) {
    return 1;
  } else {
    return 2;
  }
}
"))
    (with-temp-buffer
      (hack-mode)
      (insert src)

      (indent-region (point-min) (point-max))
      (should (string= (buffer-string) src)))))

(ert-deftest hack-indent-repeated-parens ()
  "Repeated parens on the same line should not increase the indent"
  (let ((src "foo(foo(\n  \"baz\"\n));"))
    (with-temp-buffer
      (hack-mode)
      (insert src)

      (indent-region (point-min) (point-max))
      (should (string= (buffer-string) src)))))

(ert-deftest hack-indent-generic ()
  "Generic brackets should be indented like other parens."
  (let ((src "function f<
  T1,
  T2,
>(): void {
  echo \"\";
  echo \"\";
}"))
    (with-temp-buffer
      (hack-mode)
      (insert src)

      (indent-region (point-min) (point-max))
      (should (string= (buffer-string) src)))))

(ert-deftest hack-indent-vec ()
  "Generic brackets should be indented like other parens."
  (let ((src "$x = vec[
  1,
  2,
];"))
    (with-temp-buffer
      (hack-mode)
      (insert src)

      (indent-region (point-min) (point-max))
      (should (string= (buffer-string) src)))))

(ert-deftest hack-indent-operator ()
  "After an infix operator, we should increase the indent."
  (let ((src "<?hh

$foo =
  bar();
"))
    (with-temp-buffer
      (hack-mode)
      (insert src)

      (indent-region (point-min) (point-max))
      (should (string= (buffer-string) src)))))

(ert-deftest hack-indent-comparison ()
  "Don't confuse <= with a paren that we should indent."
  (let ((src "<?hh

function f(int $n): bool {
  return $n <= 0;
}"))
    (with-temp-buffer
      (hack-mode)
      (insert src)

      (indent-region (point-min) (point-max))
      (should (string= (buffer-string) src)))))

(ert-deftest hack-indent-method ()
  "Methods on a separate line should be indented."
  (let ((src "<?hh

$foo = bar()
  ->baz();
"))
    (with-temp-buffer
      (hack-mode)
      (insert src)

      (indent-region (point-min) (point-max))
      (should (string= (buffer-string) src)))))

(ert-deftest hack-indent-heredoc ()
  "Don't change the indentation inside a heredoc literal."
  (let ((src "<?hh

$foo = <<<EOT
bar
          baz
EOT;
"))
    (with-temp-buffer
      (hack-mode)
      (insert src)

      (indent-region (point-min) (point-max))
      (should (string= (buffer-string) src)))))

(ert-deftest hack-indent-class-attribute ()
  (let ((src "<?hh

class MyClass {
  <<__Override>>
  public async function foo(): Awaitable<vec<Thing>> {
    if (true) {
      return null;
    }
  }
}
"))
    (with-temp-buffer
      (hack-mode)
      (insert src)

      (indent-region (point-min) (point-max))
      (should (string= (buffer-string) src)))))
(ert-deftest hack-indent-left-shift ()
  (let ((src "<?hh

$foo = 1 << 2;
bar();
"))
    (with-temp-buffer
      (hack-mode)
      (insert src)

      (indent-region (point-min) (point-max))
      (should (string= (buffer-string) src)))))

(ert-deftest hack-indent-nullable-attribute ()
  (let ((src "<?hh

$foo = bar()
  ?->baz;
"))
    (with-temp-buffer
      (hack-mode)
      (insert src)

      (indent-region (point-min) (point-max))
      (should (string= (buffer-string) src)))))

(ert-deftest hack-indent-pipe ()
  (let ((src "<?hh

$foo
  |> baz($$);
"))
    (with-temp-buffer
      (hack-mode)
      (insert src)

      (indent-region (point-min) (point-max))
      (should (string= (buffer-string) src)))))

(ert-deftest hack-indent-case ()
  (let ((src "switch ($foo) {
  case 1:
    bar();
    bar();
  default:
    bar();
}"))
    (with-temp-buffer
      (hack-mode)
      (insert src)

      (indent-region (point-min) (point-max))
      (should (string= (buffer-string) src)))))

(ert-deftest hack-indent-trailing-comment ()
  (let ((src "function foo(): void { // stuff
  bar();
}"))
    (with-temp-buffer
      (hack-mode)
      (insert src)

      (indent-region (point-min) (point-max))
      (should (string= (buffer-string) src)))))

(ert-deftest hack-indent-xhp ()
  "Ensure we indent XHP expressions correctly."
  (let ((src "<?hh

$x = <div>
  <p>hello world</p>
</div>;
"))
    (with-temp-buffer
      (hack-mode)
      (insert src)

      (indent-region (point-min) (point-max))
      (should (string= (buffer-string) src)))))

(ert-deftest hack-indent-after-xhp ()
  "Ensure we indent expressions after XHP correctly"
  (let ((src "<?hh

function stuff(): int {
  $x = <p>hello</p>;
}"))
    (with-temp-buffer
      (hack-mode)
      (insert src)

      (indent-region (point-min) (point-max))
      (should (string= (buffer-string) src)))))

(ert-deftest hack-indent-xhp-in-comment ()
  "XHP expressions in comments should not affect indentation."
  (let ((src "function foo(): void {\n  $x = 123;\n  // return <p>H'el'lo</p>;\n  1;\n}"))
    (with-temp-buffer
      (hack-mode)
      (insert src)

      (indent-region (point-min) (point-max))
      (should (string= (buffer-string) src)))))

(defmacro with-hack-buffer (src &rest body)
  "Insert SRC in a temporary `hack-mode' buffer, apply syntax highlighting,
then run BODY."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,src)
     (goto-char (point-min))
     ;; Activate hack-mode, but don't run any hooks. This doesn't
     ;; matter on Travis, but is defensive when running tests in the
     ;; current Emacs instance.
     (delay-mode-hooks (hack-mode))
     ;; Ensure we've syntax-highlighted the whole buffer.
     (if (fboundp 'font-lock-ensure)
         (font-lock-ensure)
       (with-no-warnings
         (font-lock-fontify-buffer)))
     ,@body))

(ert-deftest hack-syntax-angle-bracket-for-type ()
  "Match angle brackets in type parameters."
  (with-hack-buffer "function foo(vec<int> $_): void {}"
    (hack--search-up-to ">")
    (should
     (eq (syntax-class (syntax-after (point)))
         5))))

(ert-deftest hack-syntax-angle-bracket-method ()
  "Method access is not a matched pair."
  (with-hack-buffer "$foo->bar();"
    (hack--search-up-to ">")
    (should
     (eq (syntax-class (syntax-after (point)))
         1))))

(ert-deftest hack-syntax-angle-bracket-comparison ()
  (with-hack-buffer "1>=2"
    (hack--search-up-to ">")
    (should
     (eq (syntax-class (syntax-after (point)))
         1))))

(ert-deftest hack-syntax-angle-bracket-shift-comparison ()
  (with-hack-buffer "$x >>= $y"
    (hack--search-up-to ">")
    (should
     (eq (syntax-class (syntax-after (point)))
         1))
    (forward-char)
    (should
     (eq (syntax-class (syntax-after (point)))
         1))))

(ert-deftest hack-syntax-angle-bracket-shift-left ()
  "Left shift is not a matched pair of angle brackets."
  (with-hack-buffer "1 << 2;"
    (hack--search-up-to "<")
    (should
     (eq (syntax-class (syntax-after (point)))
         1))
    (forward-char)
    (should
     (eq (syntax-class (syntax-after (point)))
         1))))

(ert-deftest hack-syntax-angle-bracket-shift-right ()
  "Left shift is not a matched pair of angle brackets."
  (with-hack-buffer "1 >> 2;"
    (hack--search-up-to ">")
    (should
     (eq (syntax-class (syntax-after (point)))
         1))
    (forward-char)
    (should
     (eq (syntax-class (syntax-after (point)))
         1))))

(ert-deftest hack-syntax-angle-bracket-lambda ()
  "Lambdas are not a matched pair."
  (with-hack-buffer "() ==> $x;"
    (hack--search-up-to ">")
    (should
     (eq (syntax-class (syntax-after (point)))
         1))))

(ert-deftest hack-syntax-angle-bracket-dict ()
  "Lambdas are not a matched pair."
  (with-hack-buffer "dict['foo' => 42];"
    (hack--search-up-to ">")
    (should
     (eq (syntax-class (syntax-after (point)))
         1))))

(ert-deftest hack-syntax-angle-bracket-less-than ()
  "Less than is not a matched type delimiter."
  (with-hack-buffer "$x = 1 > 2;"
    (hack--search-up-to ">")
    (should
     (eq (syntax-class (syntax-after (point)))
         1))))

(ert-deftest hack-syntax-angle-bracket-pipe ()
  "|> is not a matched delimiter."
  (with-hack-buffer "$x = 1 |> foo();"
    (hack--search-up-to ">")
    (should
     (eq (syntax-class (syntax-after (point)))
         1))))

(ert-deftest hack-syntax-angle-bracket-generic ()
  "> is a matched delimiter for generics."
  ;; This program is deliberately not well indented, because it's a
  ;; regression test for #28.
  (with-hack-buffer "function f<
  T1,
  T2,
 >(): void {
  echo \"\";
  echo \"\";
}"
    (hack--search-up-to ">")
    (should
     (eq (syntax-class (syntax-after (point)))
         5))))

(ert-deftest hack-syntax-angle-bracket-greater-than ()
  "Greater than is not a matched type delimiter."
  (with-hack-buffer "$x = 1 < 2;"
    (hack--search-up-to "<")
    (should
     (eq (syntax-class (syntax-after (point)))
         1))))

(ert-deftest hack-syntax-hyphen-in-xhp-class-name ()
  "XHP class names should treat - as a symbol constituent."
  (with-hack-buffer "class :foo-bar {}"
    (hack--search-up-to "-")
    (should
     (eq (syntax-class (syntax-after (point)))
         (car (string-to-syntax "_"))))))

(ert-deftest hack-syntax-colon-in-xhp-class-name ()
  "XHP class names should treat inner : as a symbol constituent."
  (with-hack-buffer "class :foo:bar {}"
    ;; The first : should be punctuation.
    (hack--search-up-to ":")
    (should
     (eq (syntax-class (syntax-after (point)))
         (car (string-to-syntax "."))))

    ;; The second : should be a symbol consituent.
    (forward-char)
    (hack--search-up-to ":")
    (should
     (eq (syntax-class (syntax-after (point)))
         (car (string-to-syntax "_"))))))

(ert-deftest hack-syntax-hyphen-in-xhp-use ()
  "XHP usage should treat - as a symbol constituent."
  (with-hack-buffer "$x = <foo-bar>hello</foo-bar>;"
    (hack--search-up-to "-")
    (should
     (eq (syntax-class (syntax-after (point)))
         (car (string-to-syntax "_"))))))

(ert-deftest hack-syntax-colon-in-xhp-use ()
  "XHP usage should treat - as a symbol constituent."
  (with-hack-buffer "$x = <foo:bar>hello</foo:bar>;"
    (hack--search-up-to ":")
    (should
     (eq (syntax-class (syntax-after (point)))
         (car (string-to-syntax "_"))))))

(ert-deftest hack-highlight-header ()
  "The <?hh should be highlighted, if present."
  (with-hack-buffer "<?hh // strict"
    (search-forward "h")
    (should (eq (face-at-point) 'font-lock-keyword-face))))

(ert-deftest hack-highlight-header-mode ()
  "The mode in the header should be highlighted, if present."
  (with-hack-buffer "<?hh // strict "
    (search-forward "s")
    (should (eq (face-at-point) 'font-lock-keyword-face))))

(ert-deftest hack-highlight-keyword ()
  (with-hack-buffer "class"
    (should (eq (face-at-point) 'font-lock-keyword-face)))
  (with-hack-buffer "CLASS"
    (should (eq (face-at-point) 'font-lock-keyword-face)))
  (with-hack-buffer "endforeach"
    (should (eq (face-at-point) 'font-lock-keyword-face))))

(ert-deftest hack-highlight-contextual-keyword ()
  (with-hack-buffer "super"
    (should (eq (face-at-point) 'font-lock-keyword-face))))

(ert-deftest hack-highlight-constant ()
  (with-hack-buffer "true"
    (should (eq (face-at-point) 'font-lock-constant-face)))
  (with-hack-buffer "TRUE"
    (should (eq (face-at-point) 'font-lock-constant-face))))

(ert-deftest hack-highlight-after-xhp-in-comment ()
  "Regression test after XHP in comments."
  (with-hack-buffer "class MyClass {
  public function myMethod(): mixed {
    // ( <baz> )
      return
        <foo
          desc=\"hello\">
        </foo>;
  }
}"
    (hack--search-up-to "return")
    (should (eq (face-at-point) 'font-lock-keyword-face))
    (hack--search-up-to "foo")
    (should (eq (face-at-point) 'hack-xhp-tag))))

(ert-deftest hack-highlight-type ()
  (with-hack-buffer "string"
    (should (eq (face-at-point) 'font-lock-type-face))))

(ert-deftest hack-highlight-optional-type ()
  (with-hack-buffer "?string"
    ;; Don't highlight the ? as part of the type.
    (should (not (eq (face-at-point) 'font-lock-type-face)))

    ;; But highlight string as a type.
    (search-forward "s")
    (should (eq (face-at-point) 'font-lock-type-face))))

(ert-deftest hack-highlight-xhp-type ()
  (with-hack-buffer "class :foo:bar {}"
    (search-forward "f")
    (should (eq (face-at-point) 'font-lock-type-face))))

(ert-deftest hack-highlight-xhp-optional-type ()
  (with-hack-buffer "function foo(): ?:xhpclass {}"
    (search-forward "x")
    (should (eq (face-at-point) 'font-lock-type-face))))

(ert-deftest hack-highlight-xhp-as-type-param ()
  (with-hack-buffer "function foo(): Awaitable<:xhpclass> {}"
    (search-forward "x")
    (should (eq (face-at-point) 'font-lock-type-face))))

(ert-deftest hack-highlight-built-in-constant ()
  (with-hack-buffer "true"
    (should (eq (face-at-point) 'font-lock-constant-face)))
  (with-hack-buffer "TRUE"
    (should (eq (face-at-point) 'font-lock-constant-face)))
  (with-hack-buffer "foo_true();"
    (search-forward "t")
    (should (not (eq (face-at-point) 'font-lock-constant-face)))))

(ert-deftest hack-highlight-built-in-function ()
  (with-hack-buffer "invariant(true, \"hello world\");"
    (should (eq (face-at-point) 'font-lock-builtin-face)))
  ;; Should be case sensitive.
  (with-hack-buffer "invariANT(true, \"hello world\");"
    (should (not (eq (face-at-point) 'font-lock-builtin-face))))
  ;; Don't confuse functions with methods.
  (with-hack-buffer "$foo->invariant(true, \"hello world\");"
    (search-forward "i")
    (should (not (eq (face-at-point) 'font-lock-builtin-face)))))

(ert-deftest hack-highlight-function-name ()
  (with-hack-buffer "function foo(): void {}"
    (search-forward "fo")
    (should (eq (face-at-point) 'font-lock-function-name-face)))
  (with-hack-buffer "$x = <p>function foo()</p>"
    (search-forward "fo")
    (should (not (eq (face-at-point) 'font-lock-function-name-face)))))

(ert-deftest hack-highlight-heredoc ()
  (with-hack-buffer "$x = <<<EOT
hello world
EOT;
bar();"
    (search-forward "hello")
    (should (eq (face-at-point) 'font-lock-string-face))
    (search-forward "bar")
    (should (not (eq (face-at-point) 'font-lock-string-face)))))

(ert-deftest hack-highlight-heredoc-identifier-position ()
  "The closing identifier must be at the start of a line."
  (with-hack-buffer "$x = <<<EOT
hello EOT world
EOT;
bar();"
    (search-forward "world")
    (should (eq (face-at-point) 'font-lock-string-face))))

(ert-deftest hack-highlight-heredoc-requires-newline ()
  "Heredocs require a newline immediately after the identifier."
  (with-hack-buffer "function filter<<<__Enforceable>> reify T>(vec<mixed> $list): vec<T> {}"
    (search-forward "<")
    (should (not (eq (face-at-point) 'font-lock-string-face)))))

(ert-deftest hack-highlight-nowdoc ()
  (with-hack-buffer "$x = <<<'EOT'
hello world
EOT;
bar();"
    (search-forward "hello")
    (should (eq (face-at-point) 'font-lock-string-face))
    (search-forward "bar")
    (should (not (eq (face-at-point) 'font-lock-string-face)))))

(ert-deftest hack-highlight-heredoc-containing-quotes ()
  "If there's a doublequote inside a heredoc, we should still
stop highlighting at the identifier."
  (with-hack-buffer "$x = <<<EOT
hello \"world
EOT;
bar();"
    (search-forward "hello")
    (should (eq (face-at-point) 'font-lock-string-face))
    (search-forward "bar")
    (should (not (eq (face-at-point) 'font-lock-string-face)))))

(ert-deftest hack-highlight-c-comment ()
  (with-hack-buffer "// foo bar
baz();"
    (search-forward "foo")
    (should (eq (face-at-point) 'font-lock-comment-face))
    (search-forward "baz")
    (should (not (eq (face-at-point) 'font-lock-comment-face)))))

(ert-deftest hack-highlight-cpp-comment ()
  (with-hack-buffer "/* foo bar
baz();
*/
qux();"
    (search-forward "baz")
    (should (eq (face-at-point) 'font-lock-comment-face))
    (search-forward "qux")
    (should (not (eq (face-at-point) 'font-lock-comment-face)))))

(ert-deftest hack-highlight-shell-comment ()
  (with-hack-buffer "# foo bar
baz()"
    (search-forward "foo")
    (should (eq (face-at-point) 'font-lock-comment-face))
    (search-forward "baz")
    (should (not (eq (face-at-point) 'font-lock-comment-face)))))

(ert-deftest hack-highlight-string-interpolation ()
  (with-hack-buffer "$x = \"$foo bar\";"
    (search-forward "f")
    (should (eq (face-at-point) 'font-lock-variable-name-face))
    (search-forward "b")
    (should (eq (face-at-point) 'font-lock-string-face))))

(ert-deftest hack-highlight-string-interpolation-objects ()
  (with-hack-buffer "$x = \"$foo->bar\";"
    (search-forward "b")
    (should (eq (face-at-point) 'font-lock-variable-name-face)))
  (with-hack-buffer "$x = \"$foo->bar->biz\";"
    (search-forward "i")
    (should (eq (face-at-point) 'font-lock-variable-name-face))))

(ert-deftest hack-highlight-string-interpolation-indexing ()
  (with-hack-buffer "$x = \"$foo[123]\";"
    (search-forward "2")
    (should (eq (face-at-point) 'font-lock-variable-name-face)))
  (with-hack-buffer "$x = \"$foo[a][b]\";"
    (search-forward "b")
    (should (eq (face-at-point) 'font-lock-variable-name-face))))

(ert-deftest hack-highlight-string-interpolation-braced ()
  (with-hack-buffer "$x = \"${foo}bar\";"
    (search-forward "f")
    (should (eq (face-at-point) 'font-lock-variable-name-face))
    (search-forward "}")
    (backward-char 1)
    (should (eq (face-at-point) 'font-lock-variable-name-face))
    (search-forward "b")
    (should (not (eq (face-at-point) 'font-lock-variable-name-face)))))

(ert-deftest hack-highlight-heredoc-interpolation ()
  (with-hack-buffer "$x = <<<EOT\n$foo bar\nEOT;"
    (search-forward "f")
    (should (eq (face-at-point) 'font-lock-variable-name-face))))

(ert-deftest hack-highlight-string-interpolation-escaped ()
  "Backslashes prevent interpolation."
  (with-hack-buffer "$x = \"\\$foo bar\";"
    (search-forward "f")
    (should (not (eq (face-at-point) 'font-lock-variable-name-face)))))

(ert-deftest hack-highlight-string-interpolation-complex ()
  (with-hack-buffer "$x = \"{$foo}\";"
    (search-forward "f")
    (should (eq (face-at-point) 'font-lock-variable-name-face))
    (search-forward "}")
    (backward-char 1)
    (should (eq (face-at-point) 'font-lock-variable-name-face)))
  (with-hack-buffer "$x = \"{$foo['bar']}\";"
    (search-forward "}")
    (backward-char 1)
    (should (eq (face-at-point) 'font-lock-variable-name-face))))

(ert-deftest hack-highlight-string-interpolation-not-complex ()
  "Spaces prevent complex interpolation."
  (with-hack-buffer "$x = \"{ $foo}\";"
    (search-forward "{")
    (backward-char 1)
    (should (not (eq (face-at-point) 'font-lock-variable-name-face)))

    (search-forward "f")
    (backward-char 1)
    (should (eq (face-at-point) 'font-lock-variable-name-face))))

(ert-deftest hack-highlight-no-interpolation ()
  "Hack doesn't interpolate between single quoted strings."
  (with-hack-buffer "$x = '$foo bar';"
    (search-forward "f")
    (should (not (eq (face-at-point) 'font-lock-variable-name-face))))
  (with-hack-buffer "$x = <<<'EOT'\n\n$foo bar\nEOT;"
    (search-forward "f")
    (should (not (eq (face-at-point) 'font-lock-variable-name-face)))))

(ert-deftest hack-highlight-fallthrough ()
  "Highlight fallthrough comments."
  (with-hack-buffer "// FALLTHROUGH"
    (search-forward "F")
    (should (eq (face-at-point) 'font-lock-keyword-face)))
  ;; Ensure we don't highlight FALLTHROUGH in other contexts.
  (with-hack-buffer "/* FALLTHROUGH */"
    (search-forward "F")
    (should (not (eq (face-at-point) 'error))))
  (with-hack-buffer "$x = \" FALLTHROUGH\";"
    (search-forward "F")
    (should (not (eq (face-at-point) 'error)))))

(ert-deftest hack-highlight-unsafe-block ()
  "Highlight unsafe blocks."
  (with-hack-buffer "// UNSAFE"
    (search-forward "U")
    (should (eq (face-at-point) 'error)))
  (with-hack-buffer "//UNSAFE"
    (search-forward "U")
    (should (eq (face-at-point) 'error)))
  ;; Ensure we don't highlight UNSAFE in other contexts.
  (with-hack-buffer "/* UNSAFE */"
    (search-forward "U")
    (should (not (eq (face-at-point) 'error))))
  (with-hack-buffer "$x = \" UNSAFE\";"
    (search-forward "U")
    (should (not (eq (face-at-point) 'error)))))

(ert-deftest hack-highlight-unsafe-expr-block ()
  "Highlight unsafe expression comments."
  (with-hack-buffer "/* UNSAFE_EXPR */"
    (search-forward "U")
    (should (eq (face-at-point) 'error)))
  ;; Ensure we don't highlight UNSAFE_EXPR in other contexts.
  (with-hack-buffer "// UNSAFE_EXPR"
    ;; This case is messy: it's not an UNSAFE_EXPR comment, but it's a
    ;; valid UNSAFE comment. The _EXPR bit should not be highlighted.
    (search-forward "X")
    (should (not (eq (face-at-point) 'error))))
  (with-hack-buffer "$x = \" UNSAFE_EXPR\";"
    (search-forward "U")
    (should (not (eq (face-at-point) 'error)))))

(ert-deftest hack-highlight-hh-fixme ()
  "Highlight HH_FIXME comments."
  (with-hack-buffer "/* HH_FIXME[1234] hello world */"
    (search-forward "H")
    (should (eq (face-at-point) 'error)))
  ;; The lexer does actually allow spaces.
  (with-hack-buffer "/* HH_FIXME [1234] hello world */"
    ;; Ensure that the error code is included.
    (search-forward "4")
    (should (eq (face-at-point) 'error)))
  ;; Ensure we don't highlight it in other contexts.
  (with-hack-buffer "// HH_FIXME[1234] hello world"
    (search-forward "H")
    (should (not (eq (face-at-point) 'error))))
  (with-hack-buffer "$x = \" HH_FIXME[1234] hello world\";"
    (search-forward "H")
    (should (not (eq (face-at-point) 'error)))))

(ert-deftest hack-highlight-ignore-error ()
  "Highlight HH_IGNORE_ERROR comments."
  (with-hack-buffer "/* HH_IGNORE_ERROR[1234] hello world */"
    (search-forward "H")
    (should (eq (face-at-point) 'error)))
  ;; The lexer does actually allow spaces.
  (with-hack-buffer "/* HH_IGNORE_ERROR [1234] hello world */"
    ;; Ensure that the error code is included.
    (search-forward "4")
    (should (eq (face-at-point) 'error)))
  ;; Ensure we don't highlight it in other contexts.
  (with-hack-buffer "// HH_IGNORE_ERROR[1234] hello world"
    (search-forward "H")
    (should (not (eq (face-at-point) 'error))))
  (with-hack-buffer "$x = \" HH_IGNORE_ERROR[1234] hello world\";"
    (search-forward "H")
    (should (not (eq (face-at-point) 'error)))))

(ert-deftest hack-highlight-xhp-tag ()
  "Set font face on tag names in XHP expressions."
  (with-hack-buffer "<foo>hello</foo>"
    (search-forward "f")
    (should (eq (face-at-point) 'hack-xhp-tag)))
  ;; - is symbol constituent in XHP.
  (with-hack-buffer "<foo-bar>hello</foo-bar>"
    (search-forward "b")
    (should (eq (face-at-point) 'hack-xhp-tag))))

(ert-deftest hack-xhp-single-quote ()
  "Single quotes inside XHP do not signify a string."
  (with-hack-buffer "$p = <p>Hello'world</p>;"
    (search-forward "'")
    (backward-char 1)
    (should
     (eq (syntax-class (syntax-after (point)))
         1))))

(ert-deftest hack-xhp-single-quote-interpolate ()
  "Single quotes inside XHP inside interpolation."
  (with-hack-buffer "$p = <div class={foo('bar')}>stuff</p>;"
    (search-forward "'")
    (backward-char 1)
    (should
     (eq (syntax-class (syntax-after (point)))
         7))))

(ert-deftest hack-xhp-incomplete ()
  "Ensure we handle incomplete XHP blocks gracefully."
  (with-hack-buffer "$p = <p>Hello"))

(ert-deftest hack-xhp-incomplete-tag ()
  "Ensure we handle incomplete XHP blocks gracefully."
  (with-hack-buffer "$p = <p"))

(ert-deftest hack-xhp-unbalanced ()
  "Ensure we handle unbalanced XHP blocks gracefully."
  (with-hack-buffer "$p = <foo>Hello</bar>;"))

(ert-deftest hack-xhp-quoted-tags ()
  "< or > in strings shouldn't confuse our XHP highlighting."
  (with-hack-buffer "$p = <foo attr=\">\">Hello'world</foo>;"
    ;; If we've correctly detected that we're inside XHP, we should
    ;; have treated ' as punctuation.
    (search-forward "'")
    (backward-char 1)
    (should
     (eq (syntax-class (syntax-after (point)))
         1)))
  (with-hack-buffer "$p = <foo attr=\"<\">Hello'world</foo>;"
    (search-forward "'")
    (backward-char 1)
    (should
     (eq (syntax-class (syntax-after (point)))
         1)))
  (with-hack-buffer "$p = <foo attr=\"><bar>\">Hello'world</foo>;"
    (search-forward "'")
    (backward-char 1)
    (should
     (eq (syntax-class (syntax-after (point)))
         1))))

(ert-deftest hack-xhp-string-contents ()
  "Text between XHP tags should not be highlighted."
  (with-hack-buffer "$p = <p>Hello</p>;"
    (search-forward "H")
    (should
     (should
      (not (eq (face-at-point) 'font-lock-type-face)))))
  (with-hack-buffer "$p = <p>vec</p>;"
    (search-forward "v")
    (should
     (should
      (not (eq (face-at-point) 'font-lock-keyword-face))))))

(ert-deftest hack-xhp-string-contents-syntax ()
  "Syntax delimiters inside XHP tags should be punctuation."
  (with-hack-buffer "$p = <p>\"</p>;"
    (hack--search-up-to "\"")
    (should
     (eq (syntax-class (syntax-after (point)))
         (car (string-to-syntax ".")))))
  (with-hack-buffer "$p = <p>'</p>;"
    (hack--search-up-to "'")
    (should
     (eq (syntax-class (syntax-after (point)))
         (car (string-to-syntax "."))))))

(ert-deftest hack-xhp-interpolation-syntax ()
  "Syntax delimiters inside interpolation are normal delimiters."
  (with-hack-buffer "$p = <p>{\"foo\"}</p>;"
    (hack--search-up-to "\"")
    (should
     (eq (syntax-class (syntax-after (point)))
         (car (string-to-syntax "\"")))))
  (with-hack-buffer "$p = <p>{'foo'}</p>;"
    (hack--search-up-to "'")
    (should
     (eq (syntax-class (syntax-after (point)))
         (car (string-to-syntax "\""))))))

(ert-deftest hack-xhp-interpolation-syntax-angle-bracket ()
  "< > delimiters inside interpolation are normal delimiters."
  (with-hack-buffer "$p = <p>{$foo->bar}</p>;"
    (hack--search-up-to "->")
    (should
     (eq (syntax-class (syntax-after (point)))
         (car (string-to-syntax "."))))))

(ert-deftest hack-xhp-property-interpolation-syntax-angle-bracket ()
  "< > delimiters inside XHP property interpolation are normal delimiters."
  (with-hack-buffer "$f = <foo prop={$this->getProp()} />;"
    (hack--search-up-to "->")
    (should
     (eq (syntax-class (syntax-after (point)))
         (car (string-to-syntax "."))))))

(ert-deftest hack-xhp-interpolation ()
  "Interpolation in XHP blocks occurs between curly braces."
  (with-hack-buffer "$p = <p>{$x}</p>;"
    (search-forward "$")
    (should
     (eq (face-at-point) 'font-lock-variable-name-face))))

(ert-deftest hack-xhp-not-interpolation ()
  "Ensure we don't highlight outside interpolated regions."
  ;; Not interpolation.
  (with-hack-buffer "$p = <p>$x</p>;"
    (search-forward "$x")
    (backward-char)
    (should
     (not (eq (face-at-point) 'font-lock-variable-name-face))))
  (with-hack-buffer "$p = <p>$$</p>;"
    (search-forward "$$")
    (backward-char)
    (should
     (not (eq (face-at-point) 'font-lock-variable-name-face))))
  ;; Curly parens outside of the XHP expression.
  (with-hack-buffer "if (true) {\n  $p = <p>$x</p>;\n}"
    (search-forward "$x")
    (backward-char)
    (should
     (not (eq (face-at-point) 'font-lock-variable-name-face)))))

(ert-deftest hack-xhp-self-closing ()
  "Ensure we don't think a self-closing tag ends an XHP block"
  (with-hack-buffer "$p = <div>\n    <p>\n  <Stuff/>\n  </p>\n  Hello\n</div>;\n"
    (search-forward "H")
    (should
     (should
      (not (eq (face-at-point) 'font-lock-type-face)))))
  (with-hack-buffer "$p = <Stuff/>;"
    (search-forward "S")
    (should
     (should
      (not (eq (face-at-point) 'font-lock-type-face))))))

(ert-deftest hack-xhp-in-comments ()
  "Don't try to parse XHP in comments."
  (with-hack-buffer "// <foo>"
    (search-forward "f")
    (should (eq (face-at-point) 'font-lock-comment-face))))
