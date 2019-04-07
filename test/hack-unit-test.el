;; Copyright (c) Facebook, Inc. and its affiliates. All Rights Reserved.

(require 'ert)
(require 'hack-mode)

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
    (search-forward ">")
    (backward-char)
    (should
     (eq (syntax-class (syntax-after (point)))
         5))))

(ert-deftest hack-syntax-angle-bracket-method ()
  "Method access is not a matched pair."
  (with-hack-buffer "$foo->bar();"
    (search-forward ">")
    (backward-char)
    (should
     (eq (syntax-class (syntax-after (point)))
         1))))

(ert-deftest hack-syntax-angle-bracket-lambda ()
  "Lambdas are not a matched pair."
  (with-hack-buffer "() ==> $x;"
    (search-forward ">")
    (backward-char)
    (should
     (eq (syntax-class (syntax-after (point)))
         1))))

(ert-deftest hack-syntax-angle-bracket-dict ()
  "Lambdas are not a matched pair."
  (with-hack-buffer "dict['foo' => 42];"
    (search-forward ">")
    (backward-char)
    (should
     (eq (syntax-class (syntax-after (point)))
         1))))

(ert-deftest hack-syntax-angle-bracket-less-than ()
  "Less than is not a matched type delimiter."
  (with-hack-buffer "$x = 1 > 2;"
    (search-forward ">")
    (backward-char)
    (should
     (eq (syntax-class (syntax-after (point)))
         1))))

(ert-deftest hack-syntax-angle-bracket-greater-than ()
  "Greater than is not a matched type delimiter."
  (with-hack-buffer "$x = 1 < 2;"
    (search-forward "<")
    (backward-char)
    (should
     (eq (syntax-class (syntax-after (point)))
         1))))

(ert-deftest hack-highlight-header ()
  (with-hack-buffer "<?hh // strict"
    (search-forward "h")
    (should (eq (face-at-point) 'font-lock-keyword-face))))

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

(ert-deftest hack-xhp-single-quote ()
  "Single quotes inside XHP do not signify a string."
  (with-hack-buffer "$p = <p>Hello'world</p>;"
    (search-forward "'")
    (backward-char 1)
    (should
     (eq (syntax-class (syntax-after (point)))
         1))))

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

(ert-deftest hack-xhp-in-comments ()
  "Don't try to parse XHP in comments."
  (with-hack-buffer "// <foo>\n$x = 'bar';"
    (search-forward "b")
    (should (eq (face-at-point) 'font-lock-string-face))))
