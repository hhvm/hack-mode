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
