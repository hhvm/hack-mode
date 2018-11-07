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
  (let ((src "function foo(): int {
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
