;;; test-helper.el --- Helper for tests              -*- lexical-binding: t; -*-

;; Copyright (c) Facebook, Inc. and its affiliates. All Rights Reserved.

;;; Code:

(require 'ert)
(require 'f)

(let ((hack-mode-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path hack-mode-dir))

(require 'undercover)
(undercover "hack-mode.el"
	    (:exclude "*-test.el")
	    (:report-file "/tmp/undercover-report.json"))

;;; test-helper.el ends here
