;;; xhp-indent.el --- indent xhp fragments -*- lexical-binding: t -*-

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

;;; Code:

(require 'cc-mode)

(defvar xhp-indent-debug-on nil)

(defvar xhp-indent-start-regex "\\(return +\\|^ *\\|==> *\\|\\? *\\|= *\\|( *\\)<[^<\\]"
  "the regex used to match the valid start of an xhp expression")

(defvar xhp-indent-syntax-attributes
  '(xhp-indent-in-attribute
    xhp-indent-in-mutiline-php-in-xhp-block
    xhp-indent-in-closing-elt
    xhp-indent-in-closing-stmt
    xhp-indent-in-first-statement-after-xhp
    xhp-indent-php-in-xhp
    xhp-indent-in-xhp))

(defun xhp-indent-debug (&rest args)
  (if xhp-indent-debug-on
      (apply 'message args)))

(defun xhp-indent-previous-semi (min)
  "helper for finding the previous semicolon not in a string or comment"
  (if (not min)
      (setq min (point-min)))
  (if (> min (point))
      nil ;; search/re-search error if this is true. stupid
    (let
        ((res))
      (save-excursion
        (while
            (if (search-backward ";" min t)
                (if (c-in-literal min)
                    t ;; keep searching
                  (setq res (point)) ;; semi found, done.
                  nil)))
        res))))


;; 1000 was chosen somewhat arbitrarily in that it didn't seem to
;; perform worse than 500 in a test file, but seems more than
;; sufficient to encompass a single xhp statement
(defconst xhp-indent-max-backtrack 1000
  "the maximum number of characters that xhp-indent will look
  backwards for xhp start. js_strings.php was the motivation for
  this")

(defun xhp-indent-xhp-detect ()
  "determine if xhp around or above point will affect indentation"
  (save-excursion
    (c-save-buffer-state
        (
         (single-line-php-brace-pos (c-most-enclosing-brace (c-parse-state)))
         (min-brace
          (progn
            ;; get out of anything being typed that might confuse the parsing
            (beginning-of-line) ;; SIDE EFFECT
            (c-most-enclosing-brace (c-parse-state))))
         (min (save-excursion
                (or
                 (xhp-indent-previous-semi min-brace)
                 min-brace
                 (+ (point-min) 5) ;; skip past <?php
                 )))
         (max (point))
         base-indent
         xhp-start-pos
         )
      ;; STEP 1: find a previous xhp element, and derive the normal
      ;; indentation from it.
      (save-excursion
        (if (and
             (> (point) min)
             (re-search-backward xhp-indent-start-regex min t)
             (not (c-in-literal)))
            (setq
             xhp-start-pos (point)
             base-indent
             ;; decide from this context if indentation should
             ;; be initially adjusted.
             (+
              ;; start with the indentation at this elt
              (current-indentation)
              ;; at the matched xhp element, figure out if the
              ;; indentation should be modified
              ;; TODO(abrady) too lazy to parse forward properly, these
              ;; work fine for now.
              (cond
               ;; CASE 1: matched elt is closed or self-closing e.g. <br />
               ;; or a 1-line enclosed stmt: <fbt:param>foo</fbt:param>
               ((save-excursion
                  (beginning-of-line)
                  (or
                   (re-search-forward "</" (line-end-position) t)
                   (re-search-forward "/> *$" max t)
                   (re-search-forward "--> *$" max t)))
                0)
               ;; DEFAULT: increase indent
               (t 2))
              ))))
      ;; STEP 2: indentation adjustment based on what user has typed so far
      (if base-indent
          ;; STEP 2.1: we found indentation to adjust. use the current
          ;; context to determine how it should be adjusted
          (progn
            (let
                ((res))
              (setq res
                    (cond
                     ;; CASE 0: indenting an attribute
                     ((looking-at "^ *[a-zA-Z_-]+")
                      (list base-indent 'xhp-indent-in-attribute))
                     ;; CASE 1: Terminating a multiline php block is a special
                     ;; case where we should default to php indentation as if we
                     ;; were inside the braces
                     ;; e.g. <div class={foo($a
                     ;;                      $b)}>
                     ((save-excursion
                        (and
                         (not (re-search-forward "^ *<" (line-end-position) t))
                         (re-search-forward "}> *$" (line-end-position) t)))
                      (xhp-indent-debug "terminating php block")
                      (list nil 'xhp-indent-in-mutiline-php-in-xhp-block))
                     ;; CASE 2: user is indenting a closing block, so out-dent
                     ;; e.g.
                     ;; <div>
                     ;; </div>
                     ((save-excursion
                        (re-search-forward "^ *</" (line-end-position) t))
                      (list (+ base-indent -2) 'xhp-indent-in-closing-elt))
                     ;; CASE 3: if this happens to be /> on its own
                     ;; line, reduce indent (coding standard)
                     ((save-excursion
                        (goto-char max)
                        (re-search-forward "^ */> *" (line-end-position) t))
                      (list (+ base-indent -2) 'xhp-indent-in-closing-stmt))
                     ;; CASE 4: close of xhp passed to a function, e.g.
                     ;; foo(
                     ;;   <xhp>
                     ;; );
                     ((save-excursion
                        (re-search-forward "^ *);" (line-end-position) t))
                      (list (+ base-indent -2) 'xhp-indent-in-closing-stmt))
                     ;; DEFAULT: no modification.
                     (t (list base-indent))))
              ;; already determined we're in xhp, if we have a
              ;; single-line brace it must be php in xhp.
              (if (and
                   single-line-php-brace-pos
                   min-brace
                   (< min-brace single-line-php-brace-pos))
                  (setq res (append res '(xhp-indent-php-in-xhp))))
              (append res '(xhp-indent-in-xhp) (list 'xhp-start-pos xhp-start-pos))
              ))
        ;; STEP 2.2: FIRST STATEMENT AFTER XHP. if we're after
        ;; the close of an xhp statement it still messes up the php
        ;; indentation, so check that here and override
        (cond
         ;; CASE 1: multiline self-enclosing tag or closing tag
         ;; e.g.
         ;; <div
         ;;   foo="bar"
         ;; />;
         ;; - or -
         ;; <div>
         ;;  ...
         ;; </div>;
         ((save-excursion
            (c-backward-syntactic-ws)
            (and
             (looking-back "\\(/>\\|</.*>\\);" nil)
             ;; don't match single-line xhp $foo = <x:frag />;
             (not (re-search-backward "^ *\\$" (line-beginning-position) t))))
          ;; previous statement IS xhp. check what user has typed so
          ;; far
          (list
           (+
            (save-excursion (c-backward-syntactic-ws) (current-indentation))
            (cond
             ;; CASE 0: user typed a brace. outdent even more
             ((looking-at ".*}") -4)
             ;; CASE 1: close of case in a switch stmt, e.g. case FOO:
             ((looking-at ".*: *$") -4)
             ;; DEFAULT
             (t -2)))
           'xhp-indent-in-first-statement-after-xhp)
          )
         ;; DEFAULT: not first stmt after xhp, let c-indent figure
         ;; this out normally
         (t (list nil 'xhp-indent-in-php)))
        )
      )))

(defun xhp-indent-syntax-detect ()
  "emacs' built in syntax checking can only handle one or two character values for determining indentation. This function provides a more expensive way to detect indentation for contexts where the builtin syntax checking fails. There are currently two cases for this:
- inside xhp
- inside heredoc (not handled)

returns a list of (indent-amount syntax-infos ...)
"
   (xhp-indent-xhp-detect))

(defun xhp-indent-syntax-indent-amount (syntax)
  (car syntax))

(defun xhp-indent-syntax-has-attribute (syntax attribute)
  (or
   (not xhp-indent-debug-on)
   (memq attribute xhp-indent-syntax-attributes) ;; perf issue
   (error "invalid attribute %s" (symbol-name attribute)))
  (memq attribute (cdr syntax)))

(defun xhp-indent-start-pos (&optional xhp-indent-info)
  "helper for getting start position attribute from `xhp-indent-xhp-detect result"
  (cadr (xhp-indent-syntax-has-attribute
        (or xhp-indent-info (xhp-indent-xhp-detect)) 'xhp-start-pos)))

(defun xhp-indent-in-xhp ()
  (interactive)
  "helper for detecting if point is in xhp"
  (xhp-indent-syntax-has-attribute (xhp-indent-xhp-detect) 'xhp-indent-in-xhp))

(defun xhp-indent-detect ()
  (interactive)
  (xhp-indent-syntax-indent-amount (xhp-indent-syntax-detect)))

(defun xhp-indent ()
  (interactive)
  (let
      ((indent (xhp-indent-detect)))
    (if indent
        (progn
          (xhp-indent-debug "xhp indent!!!")
          ;; this is better than indent-to and indent-line-to because
          ;; it sets the point properly in a few different contexts.
          ;; e.g. when you've typed stuff, keep the point
          ;; but when you've typed nothing, go to end of line.
          (c-shift-line-indentation (- indent (current-indentation)))
          ))
    indent))

(defun xhp-indent-cautious-indent-line ()
  "call xhp indent, or fallback to c-indent if not applicable"
  (if (not (xhp-indent))
      (funcall 'c-indent-line)))

(defun xhp-indent-line ()
  "Indent current line."
  (interactive (list current-prefix-arg (use-region-p)))
  (xhp-indent-cautious-indent-line))

;; Electric keys: override the built in C ones to use xhp-indent

(defun xhp-indent-keybinds ()
  (local-set-key ";" 'xhp-indent-electric-semi&comma)
  (local-set-key "," 'xhp-indent-electric-semi&comma)
  (local-set-key "}" 'xhp-indent-electric-brace)
  (local-set-key "{" 'xhp-indent-electric-brace)
  (local-set-key ":" 'xhp-indent-electric-colon)
)

(defun xhp-indent-electric-semi&comma (arg)
  (interactive "*P")
  (if (and c-electric-flag (xhp-indent))
      (self-insert-command (prefix-numeric-value arg))
    (c-electric-semi&comma arg)))

(defun xhp-indent-electric-brace (arg)
  (interactive "*P")
  (if (and c-electric-flag (xhp-indent))
      (self-insert-command (prefix-numeric-value arg))
    (c-electric-brace arg)))

(defun xhp-indent-electric-colon (arg)
  (interactive "*P")
  (if (and c-electric-flag (xhp-indent))
      (self-insert-command (prefix-numeric-value arg))
    (c-electric-colon arg)))

;; TODOS
;; arrays with xhp:
;; 'foo' =>
;;   <div>
;;     ...
;;   </div>,
;; 'bar'
;; php in xhp:
;; <ui:link
;;   href={$app->getAppCenterURL()}>
;;   {
;;     $this->getChildren()
;;       }
;; </ui:link>;
;; <br/> not on its own line:
;;; RunKeeper on your timeline<br/>
;;; <foo>
;; fails:
;; id(
;;   <fbt
;;     secret="appcenter"
;;     desc="platform type topnav filter, appcenter">
;;     All
;;   </fbt>
;; ), <= right here
(provide 'xhp-indent)
;;; xhp-indent.el ends here
