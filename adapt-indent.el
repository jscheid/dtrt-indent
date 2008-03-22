;;; adapt-indent.el --- Adapt to foreign indentation offsets

;; Copyright (C) 2003, 2007, 2008 Julian Scheid

;; Author: Julian Scheid <julians37@googlemail.com>
;; Created: 22 Mar 2008
;; Version: 0.2.0
;; Keywords: convenience files languages c

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; A minor mode that guesses the indentation offset originally used
;; for creating source code files and transparently adjusts the
;; corresponding settings in Emacs, making it easier to edit foreign
;; files.
;;
;; This hooks into many major modes - c-mode, java-mode, shell-mode
;; and ruby-mode, to name but a few - and makes an educated guess on
;; which offset is appropriate by analyzing indentation levels in the
;; file.
;;
;; Heuristics are used to estimate the proper indentation offset and
;; therefore this system is not infallible, however adjustments will
;; only be made if the guess is considered reliable.  This way it
;; should leave you off no worse than before.
;;
;; To install,
;;   (require 'adapt-indent)
;;   (adapt-indent-mode 1)
;;
;; The default settings have been carefully chosen and tested to work
;; reliably on a wide range of source files.  However, if it doesn't
;; work for you they can be fine tuned using M-x customize-group
;; adapt-indent
;;
;; There is more extensive information in the adapt-indent info page
;; which you currently need to install manually.
;;
;; Improvements over guess-offset.el:
;;
;; - Whereas guess-offset only worked for C, C++ and Java files,
;;   adapt-indent supports plenty of major modes (Shell script, Perl
;;   and Ruby are worth mentioning) and is easier to adapt to other
;;   languages.
;;
;; - adapt-indent is now a minor mode and can be switched on and off,
;;   both globally and locally (the latter using a File Variable).
;;
;; - adapt-indent is more precise in analyzing the syntax of source
;;   files, making its guess more accurate (it now ignores lines in
;;   comments, multi-line expressions, here documents and the like.)
;;
;; - adapt-indent stops analyzing a source file after a customizable
;;   amount of lines, making it operate faster on large source files.
;;
;; - adapt-indent leaves alone files that explicitly set the
;;   indentation offset with a File Variable.
;;
;; - adapt-indent comes with diagnostic functions to help you
;;   understand what it does behind the scenes, adapt it to new
;;   languages or fine-tune its parameters.
;;
;; - The name of the script has been changed to better reflect its
;;   purpose.
;;
;; - The customization group is now a child of the convenience and
;;   files groups instead of the tools group.
;;
;; - The customization variables are named more sensibly and are
;;   better documented.
;;
;; - Documentation is improved and no longer confusingly refers to
;;   "tab width" instead of "indentation offset".
;;
;; Files not touched by adapt-indent:
;;
;; - Files that specify the corresponding variable
;;   (e.g. c-basic-offset) as a File Variable.
;;
;; - Files that specify adapt-indent-mode: 0 as a File Variable.
;;
;; - Files for which adapt-indent-accept-file-function returns nil.
;;
;; - Files with a major mode that adapt-indent doesn't hook into.
;;
;; - Files for which the indentation offset cannot be guessed
;;   reliably.
;;
;; Limitations:
;;
;; - adapt-indent does not deal well with files that use variable
;;   indentation offsets, e.g. files that use varying indentation
;;   based on the outer construct.
;;
;; - adapt-indent currently only supports a limited number of
;;   languages (major-modes).
;;
;; - adapt-indent only guesses the indendation offset, not the
;;   indentation style.  For instance, it does not detect whether a
;;   C-like file uses hanging braces or not.
;;
;; - adapt-indent guesses the indendation offset, not the (hard) tab
;;   width used in a file.  If you're loading a source file that uses
;;   hard tabs mixed with spaces for indentation, and it was created
;;   with a different tab size than the one assumed by Emacs, you'll
;;   end up with messy indentation - adapt-indent won't be any help.
;;
;; TODO:
;;
;; - verbose and diagnostics messages
;; - make sure variable documentation match their function
;; - make sure defaults are sensible
;; - double-check that for potentially derived modes (e.g. c++,
;;   objective c), hooks are only registered/executed once
;; - complete info page
;; - bulk (real world) tests
;; - functional tests
;; - unit tests
;; - c-basic-offset value?

;;; Change log:

;; Revision 0.2.0 (2008-03-25)
;; Major rewrite
;; Name change from guess-offset.el to adapt-indent.el
;;
;; Revision 0.1.2 (2007-02-02)
;; Minor documentation cleanups
;; Added link to cc-guess.el
;; Applied two patches courtesy of Michael Ernst <mernst@alum.mit.edu>:
;; (1) The problem is that you wrote
;;     (- 1 bracket-level)
;;     where you probably meant
;;     (- bracket-level 1)
;; (2) The documentation for `beginning-of-buffer' says
;;     Don't use this command in Lisp programs!
;;     (goto-char (point-min)) is faster and avoids clobbering the mark.
;;
;; Revision 0.1.1 (2003-??-??)
;; Initial version

;;; Code:

;;;###autoload
(define-minor-mode adapt-indent-mode
  "Toggle adapt-indent mode.
With no argument, this command toggles the mode.  Non-null prefix
argument turns on the mode.  Null prefix argument turns off the
mode.

When adapt-indent mode is enabled, the proper indentation
offset will be guessed for newly opened files and adjusted
transparently."
  :global t :group 'adapt-indent)

(defvar adapt-indent-language-syntax-table
  '((c/c++/java ("\""                    0   "\""       nil "\\\\.")
                ("'"                     0   "'"        nil "\\\\.")
                ("[/][*]"                0   "[*][/]"   nil)
                ("[/][/]"                0   "$"        nil)
                ("("                     0   ")"        t)
                ("\\["                   0   "\\]"      t))

    (perl       ("\""                    0   "\""       nil "\\\\.")
                ("'"                     0   "'"        nil "\\\\.")
                ("[#]"                   0   "$"        nil)
                ("("                     0   ")"        t)
                ("\\["                   0   "\\]"      t))

    (ruby       ("\""                    0   "\""       nil "\\.")
                ("'"                     0   "'"        nil "\\.")
                ("#"                     0   "$"        nil)
                ("("                     0   ")"        t)
                ("\\["                   0   "\\]"      t)
                ("{"                     0   "}"        t))

    (ada        ("\""                    0   "\""       nil "\\.")
                ("--"                    0   "$"        nil)
                ("("                     0   ")"        t)
                ("\\["                   0   "\\]"      t)
                ("{"                     0   "}"        t))

;;  python-mode comes with offset guessing
;;  (python     ("\"\"\""                0   "\"\"\""   nil "\\.")
;;              ("\""                    0   "\""       nil "\\.")
;;              ("'"                     0   "'"        nil "\\.")
;;              ("#"                     0   "$"        nil)
;;              ("("                     0   ")"        t)
;;              ("\\["                   0   "\\]"      t)
;;              ("{"                     0   "}"        t))

    (shell      ("\""                    0   "\""       nil "\\.")
                ("'"                     0   "'"        nil "\\.")
                ("[<][<]\\([^ \t]+\\)"   1   "^\\1"     nil)
                ("("                     0   ")"        t)
                ("\\["                   0   "\\]"      t)))

  "A list of syntax tables for supported languages.

Each entry in this list is of the form

   (SYMBOL SYNTAX-ENTRY [SYNTAX-ENTRY [...]])

where SYMBOL is used to identify the language in
question and SYNTAX-ENTRY is in format

   (BEGIN-REGEXP NUM-GROUPS END-REGEXP RECURSIVE-P SKIP-REGEXP)

BEGIN-REGEXP is a regular expression matching the beginning of
the syntax construct in question.  NUM-GROUPS indicates how many
groups there are in BEGIN-REGEXP to be substituted in END-REGEXP.

END-REGEXP is a regular expression matching the end of the syntax
construct in question.  It can refer back to one group in
BEGIN-REGEXP using \1. Currently only one group is supported (\2
cannot be used.)

RECURSIVE-P indicates whether other syntax constructs can be
nested within this construct.  RECURSIVE-P is usually false for
comment and constant constructs, such as strings, and usually
true for bracketing constructs.

SKIP-REGEXP is a regular expression that, if matches, means that
END-REGEXP is ignored for that location.  This can be used to
prevent an escaped quote from being interpreted as the closing
quote, for example.")

(defvar adapt-indent-hook-mapping-list
;;   Hook                  Syntax        Variable
  '((c-mode-hook           c/c++/java    c-basic-offset)       ; C
    (c++-mode-hook         c/c++/java    c-basic-offset)       ; C++
    (java-mode-hook        c/c++/java    c-basic-offset)       ; Java
    (jde-mode-hook         c/c++/java    c-basic-offset)       ; Java (JDE)
    (javascript-mode-hook  c/c++/java    c-basic-offset)       ; JavaScript
    (objc-mode-hook        c/c++/java    c-basic-offset)       ; Objective C
    (php-mode-hook         c/c++/java    c-basic-offset)       ; PHP
    (perl-mode-hook        perl          perl-indent-level)    ; Perl
;;  (python-mode-hook      python        py-indent-offset)     ; Python
    (ruby-mode-hook        ruby          ruby-indent-level)    ; Ruby
    (ada-mode-hook         ada           ada-indent)           ; Ada
    (sh-mode-hook          shell         sh-basic-offset)      ; Shell Script
    (pascal-mode-hook      pascal        pascal-indent-level)) ; Pascal
   "A mapping from hook variables to language types.")

;;;-----------------------------------------------------------------
;;; Customization Definitions:

(defgroup adapt-indent nil
  "Transparently adapt to foreign indentation offsets."
  :version "22.0"
  :group 'files
  :group 'convenience)

;;;###autoload
(defcustom adapt-indent-mode nil
  "Toggle adaptive indentation mode.
Setting this variable directly does not take effect;
use either \\[customize] or the function `adapt-indent-mode'."
  :set #'(lambda (symbol value) (funcall symbol (or value 0)))
  :initialize 'custom-initialize-default
  :version "22.0"
  :type    'boolean
  :group   'adapt-indent
  :require 'adapt-indent)

(defcustom adapt-indent-verbosity 1
  "*Verbosity level.

How much adapt-indent should tell you about what it's
doing.  If this is 1, the default, adapt-indent will only
let you know when it adjusts the indentation offset but will be
silent otherwise.  By setting this to 2, it will also tell you
why it didn't adjust the offset.  You might want to try this as a
first measure if you're unhappy with adapt-indent's
actions.  A setting of 3 will output lots of diagnostic
information.  Finally, a setting of 0 keeps adapt-indent
of ever outputting anything."
  :type '(choice (const :tag "Silent" 0)
                 (const :tag "Normal" 1)
                 (const :tag "Verbose" 2)
                 (const :tag "Diagnostics" 3))
  :tag "Verbosity"
  :group 'adapt-indent)

(defcustom adapt-indent-require-confirmation-flag nil
  "*Non-nil means to ask for confirmation before making adjustments.

Whether adapt-indent asks for confirmation whenever it is
about to make any adjustments.  By default, adjustments are made
without your explicit consent because adapt-indent is
already quite conservative and tries to 'do the right thing',
adjustments can be undone easily and they aren't harmful in the
first place.  However, if you feel like it's doing things behind
your back, enable this setting."
  :type 'boolean
  :tag "Require Confirmation"
  :group 'adapt-indent)

(defcustom adapt-indent-min-relevant-lines 5
  "*Minimum number of relevant lines required for a guess to be made.

This check is in place because with a very low number of lines
that can be analyzed, the chances of a wrong guess are rather
high because the sample size is so small.  If you are getting
false positives on small files - i.e. the wrong offset is guessed
- you might want to increase this setting.  On the other hand, if
you are getting false negatives on small files - i.e. no guess is
made on a small file - you might want to decrease it."
  :type 'integer
  :tag "Minimum Number Of Relevant Lines"
  :group 'adapt-indent)

(defcustom adapt-indent-max-relevant-lines 500
  "*Maximum number of relevant lines to be considered in analysis.

This setting is meant to prevent adapt-indent to spend
large amounts of time on analyzing large source files.  In
general, the higher this setting, the more accurate the guess
will be, but the more time adapt-indent will consume when
opening files.  If you have a fast box, you might want to
consider increasing this number.  On the other hand, if you find
that adapt-indent introduces a noticable delay when
opening files, you might want to decrease it."
  :type 'integer
  :tag "Maximum Number Of Relevant Lines"
  :group 'adapt-indent)

(defcustom adapt-indent-min-quality 80.0
  "*Minimum quality for an indentation offset to be accepted.

Percentage (0-100) of lines that are indented by a non-zero
amount of spaces divisible by a given offset required for that
offset to be eligible for guessing.  The default value of 80
means that for instance, an indentation level of 4 will only be
guessed if at least 80% of all lines are indented by an offset
divisible by 4.

This setting essentially controls how lenient or conservative
adapt-indent operates.  If you are getting false positives
- i.e. guess-offset guesses the wrong offset - you might want to
increase this setting.  On the other hand, if you are getting
false negatives - i.e. guess-offset refuses to adjust the offset
- you might want to decrease it."
  :type 'float
  :tag "Minimum Number Of Matching Lines"
  :group 'adapt-indent)

(defcustom adapt-indent-min-superiority 100.0
  "*Minimum percentage the best guess needs to be better than second best.

The percentage (0-100, but higher values than 100 are possible)
that the number of lines matching the best guess must be higher
than the number of lines matching the second best guess in order
for adapt-indent to adjust the offset.  The default value
of 100 means that there must be twice as many lines matching the
best guess than the number of lines matching the second best
guess.

This check is in place to avoid a good guess to be accepted if
there is another, similarly good guess, because in that situation
there is ambiguity and no single reliable guess.  If you are
getting false positives - i.e. adapt-indent guesses the
wrong offset - you might want to increase this setting.  On the
other hand, if you are getting false negatives -
i.e. adapt-indent refuses to adjust the offset - you might
want to decrease it."
  :type 'float
  :tag "Minimum Superiority Of Best Guess"
  :group 'adapt-indent)

(defcustom adapt-indent-max-merge-deviation 20.0
  "*Minimum difference between offsets divisible without remainder.

The percentage of difference in the number of lines that are
matched by two guessed offsets where the larger offset is
divisible by the smaller without remainder for the smaller offset
to be discarded.

This setting is in place because without it, in a file with 1000
lines matching an offset of 4, all of those 1000 lines are also
matching an offset of 2 and a number of stray lines whose offset
is divisible by 2 but not by 4 would confuse adapt-indent
to treat the smaller offset as the better guess.  By discarding
the smaller offset with some tolerance, this problem is avoided.

If you notice that often, sub-offsets are wrongly guessed (e.g. 8
would be the proper offset but 4 is guessed, or 6 would be
correct but 3 is guessed) you might want to decrease this
setting.  On the other hand, if super-offsets are guessed (e.g. 4
would be appropriate, but 8 is guessed) you might want to
increase it."
  :type 'float
  :tag "Maximum Deviation For Sub-Offset Merging"
  :group 'adapt-indent)

(defcustom adapt-indent-ignore-single-chars-flag t
  "*Non-nil means ignore lines containing only a single character.

Whether to treat lines that contain only a single non-whitespace
character as irrelevant for the analysis.  This defaults to t in
order to avoid hanging braces etc.  to skew the results.  Set it
to nil if you are dealing with source files whose indentation
level isn't reliably guessed without those lines."
  :type 'boolean
  :tag "Ignore Single-Character Lines"
  :group 'adapt-indent)

(defcustom adapt-indent-min-matching-indentations 2
  "*Minimum number of distinct levels for an offset to be eligible.

The minimum number of distinct, non-zero indentation levels
matching a given offset required to be present in a file for that
offset to be eligible for guessing.  The default value of 2 means
that for instance, an indentation level of 4 will only be guessed
if some lines are indented at 4 spaces and some at 8; or if some
lines are indented at 12 spaces and some at 20; but not if some
lines are indented at 4 spaces but there are no other lines
indented at an offset divisible by 4.

The default value of 1 effectively disables any such requirement.
If you are getting false positives, you might want to set this to
a higher value such as 2.  However, a value of 2 means that the
offset won't be guessed for files containing only 'flat'
constructs"
  :type 'integer
  :tag "Minimum Depth"
  :group 'adapt-indent)

(defcustom adapt-indent-min-offset 2
  "*Minimum indentation offset that can be guessed.

You usually don't want to tinker with this - setting it to a
higher value than 2 means that the rather common offset of 2 will
no longer be guessed.  Setting it to 1 will likely screw up
adapt-indent algorithms because every line in every source
code matches that indentation level."
  :type 'integer
  :tag "Minimum Guessed Indentation Offset"
  :group 'adapt-indent)

(defcustom adapt-indent-max-offset 8
  "*Maximum indentation offset that can be guessed.

You usually don't want to tinker with this - setting it to a
lower value than 8 means that the (unfortunately) rather common
indentation offset of 8 will no longer be guessed.  Setting it to
a higher value than 8 should not be harmful, but source files
using more than 8 spaces per indentation level are very rare."
  :type 'integer
  :tag "Maximum Guessed Indentation Offset"
  :group 'adapt-indent)

(defcustom adapt-indent-accept-file-function (lambda (filename) t)
  "*Acceptor determining which files are analyzed.

This function will be called for every file adapt-indent would
normally analyze with one argument, the file name.  Only if it
returns a non-nil value analysis will be performed on the file.

By default, all files are analyzed."
  :type 'function
  :tag "Analysed File Inclusion Function"
  :group 'adapt-indent)

(defvar adapt-indent-buffer-language-and-variable)
(make-variable-buffer-local
 'adapt-indent-buffer-language-and-variable)

(defun adapt-indent--replace-in-string (haystack
                                        needle-regexp
                                        replacement)
  "Replace every match in string by constant replacement.
Returns HAYSTACK with every match of NEEDLE-REGEXP replaced by
REPLACEMENT."
  (if (string-match needle-regexp haystack)
      (concat (substring haystack 0 (match-beginning 0))
              replacement
              (substring haystack (match-end 0)))
    haystack))


(defun adapt-indent--skip-to-end-of-match (end-regex
                                           skip-regex
                                           syntax-regex-pairs
                                           multi-line)
  "Place point at the end of the current match.
END-REGEX is a regular expression matching the end.  If
SKIP-REGEX matches though, END-REGEX is ignored.
SYNTAX-REGEX-PAIRS is a list of syntax entries as described for
`adapt-indent-language-syntax-table'.  MULTI-LINE, if
false, constrains the search to the current line."
  (let* ((index-offset 1)
         end-index
         skip-index
         (regexp
          (mapconcat
           (lambda (el) (concat "\\(" el "\\)"))
           (append (when end-regex
                     (setq end-index index-offset)
                     (setq index-offset (1+ index-offset))
                     (list end-regex))
                   (when skip-regex
                     (setq skip-index index-offset)
                     (setq index-offset (1+ index-offset))
                     (list skip-regex))
                   (mapcar (lambda (x) (car x))
                           syntax-regex-pairs))
           "\\|")))
    (while
        (and
         (re-search-forward regexp
                            (unless multi-line
                              (save-excursion (end-of-line) (point))) t)
         (let ((match-index 1))
           (while (null (match-beginning match-index))
             (setq match-index (1+ match-index)))
           (cond
            ((eq match-index end-index) nil)
            ((eq match-index skip-index) t)
            (t
             (let ((matching-syntax-entry
                    (let ((match-count (- match-index index-offset))
                          (syntax-regex-iterator syntax-regex-pairs))
                      (while (> match-count
                                (nth 1 (car syntax-regex-iterator)))
                        (setq match-count
                              (- match-count
                                 (nth 1 (car syntax-regex-iterator)) 1))
                        (setq syntax-regex-iterator
                              (cdr syntax-regex-iterator)))
                      (car syntax-regex-iterator))))
               (adapt-indent--skip-to-end-of-match
                (if (> (nth 1 matching-syntax-entry) 0)
                    (adapt-indent--replace-in-string
                     (nth 2 matching-syntax-entry)
                     "[\\][1]" (match-string-no-properties
                                (1+ match-index)))
                  (nth 2 matching-syntax-entry))
                (nth 4 matching-syntax-entry)
                (when (nth 3 matching-syntax-entry) syntax-regex-pairs)
                t)
               t))))))))

(defun adapt-indent--for-each-indentation (language func user-data)
  "Call a function for each indentation found.
LANGUAGE is used to lookup a syntax table for excluding lines
from the process.  For each line not excluded, FUNC is called
with USER-DATA as its argument and with point on the first
non-whitespace character of the line."
  (save-excursion
    (goto-char (point-min))
    (while (and (re-search-forward "^[ \t]*" nil t)
                (funcall func user-data)
                (progn
                  (adapt-indent--skip-to-end-of-match
                   nil
                   nil
                   (cdr
                    (assoc language
                           adapt-indent-language-syntax-table))
                   nil)
                  (beginning-of-line)
                  (let ((here (point)))
                    (forward-line)
                    (not (eq here (point)))))))))

(defun adapt-indent--calc-histogram (language)
  "Calculate an indendation histogram.

The histogram is calculated for the current buffer using LANGUAGE
to determine which lines to exclude from the histogram."
  (let ((histogram (make-hash-table)))

    (adapt-indent--for-each-indentation
     language
     (lambda (histogram-and-count)
       (when (and (> (current-column) 0)
                  (not (looking-at "$"))
                  (or (not adapt-indent-ignore-single-chars-flag)
                      (save-excursion
                        (forward-char)
                        (not (looking-at "[ \t]*$")))))
         (puthash (current-column)
                  (1+ (gethash (current-column)
                               (car histogram-and-count) 0))
                  (car histogram-and-count))
         (setcdr histogram-and-count (1+ (cdr histogram-and-count))))
       (< (cdr histogram-and-count)
          adapt-indent-max-relevant-lines))
     (cons histogram 0))
    (let ((histogram-list '()) (total-lines 0))
      (maphash (lambda (key value)
                 (setq histogram-list (append histogram-list
                                              (list (list key value))))
                 (setq total-lines (+ total-lines value)))
               histogram)
      (list histogram-list total-lines))))

(defun adapt-indent--analyze-histogram-try-offset (try-offset
                                                   histogram
                                                   total-lines)
  "Return match information for the given offset.
TRY-OFFSET is the offset to try, HISTOGRAM is the previously
calculated indentation histogram, TOTAL-LINES is the total number
of lines for which the histogram was calculated.

Returns a list in the format (TRY-OFFSET, PERCENTAGE,
MATCHING-INDENTATIONS, REJECT-REASON) where TRY-OFFSET is the
offset that was passed in as the first argument, PERCENTAGE is
the percentage of lines (0..1) with indentation levels that are a
multiple of TRY-OFFSET, MATCHING-INDENTATIONS is the number of
distinct indentation levels found that are a multiple of
TRY-OFFSET, and REJECT-REASON, if non-nil, is a string explaining
why TRY-OFFSET should be rejected."
  (let ((total-matching-lines 0)
        (matching-indentations 0))
    (dolist (histogram-entry histogram)
      (when (eq 0 (mod (nth 0 histogram-entry) try-offset))
        (setq total-matching-lines (+ total-matching-lines
                                      (nth 1 histogram-entry)))
        (setq matching-indentations (1+ matching-indentations))))
    (list try-offset
          (/ (float total-matching-lines) total-lines)
          matching-indentations
          (cond
           ((< matching-indentations
               adapt-indent-min-matching-indentations)
            (format "\
rejected: too few distinct matching offsets (%d required)"
                    adapt-indent-min-matching-indentations))
           (t
            nil)))))
  
(defun adapt-indent--analyze (histogram-and-total-lines)
  "Analyze the histogram.

HISTOGRAM-AND-TOTAL-LINES is a tuple with the first item being
the histogram, the second item being the total number of lines
considered in the histogram.

Returns a map with the following entries:

TBD"
  (let* ((analysis
          (let ((try-offset adapt-indent-min-offset)
                unsorted-analysis)
            (while (<= try-offset adapt-indent-max-offset)
              (setq
               unsorted-analysis
               (append unsorted-analysis
                       (list (adapt-indent--analyze-histogram-try-offset
                              try-offset
                              (nth 0 histogram-and-total-lines)
                              (nth 1 histogram-and-total-lines)))))
              (setq try-offset (1+ try-offset)))
            (sort unsorted-analysis (lambda (x y) (> (nth 1 x) (nth 1 y))))))

         (analysis-iterator analysis))

    (while analysis-iterator
      (let ((analysis-entry (car analysis-iterator)))
        (dolist (other-analysis-entry (cdr analysis-iterator))

          (let ((deviation (abs (- (nth 1 other-analysis-entry)
                                    (nth 1 analysis-entry)))))
            (when (and (not (nth 3 analysis-entry))
                       (eq 0 (mod (car other-analysis-entry)
                                  (car analysis-entry)))
                       (> adapt-indent-max-merge-deviation
                          (* 100.0 deviation)))
              (setcdr
               (cdr (cdr analysis-entry))
               (list
                (format "\
merged with offset %s (%.2f%% deviation, limit %.2f%%)"
                 (nth 0 other-analysis-entry)
                 (* 100.0 deviation)
                 adapt-indent-max-merge-deviation)))))))
      (setq analysis-iterator (cdr analysis-iterator)))

    (let* ((best-guess
            (reduce (lambda (carry el) (or carry (unless (nth 3 el) el)))
                    analysis :initial-value nil))
           (second-best-guess
            (reduce (lambda (carry el)
                      (or carry (unless (or (eq el best-guess) (nth 3 el))
                                  el)))
                    analysis :initial-value nil))
           (confidence
            (if best-guess
                (- (nth 1 best-guess)
                   (if second-best-guess
                       (* 2.0 (expt (/ (nth 1 second-best-guess) 2.0) 2))
                     0))
              0))
           (rejected
            (cond
             ((null best-guess)
              "no best guess")
             ((< (* 100.0 (nth 1 best-guess))
                 adapt-indent-min-quality)
              (format "best guess below minimum quality (%f < %f)" (* 100.0 (nth 1 best-guess)) 
                      adapt-indent-min-quality))
             ((and second-best-guess
                   (< (- (/ (* 100.0 (nth 1 best-guess))
                            (nth 1 second-best-guess))
                         100)
                      adapt-indent-min-superiority))
              "best guess not much better than second best guess"))))

      (list (cons :histogram (car histogram-and-total-lines))
            (cons :total-lines (nth 1 histogram-and-total-lines))
            (cons :analysis analysis)
            (cons :best-guess best-guess)
            (cons :second-best-guess second-best-guess)
            (cons :rejected rejected)
            (cons :confidence confidence)))))

(defun adapt-indent-try-set-offset ()
  "Try adjusting the current buffer's indentation offset."
  (when (not adapt-indent-buffer-language-and-variable)
    (error "\
Buffer hasn't been prepared using adapt-indent-setup"))
  (let* ((result
          (adapt-indent--analyze
           (adapt-indent--calc-histogram
            (car adapt-indent-buffer-language-and-variable))))
         (best-guess
          (cdr (assoc :best-guess result)))
         (rejected
          (cdr (assoc :rejected result)))
         (confidence
          (cdr (assoc :confidence result)))
         (best-indent-offset
          (nth 0 best-guess))
         (indent-offset-variable
          (nth 1 adapt-indent-buffer-language-and-variable)))
    (cond
     ((and best-guess
           (not rejected)
           (not (eq (symbol-value indent-offset-variable)
                    best-indent-offset)))
      (when (or (not adapt-indent-require-confirmation-flag)
                (yes-or-no-p
                 (format "Do you want to adjust %s to %s for buffer %s? "
                         indent-offset-variable
                         best-indent-offset
                         (buffer-name))))
        (set (make-local-variable indent-offset-variable) best-indent-offset)
        (when (>= adapt-indent-verbosity 1)
          (message "Note: %s adjusted to %s%s"
                   indent-offset-variable
                   best-indent-offset
                   (if (>= adapt-indent-verbosity 2)
                       (format " (%.0f%% confidence)" (* 100 confidence))
                     "")))
        best-indent-offset))
     (t
      (when (>= adapt-indent-verbosity 2)
        (message "Note: %s not adjusted" indent-offset-variable))
      nil))))

(defun adapt-indent-find-file-hook ()
  "Try adjusting indentation offset when a file is loaded."
  (when (and adapt-indent-mode
             adapt-indent-buffer-language-and-variable
             (funcall adapt-indent-accept-file-function buffer-file-name))
    (adapt-indent-try-set-offset)))

(defun adapt-indent-setup (language-and-variable)
  "Setup adapt-indent per buffer.
Configures `adapt-indent-buffer-language-and-variable' as
a buffer local variable and sets it to LANGUAGE-AND-VARIABLE,
where LANGUAGE is the language to use for the buffer and VARIABLE
is the variable used to configure indentation offset for the
buffer."
  (set (make-local-variable
        'adapt-indent-buffer-language-and-variable)
       language-and-variable))

;;-----------------------------------------------------------------
;; Diagnostic functions

(defun adapt-indent-diagnosis ()
  "Guess indentation for the current buffer and output diagnostics."
  (interactive)
  (require 'benchmark)
  (let* (result
         (time-for-analysis
          (benchmark-elapse
            (setq result
                  (adapt-indent--analyze
                   (adapt-indent--calc-histogram
                    (car adapt-indent-buffer-language-and-variable))))))         
         (histogram (cdr (assoc :histogram result)))
         (total-lines (cdr (assoc :total-lines result)))
         (analysis (cdr (assoc :analysis result)))
         (best-guess (cdr (assoc :best-guess result)))
         (second-best-guess (cdr (assoc :second-best-guess result)))
         (confidence (cdr (assoc :confidence result))))

    (with-output-to-temp-buffer "*adapt-indent-debug*"
      (princ (format "\nGuessing offset for %s\n\n"
                     (or (buffer-file-name) (buffer-name))))
      (princ (format "Elapsed time for analysis: %.3f seconds\n\n"
                     time-for-analysis))
      (princ (format "Total relevant lines: %d out of %d (limit: %d)\n"
                     total-lines
                     (line-number-at-pos (point-max))
                     adapt-indent-max-relevant-lines))
      (if (< total-lines
             adapt-indent-min-relevant-lines)
          (princ
           (format "\
\n\
Analysis cancelled: not enough relevant lines (%d required) - not \
modifying offset\n\n"
                   adapt-indent-min-relevant-lines))
        (princ "\nHistogram:\n\n")
        (princ
         (eval
          (append '(concat)
                  (mapcar (lambda (x)
                            (format "  %4dx %3d spaces\n"
                                    (nth 1 x)
                                    (nth 0 x)))
                          (sort
                           histogram
                           (lambda (x y)
                             (or (not y)
                                 (and x (< (car x) (car y))))))))))
        (princ "\nAnalysis:\n\n")
        (princ
         (eval
          (append '(concat)
                  (mapcar
                   (lambda (analysis-entry)
                     (format "\
  offset %d works for %6.2f%% of relevant lines, matching %d \
distinct offsets - %s\n"
                             (nth 0 analysis-entry)
                             (* 100.0 (nth 1 analysis-entry))
                             (nth 2 analysis-entry)
                             (or (nth 3 analysis-entry) "CONSIDERED")))
                   analysis))))
        (princ "\nSummary:\n\n")
            
        (princ
         (format "\
  Best guess is offset %d with %.2f%% matching lines \(%.2f%% \
required)\n"
                 (nth 0 best-guess)
                 (* 100.0 (nth 1 best-guess))
                 adapt-indent-min-quality))
        
        (if second-best-guess
            (progn
              (princ
               (format "\
  Second best guess is offset %d with %.2f%% matching lines\n"
                       (nth 0 second-best-guess)
                       (* 100.0 (nth 1 second-best-guess))))
              (princ
               (format "\
  Best guess is %.2f%% better than second best guess (%.2f%% \
required)\n"
                       (- (/ (* 100.0 (nth 1 best-guess))
                             (nth 1 second-best-guess)) 100)
                       adapt-indent-min-superiority)))
          (princ
           (format "  There is no second best guess\n")))

        (princ "\nConclusion:\n\n")

        (princ (format "\
  Guessed offset %s with %.0f%% confidence.\n"
                       (nth 0 best-guess)
                       (* 100.0 confidence)))))))


;; The following is from font-lock.el
(defmacro save-buffer-state (varlist &rest body)
  "Bind variables according to VARLIST and eval BODY restoring buffer state."
  (declare (indent 1) (debug let))
  (let ((modified (make-symbol "modified")))
    `(let* ,(append varlist
                    `((,modified (buffer-modified-p))
                      (buffer-undo-list t)
                      (inhibit-read-only t)
                      (inhibit-point-motion-hooks t)
                      (inhibit-modification-hooks t)
                      deactivate-mark
                      buffer-file-name
                      buffer-file-truename))
       (progn
         ,@body)
       (unless ,modified
         (restore-buffer-modified-p nil)))))

(defun adapt-indent-highlight ()
  "Highlight non-excluded indentation in the current buffer."
  (interactive)
  (save-buffer-state nil
    (adapt-indent--for-each-indentation
     (car adapt-indent-buffer-language-and-variable)
     (lambda (histogram)
       (put-text-property (save-excursion (beginning-of-line) (point))
                          (point)
                          'face '(background-color . "red"))
       t)
     nil)))

;;-----------------------------------------------------------------
;; Tests

(eval-when-compile

  (defun adapt-indent-functional-test (args)
    (with-temp-buffer 
      (make-local-variable 'adapt-indent-verbosity)
      (setq adapt-indent-verbosity 0)
      (make-local-variable 'adapt-indent-min-relevant-lines)
      (setq adapt-indent-min-relevant-lines 3)
      (insert (cdr (assoc :buffer-contents args)))
      (let ((language-and-variable
             (cdr (assoc (cdr (assoc :mode-hook args))
                         adapt-indent-hook-mapping-list))))
        (when (not language-and-variable)
          (error "Unknown mode-hook `%s'" (cdr (assoc :mode-hook args))))
        (adapt-indent-setup language-and-variable)
        (let* ((result
                (adapt-indent--analyze
                 (adapt-indent--calc-histogram
                  (car adapt-indent-buffer-language-and-variable))))
               (best-guess
                (cdr (assoc :best-guess result)))
               (rejected
                (cdr (assoc :rejected result)))
               (confidence
                (cdr (assoc :confidence result)))
               (best-indent-offset
                (nth 0 best-guess))
               (indent-offset-variable
                (nth 1 adapt-indent-buffer-language-and-variable))
               (expected-offset
                (cdr (assoc :expected-offset args))))
          (if expected-offset
              (assert (eq nil rejected) t)
            (assert (not (eq nil rejected)) t))
          (assert (eq expected-offset
                      best-indent-offset) t)))))

  (defun adapt-indent-test-rec-directory-files
    (directory filename-pattern function)
    (let ((files
           (directory-files directory t)))
      (mapc (lambda (file)
              (when (and (not (file-directory-p file))
                         (string-match filename-pattern
                                       (file-name-nondirectory file)))
                (funcall function file)))
            files)))

  (defun adapt-indent-bulk-test (args)
    (princ (format "Performing bulk test on %s\n"
                   (cdr (assoc :directory args))))
    (setq adapt-indent-verbosity 0)
  
    (adapt-indent-test-rec-directory-files
     (cdr (assoc :directory args))
     (cdr (assoc :filename-pattern args))
     (lambda (file)
       (princ
        (format
         "file %s -> %s\n"
         file
         (with-temp-buffer
           "*adapt-indent-test-file*"
           (insert-file-contents-literally file)
           (set (make-local-variable
                 'adapt-indent-buffer-language-and-variable)
                (cdr (assoc (cdr (assoc :mode-hook args))
                            adapt-indent-hook-mapping-list)))
           (adapt-indent-try-adjust)))))))

  ;; Functional tests

  (adapt-indent-functional-test
   '((:buffer-contents . "foo")
     (:mode-hook . sh-mode-hook)
     (:expected-offset . nil)))

  (adapt-indent-functional-test
   '((:buffer-contents . "\
aa
    aa
        aa")
     (:mode-hook . sh-mode-hook)
     (:expected-offset . 4)))

  (adapt-indent-functional-test
   '((:buffer-contents . "\
aa /*foo
    bar
    blah*/
   aa
      aa")
     (:mode-hook . c-mode-hook)
     (:expected-offset . 3)))

  (when nil ;; disabled
    (with-output-to-temp-buffer "*adapt-indent-test-results*"
      (adapt-indent-bulk-test
       '((:directory . "\
/Volumes/IOMEGA_HDD/guess-offset-test/phpMyAdmin-2.10.0.2-english/")
         (:filename-pattern . ".php$")
         (:mode-hook . php-mode-hook)
         (:expected-offset . 4)
         (:min-confidence . 80))))))


;;-----------------------------------------------------------------
;; Installation

(defun adapt-indent-unload-hook ()
  "Unload adapt-indent."
  (adapt-indent-mode 0))
(add-hook 'adapt-indent-unload-hook 'adapt-indent-unload-hook)

(defadvice hack-one-local-variable
  (before adapt-indent-advise-hack-one-local-variable activate)
  "Disable adapt-indent if offset explicitly set."
  (when (and adapt-indent-mode
             (eql (nth 1 adapt-indent-buffer-language-and-variable)
                  (ad-get-arg 0)))
    (setq adapt-indent-buffer-language-and-variable nil)
    (when (>= adapt-indent-verbosity 1)
      (message "Indentation offset set with File Variable; not adjusted"))))

; Install global find-file-hook
(add-hook 'find-file-hook 'adapt-indent-find-file-hook)

; Install one buffer local hook per supported major mode
(mapcar
 (lambda (adapt-indent-hook-mapping)
   (add-hook (nth 0 adapt-indent-hook-mapping)
             `(lambda () (adapt-indent-setup
                          (quote ,(cdr adapt-indent-hook-mapping))))
             t))
 adapt-indent-hook-mapping-list)

; Register minor mode
(add-to-list 'minor-mode-alist '(adapt-indent-mode " Adapt-Indent"))

(provide 'adapt-indent)

;;; adapt-indent.el ends here
