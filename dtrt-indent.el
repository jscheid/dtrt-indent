;;; dtrt-indent.el --- Adapt to foreign indentation offsets

;; Copyright (C) 2003, 2007, 2008 Julian Scheid
;; Copyright (C) 2014-2021 Reuben Thomas

;; Author: Julian Scheid <julians37@googlemail.com>
;; Maintainer: Reuben Thomas <rrt@sc3d.org>
;; Version: 1.4
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
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301 USA

;;; Commentary:

;; A minor mode that guesses the indentation offset and
;; `indent-tabs-mode' originally used for creating source code files and
;; transparently adjusts the corresponding settings in Emacs, making it
;; more convenient to edit others' files.
;;
;; This hooks into many major modes - c-mode, java-mode and ruby-mode, to
;; name but a few - and makes an educated guess on which offset is
;; appropriate by analyzing indentation levels in the file.  Modes that have
;; their own indentation offset guessing, such as python-mode, are not dealt
;; with.  In modes based on SMIE, dtrt-indent delegates to smie-config-guess.
;;
;; Heuristics are used to estimate the proper indentation offset and
;; therefore this system is not infallible, however adjustments will
;; only be made if the guess is considered reliable.  This way it
;; should leave you off no worse than before.
;;
;; To install, M-x customize-variable dtrt-indent-mode, and turn it on.
;;
;; The default settings have been carefully chosen and tested to work
;; reliably on a wide range of source files.  However, if it doesn't work
;; for you they can be fine tuned using M-x customize-group dtrt-indent.
;; You can use `dtrt-indent-diagnosis' to see dtrt-indent's
;; measurements, `dtrt-indent-highlight' to show indentation that was
;; considered,and `dtrt-indent-undo' to undo any changes it makes.
;;
;;
;; Heuristics
;;
;; We now describe the inner workings of dtrt-indent and how it arrives
;; at a conclusion on whether or not to change the indentation settings,
;; and to which value.
;;
;; Lines Analyzed
;;
;; In order to limit performance degradation due to the analysis, only a
;; fixed number of lines will be analyzed.  If the size of the file is
;; less than this number of lines, the whole file will be analyzed;
;; otherwise, the given number of lines at the beginning of the file are
;; analyzed.
;;
;; Certain lines are ignored during analysis:
;;
;; * Empty lines.
;; * Lines that are not indented (indentation offset 0).
;; * Lines that are the continuation of a multi-line comment or a
;;   multi-line statement or expression.
;; * Lines that only contain a single character can be ignored; by
;;   default, however, they are included.
;;
;; If, after ignoring any lines that are not eligible, the number of
;; relevant lines is smaller than a given threshold then the file is
;; treated as not fit for analysis and no guess will be made.
;;
;; Configuration settings used at this stage:
;; `dtrt-indent-min-relevant-lines', `dtrt-indent-max-lines',
;; `dtrt-indent-ignore-single-chars-flag'
;;
;; Histogram Generation
;;
;; For the remaining lines - those eligible within the fixed range - a
;; histogram is generated.  The histogram informs dtrt-indent about how
;; many lines are indented with one space, how many with two spaces, how
;; many with three spaces, etc.
;;
;; Offset Assessment
;;
;; Using the histogram, dtrt-indent determines for each of the potential
;; indentation offsets (by default, 2 through 8) how many lines are
;; indented with a multiple of that offset.
;;
;; Offsets for which the histogram doesn't contain enough distinct
;; indentations might be ignored; by default, however, a single
;; indentation per offset is accepted.
;;
;; After this step, dtrt-indent has a map of probabilities for each of
;; the potential offsets.
;;
;; Configuration settings used at this stage: `dtrt-indent-min-offset',
;; `dtrt-indent-max-offset', `dtrt-indent-min-matching-indentations'
;;
;; Offset Merging
;;
;; As a next step, offsets that are a factor of another offset with
;; similar probability are discarded; this is necessary because in a file
;; that has been indented with, say, 4 spaces per level, 2 spaces per
;; level could otherwise be wrongly guessed.
;;
;; Configuration settings used at this stage:
;; `dtrt-indent-max-merge-deviation'
;
;; Final Evaluation
;;
;; Finally, dtrt-indent looks at the highest probability of all
;; potential offsets; if that probablity is below a given threshold, the
;; guess is deemed unreliable and no settings are changed.
;;
;; If the analysis yielded a best guess that exceeds the absolute
;; threshold, that guess is deemed reliable and the indentation setting
;; will be modified.
;;
;; Configuration settings used at this stage: `dtrt-indent-min-quality'.
;;
;; `indent-tabs-mode' Setting
;;
;; For determining hard vs. soft tabs, dtrt-indent counts the number of
;; lines out of the eligible lines in the fixed segment that are
;; indented using hard tabs, and the number of lines indented using
;; spaces.  If either count is significantly higher than the other count,
;; `indent-tabs-mode' will be modified.
;;
;; Configuration settings used at this stage:
;; `dtrt-indent-min-soft-tab-superiority',
;; `dtrt-indent-min-hard-tab-superiority'
;;
;; Files not touched by dtrt-indent:
;;
;; - Files that specify the corresponding variable
;;   (e.g. c-basic-offset) as a File Variable.
;;
;; - Files that specify dtrt-indent-mode: 0 as a File Variable.
;;
;; - Files with a major mode that dtrt-indent doesn't hook into.
;;
;; - Files for which the indentation offset cannot be guessed
;;   reliably.
;;
;; - Files for which `dtrt-indent-explicit-offset' is true; this can be
;; - used in `.dir-locals.el' files, for example.
;;
;; Limitations:
;;
;; - dtrt-indent can't deal well with files that use variable
;;   indentation offsets, e.g. files that use varying indentation
;;   based on the outer construct.
;;
;; - dtrt-indent currently only supports a limited number of languages
;;   (major-modes).
;;
;; - dtrt-indent only guesses the indendation offset, not the
;;   indentation style.  For instance, it does not detect whether a
;;   C-like file uses hanging braces or not.
;;
;; - dtrt-indent can't deal well with files that mix hard tabs with
;; - spaces for indentation.
;;
;; TODO:
;;
;; - verbose and diagnostics messages
;; - make sure variable documentation match their function
;; - make sure defaults are sensible
;; - bulk (real world) tests
;; - functional tests
;; - unit tests

;;; Code:

;;;###autoload
(define-minor-mode dtrt-indent-mode
  "Toggle dtrt-indent mode.
With no argument, this command toggles the mode.  Non-null prefix
argument turns on the mode.  Null prefix argument turns off the
mode.

When dtrt-indent mode is enabled, the proper indentation offset
and `indent-tabs-mode' will be guessed for newly opened files and
adjusted transparently."
  :lighter " dtrt-indent"
  :group 'dtrt-indent
  (if dtrt-indent-mode
      (if (and (featurep 'smie) (not (null smie-grammar)) (not (eq smie-grammar 'unset)))
          (progn
            (when (null smie-config--buffer-local) (smie-config-guess))
            (when dtrt-indent-run-after-smie
              (dtrt-indent-try-set-offset)))
        (dtrt-indent-try-set-offset))
    (dtrt-indent-undo)))

;;;###autoload
(define-globalized-minor-mode dtrt-indent-global-mode dtrt-indent-mode
  (lambda ()
    ;; javascript-mode is an alias for js-mode, so derived-mode-p does not
    ;; detect it is derived from 'prog-mode (Emacs bug #46331)
    (when (derived-mode-p 'prog-mode 'text-mode 'javascript-mode)
      (dtrt-indent-mode))))

(defvar dtrt-indent-language-syntax-table
  '((c/c++/java ("\""                    0   "\""       nil "\\\\.")
                ("'"                     0   "'"        nil "\\\\.")
                ("/\\*"                  0   "\\*/"     nil)
                ("//"                    0   "$"        nil)
                ("("                     0   ")"        t)
                ("\\["                   0   "\\]"      t))

    ;; Same as c/c++/java but ignore function call arguments, to cope with
    ;; modules defined entirely within a function call, e.g. AMD style
    (javascript ("\""                    0   "\""       nil "\\\\.")
                ("'"                     0   "'"        nil "\\\\.")
                ("/\\*"                  0   "\\*/"     nil)
                ("//"                    0   "$"        nil)
                ("/\\(.*\\)"             1   "\\1/"     nil)
                ("\\["                   0   "\\]"      t))

    (perl       ("\""                    0   "\""       nil "\\\\.")
                ("'"                     0   "'"        nil "\\\\.")
                ("/"                     0   "/"        nil "\\\\.")
                ("#"                     0   "$"        nil)
                ("("                     0   ")"        t)
                ("\\["                   0   "\\]"      t))

    (lua        ("\""                    0   "\""       nil "\\\\.")
                ("'"                     0   "'"        nil "\\\\.")
                ("--"                    0   "$"        nil)
                ("("                     0   ")"        t)
                ("\\[\\(=+\\)\\["        1   "\\]\\1\\]"     nil)
                ("{"                     0   "}"        t))

    (ruby       ("\""                    0   "\""       nil "\\\\.")
                ("'"                     0   "'"        nil "\\\\.")
                ("/"                     0   "/"        nil "\\\\.")
                ("#"                     0   "$"        nil)
                ("("                     0   ")"        t)
                ("\\["                   0   "\\]"      t)
                ("{"                     0   "}"        t))

    (ada        ("\""                    0   "\""       nil "\\\\.")
                ("--"                    0   "$"        nil)
                ("("                     0   ")"        t)
                ("\\["                   0   "\\]"      t)
                ("{"                     0   "}"        t))

    ;; The standard Erlang style is to indent code inside a block
    ;; relative to the token that opened the block.  For example:
    ;;
    ;; bar(X) ->
    ;;   {A, B} = case X of
    ;;              true ->
    ;;                {alpha, [beta,
    ;;                         gamma]}
    ;;            end.
    ;;
    ;; Thus it is best to ignore the code inside these block
    ;; constructs when determining the indent offset.
    (erlang     ("\""                    0   "\""       nil "\\\\.")
                ;; next pattern avoids error on git merge conflict lines
                ("[<][<][<]"             0   "$"        nil)
                ("[<][<]"                0   "[>][>]"   nil)
                ("%"                     0   "$"        nil)
                ("^-"                    0   "\\."      nil)
                ("{"                     0   "}"        t)
                ("\\["                   0   "\\]"      t)
                ("("                     0   ")"        t)
                ("\\_<\\(?:begin\\|case\\|fun\\|if\\|receive\\|try\\)\\_>"
                                         0   "\\_<end\\_>" t))

    (css        ("\""                    0   "\""       nil "\\\\.")
                ("'"                     0   "'"        nil "\\\\.")
                ("/\\*"                  0   "\\*/"   nil))

    (sgml       ("[<]!\\[(CDATA|IGNORE|RCDATA)\\["
                                         0   "\\]\\][>]"     nil)
                ("[<]!--"                0   "[^-]--[>]"  nil))

    (cmake      ("\""                    0   "\""        nil "\\\\.")
                ("#\\[\\(=*\\)\\["       1   "\\]\\1\\]" nil)
                ("#"                     0   "$"         nil))

    (default    ("\""                    0   "\""       nil "\\\\.")))

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

(defvar dtrt-indent-hook-mapping-list
;;   Mode            Syntax        Variable
  '((c-mode          c/c++/java    c-basic-offset)       ; C
    (c++-mode        c/c++/java    c-basic-offset)       ; C++
    (d-mode          c/c++/java    c-basic-offset)       ; D
    (java-mode       c/c++/java    c-basic-offset)       ; Java
    (jde-mode        c/c++/java    c-basic-offset)       ; Java (JDE)
    (js-mode         javascript    js-indent-level)      ; JavaScript
    (js2-mode        javascript    js2-basic-offset)     ; JavaScript-IDE
    (js3-mode        javascript    js3-indent-level)     ; JavaScript-IDE
    (json-mode       javascript    js-indent-level)      ; JSON
    (lua-mode        lua           lua-indent-level)     ; Lua
    (objc-mode       c/c++/java    c-basic-offset)       ; Objective C
    (php-mode        c/c++/java    c-basic-offset)       ; PHP
    (perl-mode       perl          perl-indent-level)    ; Perl
    (cperl-mode      perl          cperl-indent-level)   ; Perl
    (raku-mode       perl          raku-indent-offset)   ; Perl6/Raku
    (erlang-mode     erlang        erlang-indent-level)  ; Erlang
    (ada-mode        ada           ada-indent)           ; Ada
    (sgml-mode       sgml          sgml-basic-offset)    ; SGML
    (nxml-mode       sgml          nxml-child-indent)    ; XML
    (pascal-mode     pascal        pascal-indent-level)  ; Pascal
    (typescript-mode javascript    typescript-indent-level) ; Typescript
    (protobuf-mode   c/c++/java    c-basic-offset)       ; Protobuf
    (plantuml-mode   default       plantuml-indent-level) ; PlantUML
    (pug-mode        default       pug-tab-width)         ; Pug
    (cmake-mode      cmake         cmake-tab-width)       ; CMake

    ;; Modes that use SMIE if available
    (sh-mode         default       sh-basic-offset)      ; Shell Script
    (ruby-mode       ruby          ruby-indent-level)    ; Ruby
    (enh-ruby-mode   ruby          enh-ruby-indent-level); Ruby
    (crystal-mode    ruby          crystal-indent-level) ; Crystal (Ruby)
    (css-mode        css           css-indent-offset)    ; CSS
    (rust-mode       c/c++/java    rust-indent-offset)   ; Rust
    (rustic-mode     c/c++/java    rustic-indent-offset) ; Rust
    (scala-mode      c/c++/java    scala-indent:step)    ; Scala

    (default         default       standard-indent))     ; default fallback
   "A mapping from hook variables to language types.")

;;-----------------------------------------------------------------
;; Customization Definitions:

(defgroup dtrt-indent nil
  "Transparently adapt to foreign indentation offsets."
  :version "22.0"
  :group 'files
  :group 'convenience)

;;;###autoload
(defcustom dtrt-indent-mode nil
  "Toggle adaptive indentation mode.
Setting this variable directly does not take effect;
use either \\[customize] or the function `dtrt-indent-mode'."
  :set #'(lambda (symbol value) (funcall symbol (or value 0)))
  :initialize 'custom-initialize-default
  :version "22.0"
  :type    'boolean
  :group   'dtrt-indent
  :require 'dtrt-indent)

(defcustom dtrt-indent-verbosity 1
  "*Verbosity level.

How much dtrt-indent should tell you about what it's doing.  If
this is 1, the default, dtrt-indent will only let you know when
it adjusts the indentation offset but will be silent otherwise.
By setting this to 2 it will also tell you why it didn't adjust
the offset.  You might want to try this as a first measure if
you're unhappy with dtrt-indent's actions.  A setting of 3 will
output lots of diagnostic information.  Finally, a setting of 0
keeps dtrt-indent of ever outputting anything."
  :type '(choice (const :tag "Silent" 0)
                 (const :tag "Normal" 1)
                 (const :tag "Verbose" 2)
                 (const :tag "Diagnostics" 3))
  :tag "Verbosity"
  :group 'dtrt-indent)

(defcustom dtrt-indent-require-confirmation-flag nil
  "*Non-nil means to ask for confirmation before making adjustments.

Whether dtrt-indent asks for confirmation whenever it is about to
make any adjustments.  By default, adjustments are made without
your explicit consent because dtrt-indent is already quite
conservative and tries to 'do the right thing', adjustments can
be undone easily, and they aren't harmful in the first place.
However, if you feel like it's doing things behind your back
you should enable this setting."
  :type 'boolean
  :tag "Require Confirmation"
  :group 'dtrt-indent)

(defcustom dtrt-indent-hook-generic-mapping-list
;;   Key variable    Value variable
  '((evil-mode       evil-shift-width))  ; evil
  "A mapping from hook variables to indentation variables.
For each true key variable, its value variable is set to the same
indentation offset as the variable in `dtrt-indent-hook-mapping-list'
(e.g., `c-basic-offset').  Every pair in the list is processed.  To
disable processing of any one pair, remove the pair from the list.
Processing the list obeys `dtrt-indent-require-confirmation-flag'.

The key can be any variable.  This list is used for cases such as when
a minor-mode defines a variable to control its own indentation
functionality (e.g. `evil-mode' using `evil-shift-width'), so the
value variable must updated in addition to the syntax indentation
variable."
  :type '(alist :key-type variable
                :value-type (group variable))
  :group 'dtrt-indent)

(defcustom dtrt-indent-run-after-smie nil
  "*Non-nil means to run dtrt-indent even in modes using SMIE.

Normally, dtrt-indent will detect SMIE-based modes and delegate
to `smie-config-guess'.  However, dtrt-indent configures some
variables that SMIE does not (e.g. the contents of
`dtrt-indent-hook-generic-mapping-list'), so you may want to run
dtrt-indent even in SMIE-based modes.  You can do so by enabling
this setting."
  :type 'boolean
  :tag "Run dtrt-indent After SMIE"
  :group 'dtrt-indent)

(defcustom dtrt-indent-min-relevant-lines 2
  "*Minimum number of relevant lines required for a guess to be made.

This check is in place because with a very low number of lines
that can be analyzed the chances of a wrong guess are rather
high because the sample size is so small.  If you are getting
false positives with small files - i.e. the wrong offset is guessed
- you might want to increase this setting.  On the other hand, if
you are getting false negatives on small files - i.e. no guess is
made on a small file - you might want to decrease it."
  :type 'integer
  :tag "Minimum Number Of Relevant Lines"
  :group 'dtrt-indent)

(defcustom dtrt-indent-max-lines 5000
  "*Maximum number of lines to be considered in analysis.

This setting is meant to prevent dtrt-indent from spending large
amounts of time on analyzing large source files.  In general, the
higher this setting, the more accurate the guess will be but the
more time dtrt-indent will consume when opening files."
  :type 'integer
  :tag "Maximum Number Of Lines"
  :group 'dtrt-indent)

(defcustom dtrt-indent-min-quality 80.0
  "*Minimum quality for an indentation offset to be accepted.

Percentage (0-100) of lines that are indented by a non-zero
amount of spaces divisible by a given offset required for that
offset to be eligible for guessing.  A value of 80 means that for
instance, an indentation level of 4 will only be guessed if at
least 80% of all lines are indented by an offset divisible by 4.

This setting essentially controls how lenient or conservative
dtrt-indent operates.  If you are getting false positives -
i.e. guess-offset guesses the wrong offset - you might want to
increase this setting.  On the other hand, if you are getting
false negatives - i.e. guess-offset refuses to adjust the offset
- you might want to decrease it."
  :type 'float
  :tag "Minimum Number Of Matching Lines"
  :group 'dtrt-indent)

(defcustom dtrt-indent-min-soft-tab-superiority 300.0
  "*Minimum percentage soft-tab lines need to outnumber hard-tab ones.

TBD"
  :type 'float
  :tag "Minimum Superiority Of Soft Tabs"
  :group 'dtrt-indent)

(defcustom dtrt-indent-min-hard-tab-superiority 300.0
  "*Minimum percentage hard-tab lines need to outnumber soft-tab ones.

TBD"
  :type 'float
  :tag "Minimum Superiority Of Hard Tabs"
  :group 'dtrt-indent)

(defcustom dtrt-indent-max-merge-deviation 20.0
  "*Minimum difference between offsets divisible without remainder.

The percentage of difference in the number of lines that are
matched by two guessed offsets where the larger offset is
divisible by the smaller without remainder for the smaller offset
to be discarded.

This setting is in place because without it, in a file with 1000
lines matching an offset of 4, all of those 1000 lines are also
matching an offset of 2 and a number of stray lines whose offset
is divisible by 2 but not by 4 would confuse dtrt-indent to treat
the smaller offset as the better guess.  By discarding the
smaller offset with some tolerance, this problem is avoided.

If you notice that often, sub-offsets are wrongly guessed (e.g. 8
would be the proper offset but 4 is guessed, or 6 would be
correct but 3 is guessed) you might want to decrease this
setting.  On the other hand, if super-offsets are guessed (e.g. 4
would be appropriate, but 8 is guessed) you might want to
increase it."
  :type 'float
  :tag "Maximum Deviation For Sub-Offset Merging"
  :group 'dtrt-indent)

(defcustom dtrt-indent-ignore-single-chars-flag nil
  "*Non-nil means ignore lines containing only a single character.

Whether to treat lines that contain only a single non-whitespace
character as irrelevant for the analysis.  Set this to t in to
prevent hanging braces etc. from skewing the results.  Set it to
nil if you are dealing with source files whose indentation level
isn't reliably guessed without those lines."
  :type 'boolean
  :tag "Ignore Single-Character Lines"
  :group 'dtrt-indent)

(defcustom dtrt-indent-min-matching-indentations 1
  "*Minimum number of distinct levels for an offset to be eligible.

The minimum number of distinct, non-zero indentation levels
matching a given offset required to be present in a file for that
offset to be eligible for guessing.  A value of 2 means that for
instance, an indentation level of 4 will only be guessed if some
lines are indented at 4 spaces and some at 8; or if some lines
are indented at 12 spaces and some at 20; but not if some lines
are indented at 4 spaces but there are no other lines indented at
an offset divisible by 4.

The default value of 1 effectively disables any such requirement.
If you are getting false positives, you might want to set this to
a higher value such as 2.  However, a value of 2 means that the
offset won't be guessed for files containing only 'flat'
constructs"
  :type 'integer
  :tag "Minimum Depth"
  :group 'dtrt-indent)

(defcustom dtrt-indent-min-offset 2
  "*Minimum indentation offset that can be guessed.

You usually don't want to tinker with this - setting it to a
higher value than 2 means that the rather common offset of 2 will
no longer be guessed.  Setting it to 1 will likely screw up
dtrt-indent algorithms because every line in every source code
matches that indentation level."
  :type 'integer
  :tag "Minimum Guessed Indentation Offset"
  :group 'dtrt-indent)

(defcustom dtrt-indent-max-offset 8
  "*Maximum indentation offset that can be guessed.

You usually don't want to tinker with this - setting it to a
lower value than 8 means that the (unfortunately) rather common
indentation offset of 8 will no longer be guessed.  Setting it to
a higher value than 8 should not be harmful, but source files
using more than 8 spaces per indentation level are very rare."
  :type 'integer
  :tag "Maximum Guessed Indentation Offset"
  :group 'dtrt-indent)

(defvar dtrt-indent-original-indent)
(make-variable-buffer-local
 'dtrt-indent-original-indent)

(defvar dtrt-indent-explicit-offset)
(make-variable-buffer-local
 'dtrt-indent-explicit-offset)

(defvar dtrt-indent-explicit-tab-mode)
(make-variable-buffer-local
 'dtrt-indent-explicit-tab-mode)

(defun dtrt-indent--replace-in-string (haystack
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


(defun dtrt-indent--skip-to-end-of-match (end-regex
                                           skip-regex
                                           syntax-regex-pairs
                                           multi-line)
  "Place point at the end of the current match.
END-REGEX is a regular expression matching the end.  If
SKIP-REGEX matches though, END-REGEX is ignored.
SYNTAX-REGEX-PAIRS is a list of syntax entries as described for
`dtrt-indent-language-syntax-table'.  MULTI-LINE, if false,
constrains the search to the current line."
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
         (let ((match-index 1)
               (match-count (/ (length (match-data)) 2)))
           (while (and (<= match-index match-count)
                       (null (match-beginning match-index)))
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
               (dtrt-indent--skip-to-end-of-match
                (if (> (nth 1 matching-syntax-entry) 0)
                    (dtrt-indent--replace-in-string
                     (nth 2 matching-syntax-entry)
                     "[\\][1]" (regexp-quote
				(match-string-no-properties
				 (1+ match-index))))
                  (nth 2 matching-syntax-entry))
                (nth 4 matching-syntax-entry)
                (when (nth 3 matching-syntax-entry) syntax-regex-pairs)
                t)
               t))))))))

(defun dtrt-indent--for-each-indentation (language func user-data)
  "Call a function for each indentation found.
LANGUAGE is used to lookup a syntax table for excluding lines
from the process.  For each line not excluded, FUNC is called
with USER-DATA as its argument and with point on the first
non-whitespace character of the line."
  (save-excursion
    (let ((case-fold-search nil))
      (goto-char (point-min))
      (while (and (re-search-forward "^[ \t]*" nil t)
                  (funcall func user-data)
                  (progn
                    (dtrt-indent--skip-to-end-of-match
                     nil
                     nil
                     (cdr
                      (assoc language
                             dtrt-indent-language-syntax-table))
                     nil)
                    (beginning-of-line)
                    (let ((here (point)))
                      (forward-line)
                      (not (eq here (point))))))))))

(defun dtrt-indent--calc-histogram (language)
  "Calculate an indendation histogram.

The histogram is calculated for the current buffer using LANGUAGE
to determine which lines to exclude from the histogram."
  (let ((histogram (make-hash-table))
        (hard-tab-line-count 0)
        (soft-tab-line-count 0)
        (line-count 0))

    (dtrt-indent--for-each-indentation
     language
     (lambda (histogram-and-count)
       (setq line-count (1+ line-count))
       (when (and (> (current-column) 0)
                  (not (looking-at "$"))
                  (or (not dtrt-indent-ignore-single-chars-flag)
                      (save-excursion
                        (forward-char)
                        (not (looking-at "[ \t]*$")))))
         (puthash (current-column)
                  (1+ (gethash (current-column)
                               (car histogram-and-count) 0))
                  (car histogram-and-count))
         (beginning-of-line)
         (if (looking-at "[\t]+")
             (setq hard-tab-line-count (1+ hard-tab-line-count))
           (setq soft-tab-line-count (1+ soft-tab-line-count)))
         (setcdr histogram-and-count (1+ (cdr histogram-and-count))))
       (< line-count dtrt-indent-max-lines))
     (cons histogram 0))
    (let ((histogram-list '()) (total-lines 0))
      (maphash (lambda (key value)
                 (setq histogram-list (append histogram-list
                                              (list (list key value))))
                 (setq total-lines (+ total-lines value)))
               histogram)
      (list histogram-list
            total-lines
            hard-tab-line-count
            soft-tab-line-count))))

(defun dtrt-indent--analyze-histogram-try-offset (try-offset
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
               dtrt-indent-min-matching-indentations)
            (format "\
rejected: too few distinct matching offsets (%d required)"
                    dtrt-indent-min-matching-indentations))
           (t
            nil)))))

(defun dtrt-indent--search-hook-mapping (mode)
  "Search hook-mapping for MODE or its derived-mode-parent."
  (if mode
      (or (assoc mode dtrt-indent-hook-mapping-list)
          (dtrt-indent--search-hook-mapping (get mode 'derived-mode-parent))
          (assoc 'default dtrt-indent-hook-mapping-list))))

(defun dtrt-indent--analyze (histogram-and-total-lines)
  "Analyze the histogram.

HISTOGRAM-AND-TOTAL-LINES is a tuple with the first item being
the histogram, the second item being the total number of lines
considered in the histogram.

Returns a map with the following entries:

TBD"
  (let* ((analysis
          (let ((try-offset dtrt-indent-min-offset)
                unsorted-analysis)
            (while (<= try-offset dtrt-indent-max-offset)
              (setq
               unsorted-analysis
               (append unsorted-analysis
                       (list (dtrt-indent--analyze-histogram-try-offset
                              try-offset
                              (nth 0 histogram-and-total-lines)
                              (nth 1 histogram-and-total-lines)))))
              (setq try-offset (1+ try-offset)))
            (sort unsorted-analysis (lambda (x y) (> (nth 1 x)
                                                     (nth 1 y))))))
         (analysis-iterator analysis))

    (while analysis-iterator
      (let ((analysis-entry (car analysis-iterator)))
        (dolist (other-analysis-entry (cdr analysis-iterator))

          (let ((deviation (abs (- (nth 1 other-analysis-entry)
                                   (nth 1 analysis-entry)))))
            (when (and (not (nth 3 analysis-entry))
                       (eq 0 (mod (car other-analysis-entry)
                                  (car analysis-entry)))
                       (> dtrt-indent-max-merge-deviation
                          (* 100.0 deviation)))
              (setcdr
               (cddr analysis-entry)
               (list
                (format "\
merged with offset %s (%.2f%% deviation, limit %.2f%%)"
                        (nth 0 other-analysis-entry)
                        (* 100.0 deviation)
                        dtrt-indent-max-merge-deviation)))))))
      (setq analysis-iterator (cdr analysis-iterator)))

    (let (best-guess)
      (dolist (guess analysis)
        (cond
         ((and (null best-guess)
               (null (nth 3 guess)))
          (setq best-guess guess))))

      (let* ((confidence
              (if best-guess
                  (nth 1 best-guess)
                0))
             (total-lines (nth 1 histogram-and-total-lines))
             (hard-tab-percentage (if (> total-lines 0)
                                      (/ (float (nth 2 histogram-and-total-lines))
                                         total-lines)
                                    0))
             (soft-tab-percentage (if (> total-lines 0)
                                      (/ (float (nth 3 histogram-and-total-lines))
                                         total-lines)
                                    0))
             (change-indent-tabs-mode)
             (indent-tabs-mode-setting)
             (rejected
             (cond
              ((null best-guess)
               "no best guess")
              ((< (* 100.0 (nth 1 best-guess))
                  dtrt-indent-min-quality)
               (format "best guess below minimum quality (%f < %f)"
                       (* 100.0 (nth 1 best-guess))
                       dtrt-indent-min-quality)))))

        (cond
         (rejected)
         ((or (= 0 hard-tab-percentage)
              (>= (/ soft-tab-percentage
                     hard-tab-percentage)
                  (+ 1.0 (/ dtrt-indent-min-soft-tab-superiority 100.0))))
         (setq change-indent-tabs-mode t)
         (setq indent-tabs-mode-setting nil))

         ((or (= 0 soft-tab-percentage)
              (>= (/ hard-tab-percentage
                     soft-tab-percentage)
                  (+ 1.0 (/ dtrt-indent-min-hard-tab-superiority 100.0))))
         (setq change-indent-tabs-mode t)
         (setq indent-tabs-mode-setting t)))

        (list (cons :histogram (car histogram-and-total-lines))
              (cons :total-lines total-lines)
              (cons :analysis analysis)
              (cons :best-guess best-guess)
              (cons :hard-tab-lines (nth 2 histogram-and-total-lines) )
              (cons :hard-tab-percentage hard-tab-percentage)
              (cons :soft-tab-lines (nth 3 histogram-and-total-lines) )
              (cons :soft-tab-percentage soft-tab-percentage)
              (cons :change-indent-tabs-mode change-indent-tabs-mode)
              (cons :indent-tabs-mode-setting indent-tabs-mode-setting)
              (cons :rejected rejected)
              (cons :confidence confidence))))))

(defun dtrt-indent-try-set-offset ()
  "Try adjusting the current buffer's indentation offset."
  (let ((language-and-variable (cdr (dtrt-indent--search-hook-mapping major-mode))))
    (when language-and-variable
      (let* ((result
              (dtrt-indent--analyze
               (dtrt-indent--calc-histogram
                (car language-and-variable))))
             (best-guess
              (cdr (assoc :best-guess result)))
             (rejected
              (cdr (assoc :rejected result)))
             (confidence
              (cdr (assoc :confidence result)))
             (change-indent-tabs-mode
              (cdr (assoc :change-indent-tabs-mode result)))
             (indent-tabs-mode-setting
              (cdr (assoc :indent-tabs-mode-setting result)))
             (best-indent-offset
              (nth 0 best-guess))
             (indent-offset-variable
              (nth 1 language-and-variable))
             (indent-offset-variables
              (cons
               indent-offset-variable
               (remove nil
                       (mapcar
                        (lambda (x)
                          (let ((mode (car x))
                                (variable (cadr x)))
                            (when (and (boundp mode)
                                       (symbol-value mode))
                              variable)))
                        dtrt-indent-hook-generic-mapping-list))))
             (indent-offset-names
              (mapconcat (lambda (x) (format "%s" x))
                         indent-offset-variables ", ")))

        ; update indent-offset-variable?
        (cond
         ((and best-guess
               (not rejected))

          (if dtrt-indent-explicit-offset
              (message "\
Indentation offset set with file variable; not adjusted")
            (when (or (not dtrt-indent-require-confirmation-flag)
                      (yes-or-no-p
                       (format "Do you want to adjust %s to %s for buffer %s? "
                               indent-offset-names
                               best-indent-offset
                               (buffer-name))))
              (setq dtrt-indent-original-indent
                    (mapcar
                     (lambda (x)
                       (list x (symbol-value x) (local-variable-p x)))
                     indent-offset-variables))
              (when (>= dtrt-indent-verbosity 1)
                (let ((offset-info
                       (format "%s adjusted to %s%s"
                               indent-offset-names
                               best-indent-offset
                               (if (>= dtrt-indent-verbosity 2)
                                   (format " (%.0f%%%% confidence)"
                                           (* 100 confidence))
                                 ""))))
                  (message (concat "Note: " offset-info))))
              (dolist (x indent-offset-variables)
                (set (make-local-variable x)
                     best-indent-offset))
              best-indent-offset)))
         (t
          (when (>= dtrt-indent-verbosity 2)
            (message "Note: %s not adjusted%s" indent-offset-variables
                     (if (and rejected (>= dtrt-indent-verbosity 3))
                         (format ": %s" rejected) "")))
          nil))

        ; update indent-tabs-mode?
        (cond
         ((and change-indent-tabs-mode
               (not (eq indent-tabs-mode indent-tabs-mode-setting)))
          (when (>= dtrt-indent-verbosity 1)
            (let ((tabs-mode-info
                   (when (and change-indent-tabs-mode
                              (not (eql indent-tabs-mode-setting
                                        indent-tabs-mode)))
                     (format "indent-tabs-mode adjusted to %s"
                             indent-tabs-mode-setting))))
              (message (concat "Note: " tabs-mode-info))))
          ; backup indent-tabs-mode setting
          (setq dtrt-indent-original-indent
                (cons
                 (let ((x 'indent-tabs-mode))
                   (list x (symbol-value x) (local-variable-p x)))
                 dtrt-indent-original-indent))
          ; actually adapt indent-tabs-mode
          (set (make-local-variable 'indent-tabs-mode)
               indent-tabs-mode-setting))
         (t
          (when (>= dtrt-indent-verbosity 2)
            (message "Note: indent-tabs-mode not adjusted"))
          nil))
        ))))

(defun dtrt-indent-adapt ()
  "Try adjusting indentation settings for the current buffer."
  (interactive)
  (if dtrt-indent-original-indent
      (message "dtrt-indent already adjusted this buffer")
    (dtrt-indent-try-set-offset)))

(defun dtrt-indent-undo ()
  "Undo any change dtrt-indent made to the indentation offset."
  (interactive)
  (if (null dtrt-indent-original-indent)
      (message "No dtrt-indent override to undo in this buffer")
    (let ((info
           (mapconcat
            (lambda (x)
              ;; x is of the form `(variable value local-variable-p)'
              (if (nth 2 x)
                  ;; variable was originally buffer local
                  (progn
                    (set (nth 0 x)
                         (nth 1 x))
                    (when (>= dtrt-indent-verbosity 1)
                      (format "Note: restored original buffer-local value of %s for %s"
                              (nth 1 x)
                              (nth 0 x))))
                ;; variable was not originally buffer local
                (kill-local-variable (nth 0 x))
                (format "Note: killed buffer-local value for %s, restoring to default %s"
                        (nth 0 x)
                        (eval (nth 1 x)))))
            dtrt-indent-original-indent
            "\n")))
      (when (>= dtrt-indent-verbosity 1)
        (message info))
      (kill-local-variable 'dtrt-indent-original-indent))))

;;-----------------------------------------------------------------
;; Installation

(defadvice hack-one-local-variable
  (before dtrt-indent-advise-hack-one-local-variable activate)
  "Adviced by dtrt-indent.

Disable dtrt-indent if offset explicitly set."
  (cond
   ((eql (nth 2 (dtrt-indent--search-hook-mapping major-mode))
         (ad-get-arg 0))
    (setq dtrt-indent-explicit-offset t))
   ((eql 'indent-tabs-mode
         (ad-get-arg 0))
    (setq dtrt-indent-explicit-tab-mode t))))

(autoload 'dtrt-indent-diagnosis "dtrt-indent-diag"
  "Guess indentation for the current buffer and output diagnostics."
  t)

(autoload 'dtrt-indent-highlight "dtrt-indent-diag"
  "Highlight non-excluded indentation in the current buffer."
  t)

(provide 'dtrt-indent)

;;; dtrt-indent.el ends here
