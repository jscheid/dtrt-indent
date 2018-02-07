;;; dtrt-indent-test.el --- Tests for dtrt-indent.el

;; Copyright (C) 2003, 2007, 2008 Julian Scheid

;; Author: Julian Scheid <julians37@googlemail.com>
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

(eval-when-compile (require 'cl))

(require 'dtrt-indent)


;;-----------------------------------------------------------------
;; Tests

(defun dtrt-indent-functional-test (args)
  (with-temp-buffer
    (make-local-variable 'dtrt-indent-verbosity)
    (setq dtrt-indent-verbosity 0)
    (make-local-variable 'dtrt-indent-min-relevant-lines)
    (setq dtrt-indent-min-relevant-lines 3)
    (insert (cdr (assoc :buffer-contents args)))
    (let* ((language-and-variable
            (cdr (dtrt-indent--search-hook-mapping
                  (cdr (assoc :mode args)))))
           (result
            (dtrt-indent--analyze
             (dtrt-indent--calc-histogram
              (car language-and-variable))))
           (best-guess
            (cdr (assoc :best-guess result)))
           (rejected
            (cdr (assoc :rejected result)))
           (confidence
            (cdr (assoc :confidence result)))
           (best-indent-offset
            (nth 0 best-guess))
           (indent-offset-variable
            (nth 1 language-and-variable))
           (change-indent-tabs-mode
            (cdr (assoc :change-indent-tabs-mode result)))
           (indent-tabs-mode-setting
            (cdr (assoc :indent-tabs-mode-setting result)))
           (expected-offset
            (cdr (assoc :expected-offset args)))
           (expected-tab-setting
            (cdr (assoc :expected-tab-setting args))))
      (cond
       ((eq expected-tab-setting 'hard)
        (assert (eq change-indent-tabs-mode t))
        (assert (eq indent-tabs-mode-setting t)))
       ((eq expected-tab-setting 'soft)
        (assert (eq change-indent-tabs-mode t))
        (assert (eq indent-tabs-mode-setting nil)))
       ((eq expected-tab-setting 'undecided)
        (assert (eq change-indent-tabs-mode nil))))
      (if expected-offset
          (assert (eq nil rejected) t)
        (assert (not (eq nil rejected)) t))          
      (assert (eq expected-offset
                  best-indent-offset) t))))

(defun dtrt-indent-test-rec-directory-files
  (directory filename-pattern function)
  (let ((files
         (directory-files directory t)))
    (mapc (lambda (file)
            (when (and (not (file-directory-p file))
                       (string-match filename-pattern
                                     (file-name-nondirectory file)))
              (funcall function file)))
          files)))

(defun dtrt-indent-bulk-test (args)
  (princ (format "Performing bulk test on %s\n"
                 (cdr (assoc :directory args))))
  (setq dtrt-indent-verbosity 0)

  (dtrt-indent-test-rec-directory-files
   (cdr (assoc :directory args))
   (cdr (assoc :filename-pattern args))
   (lambda (file)
     (princ
      (format
       "file %s -> %s\n"
       file
       (with-temp-buffer
         "*dtrt-indent-test-file*"
         (insert-file-contents-literally file)
         (dtrt-indent-try-adjust)))))))

;; Functional tests

(dtrt-indent-functional-test
 '((:buffer-contents . "foo")
   (:mode . sh-mode)
   (:expected-offset . nil)))

(dtrt-indent-functional-test
 '((:buffer-contents . "\
aa
    aa
        aa")
   (:mode . sh-mode)
   (:expected-offset . 4)))

(dtrt-indent-functional-test
 '((:buffer-contents . "\
aa /*foo
    bar
    blah*/
   aa
      aa")
   (:mode . c-mode)
   (:expected-offset . 3)))

(dtrt-indent-functional-test
 '((:buffer-contents . "\
	tabbed-line
	tabbed-line
	tabbed-line
	tabbed-line
        softspace-line")
   (:mode . c-mode)
   (:expected-tab-setting . hard)
   (:expected-offset . 8)))

(dtrt-indent-functional-test
 '((:buffer-contents . "\
	tabbed-line
        softspace-line
        softspace-line
        softspace-line
        softspace-line")
   (:mode . c-mode)
   (:expected-tab-setting . soft)
   (:expected-offset . 8)))

(dtrt-indent-functional-test
 '((:buffer-contents . "\
	tabbed-line
	tabbed-line
	tabbed-line
        softspace-line
        softspace-line")
   (:mode . c-mode)
   (:expected-tab-setting . undecided)
   (:expected-offset . 8)))

(define-derived-mode derived-c-mode c-mode "Derived-C"
  "Derived-C for Test")

(dtrt-indent-functional-test
 '((:buffer-contents . "\
	tabbed-line
        softspace-line
        softspace-line
        softspace-line
        softspace-line")
   (:mode . derived-c-mode)
   (:expected-tab-setting . soft)
   (:expected-offset . 8)))

(when nil ;; disabled
  (with-output-to-temp-buffer "*dtrt-indent-test-results*"
    (dtrt-indent-bulk-test
     '((:directory . "\
/Volumes/IOMEGA_HDD/guess-offset-test/phpMyAdmin-2.10.0.2-english/")
       (:filename-pattern . ".php$")
       (:mode . php-mode)
       (:expected-offset . 4)
       (:min-confidence . 80)))))

;;; dtrt-indent-tests.el ends here
