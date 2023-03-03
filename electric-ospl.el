;;; electric-ospl.el --- Electric OSPL Mode -*- lexical-binding: t -*-

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; Version: 1.5.1
;; Package-Requires: ((emacs "26.1") (s "1.11.0"))
;; Keywords: convenience, text
;; URL: https://git.sr.ht/~swflint/electric-ospl-mode
;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2023 Samuel W. Flint <swflint@flintfam.org>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A simple minor-mode to automatically enforce a "one-sentence per
;; line" style in text files.
;;
;;;; Installation
;;
;; Place the file `electric-ospl.el' somewhere on your `load-path',
;; and `require` it (note, autoloading is also supported).  Enable
;; `electric-ospl-mode' anywhere where you want to follow a
;; "one-sentence per line" style, for instance, by adding to various
;; mode hooks (I use `text-mode-hook' personally).
;;
;; Additionally, a globalized minor mode, `global-electric-ospl-mode'
;; is available, which will activate the mode based on the value of
;; `electric-ospl-global-modes' (see below).
;;
;;;; Configuration
;;
;; There are a couple of options which can be used to modify behavior
;; (and speed) of the mode.
;;
;; - The first is `electric-ospl-regexps', which sets the list of
;;   regular expressions defining how a sentence ends.
;;
;; - The next is `electric-ospl-ignored-abbreviations', which is a
;;   (case-sensitive) list of abbrevations ending in a period that are
;;   not necessarily considered the end of a sentence.
;;
;; - Finally, efficiency may be modified by changing
;;   `electric-ospl-maximum-lookback-chars', which determines how far
;;   to look back to find the end of a sentence.  Additionally, where
;;   the globalized mode is enabled is configured using
;;   `electric-ospl-global-modes`, which has the following semantics.
;;
;; - If t, it will be enabled in all modes (except for special modes,
;;   ephemeral buffers, or the minibuffer).
;;
;; - If nil, it will not be enabled in any modes.
;;
;; - If the first symbol is `not`, `electric-ospl-mode` will not be
;;   enabled in buffers with the listed major modes or descending from
;;   the listed major modes.
;;
;; - Otherwise, it will be enabled only in the listed modes.
;;
;;;; Acknowledgments
;;
;; This code is based significantly on Jan Seeger's `twauctex'
;; (https://github.com/jeeger/twauctex).  I've made effort to simplify
;; the logic and make it so that it's usable in modes other than
;; `LaTeX-mode'.
;;
;;;; Errors and Patches
;;
;; If you find an error or have a patch to improve this package, please
;; send an email to `~swflint/public-inbox@lists.sr.ht`.



;;; Code:

(require 's)


;;; Customization

(defgroup electric-ospl nil
  "Customization for electric-ospl.

electric-ospl is an electric SPC that helps to make
one-sentence-per-line editing slightly easier."
  :group 'text)

(defcustom electric-ospl-regexps (list (sentence-end))
  "Definitions of the end of a sentence.

This defaults to what the function `sentence-end' returns at load
time."
  :group 'electric-ospl
  :type '(repeat regexp))

(defcustom electric-ospl-ignored-abbreviations (list "et al."
                                                     "etc."
                                                     "i.e."
                                                     "e.g."
                                                     "vs."
                                                     "viz.")
  "A list of (case-sensitive) abbrevations which may not end sentences.

It is generally a good idea to customize this based on your
writing and those which you frequently use."
  :group 'electric-ospl
  :type '(repeat (string :tag "Abbreviation")))

(defcustom electric-ospl-maximum-lookback-chars 1
  "How far is lookback performed to find a sentence ending.

This should be calculated from the longest possible match to
`electric-ospl-regexps'."
  :group 'electric-ospl
  :type 'integer)

(defcustom electric-ospl-global-modes t
  "Modes in which `global-electric-ospl-mode' should enable the local mode.

If t, `electric-ospl-mode' is turned on for all major modes.  If
a list, it is turned on for all `major-mode' symbols; however, if
the `car' is `not', the mode is turned on for all modes not in
that list (or not descending from that list).  If nil,
`electric-ospl-mode' is never turned on automatically.

The mode is never turned on for modes which are considered
special or are ephemeral (have a space as prefix of the name)."
  :group 'electric-ospl
  :type '(choice (const t :tag "All modes")
                 (const nil :tag "No modes")
                 (set :menu-tag "certain modes" :tag "modes"
                      :value (not)
                      (const :tag "Forbid" not)
                      (repeat :inline t (symbol :tag "mode")))))


;;; Cached Regular Expressions

(defvar electric-ospl--ignored-abbrevs-regexp
  (regexp-opt electric-ospl-ignored-abbreviations)
  "Regular-expression for of `electric-ospl-ignored-abbreviations'.

This variable is generated automatically, and upon change to the
original variable.  Do not modify it directly.")

(defun electric-ospl--update-ignored-abbrevs-regexp (_symbol new-value op _where)
  "Update `electric-ospl--ignored-abbrevs-regexp' using NEW-VALUE when OP is `set'."
  (when (eq op 'set)
    (setf electric-ospl--ignored-abbrevs-regexp (regexp-opt new-value))))

(add-variable-watcher 'electric-ospl-ignored-abbreviations
                      #'electric-ospl--update-ignored-abbrevs-regexp)

(defvar electric-ospl--abbrev-lookback
  (+ 2 (apply #'max (mapcar #'length electric-ospl-ignored-abbreviations)))
  "How far should look-back be performed for ignored abbreviations?

This variable is generated automatically from
`electric-ospl-ignored-abbreviations', and upon change to the
original variable.  Do not modify it directly.")

(defun electric-ospl--update-abbrev-lookback (_symbol new-value op _where)
  "Update `electric-ospl--abbrev-lookback' using NEW-VALUE when OP is `set'."
  (when (eq op 'set)
    (setf electric-ospl--abbrev-lookback
          (+ 2 (apply #'max
                      (mapcar #'length
                              new-value))))))

(add-variable-watcher 'electric-ospl-ignored-abbreviations
                      #'electric-ospl--update-abbrev-lookback)

(defvar electric-ospl--single-sentence-end-regexp
  (s-concat "\\(?:" (s-join "\\|" (mapcar #'(lambda (regexp)
                                              (s-concat "\\(?:" regexp "\\)"))
                                          electric-ospl-regexps))
            "\\)")
  "Single sentence-ending regular expression.

This variable is automatically generated from
`electric-ospl-regexps' and upon its change.  Do not modify it
directly.")

(defun electric-ospl--update-sse-regexp (_symbol new-val op _where)
  "Change `electric-ospl--single-sentence-end-regexp' with NEW-VAL when OP is `set'."
  (when (eq op 'set)
    (setf electric-ospl--single-sentence-end-regexp
          (s-concat "\\(?:" (s-join "\\|" (mapcar #'(lambda (regexp)
                                                      (s-concat "\\(?:" regexp "\\)"))
                                                  new-val))
                    "\\)"))))

(add-variable-watcher 'electric-ospl-regexps
                      #'electric-ospl--update-sse-regexp)


;;; Electric Space

(defun electric-ospl-electric-space (arg)
  "Insert a space character, or an OSPL line break as appropriate.

If ARG is given, insert a space, do not break line.  If the
command is repeated, delete the line-break."
  (interactive "p")
  (self-insert-command 1)               ; Fool the sentence-end regular expressions...
  (let* ((case-fold-search nil)
         (repeated-p (or (> arg 1)
                         (eq last-command 'electric-ospl-electric-space)))
         (at-abbrev-p (save-match-data
                        (looking-back (s-concat "\\<" electric-ospl--ignored-abbrevs-regexp "\s?")
                                      (- (point) electric-ospl--abbrev-lookback))))
         (at-electric-p (save-match-data
                          (looking-back electric-ospl--single-sentence-end-regexp
                                        electric-ospl-maximum-lookback-chars)))
         ;; (at-last-upper-p (save-match-data
         ;;                    (looking-back (s-concat "[[:upper:]]" electric-ospl--single-sentence-end-regexp)
         ;;                                  (1+ electric-ospl-maximum-lookback-chars))))
         )
    (delete-char -1)                    ; Character is probably no longer needed
    (cond
     ((and repeated-p (bolp))
      (delete-char -1)
      (self-insert-command 1))
     ((and (not at-abbrev-p)
           at-electric-p)
      (newline)
      (indent-according-to-mode))
     (t (self-insert-command 1)))))


;;; Global Minor Mode Safety

(defun electric-ospl-allowed-mode-p ()
  "Should `global-electric-ospl-mode' enable the local mode in the current buffer?

The mode will not be enabled in the following cases:

 - In the minibuffer
 - `special' modes
 - `fundamental-mode'
 - Ephemeral Buffers
 - Major modes excluded by `electric-ospl-global-modes'"
  (and (pcase electric-ospl-global-modes
         (`t t)
         (`(not . ,modes) (and (not (memq major-mode modes))
                               (not (apply #'derived-mode-p modes))))
         (modes (memq major-mode modes)))
       (not (or (minibufferp)
                (eq major-mode 'fundamental-mode)
                (string-prefix-p " " (buffer-name))))))

(defun electric-ospl-enable-mode ()
  "Conditionally enable `electric-ospl-mode' in the current buffer.

`electric-ospl-mode' will only be enabled when
`electric-ospl-allowed-mode-p' is non-nil."
  (when (electric-ospl-allowed-mode-p)
    (electric-ospl-mode)))


;;; Minor Mode Definition

(defvar electric-ospl-mode-map
  (let ((keymap (make-keymap)))
    (define-key keymap (kbd "SPC") #'electric-ospl-electric-space)
    keymap)
  "Keymap for `electric-ospl-mode'.")

;;;###autoload
(define-minor-mode electric-ospl-mode
  "A basic One-Sentence-Per-Line mode which defines an electric SPC key."
  :lighter " OSPL" :keymap electric-ospl-mode-map
  (if electric-ospl-mode
      (message "Enabled `electric-ospl-mode'.")
    (message "Disabled `electric-ospl-mode'.")))

;;;###autoload
(define-globalized-minor-mode global-electric-ospl-mode electric-ospl-mode
  electric-ospl-enable-mode
  :init-value nil)


(provide 'electric-ospl)

;;; electric-ospl.el ends here
