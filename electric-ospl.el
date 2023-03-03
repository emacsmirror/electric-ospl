;;; electric-ospl.el --- Electric OSPL Mode -*- lexical-binding: t -*-

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.1"))
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
;; Foo


;;; Code:


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
  :type 'electric-ospl
  :type 'integer)

(provide 'electric-ospl)

;;; electric-ospl.el ends here
