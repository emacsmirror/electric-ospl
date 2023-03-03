[![REUSE status](https://api.reuse.software/badge/git.sr.ht/~swflint/electric-ospl-mode)](https://api.reuse.software/info/git.sr.ht/~swflint/electric-ospl-mode)

# electric-ospl-mode

A simple minor mode to automatically enforce a "one-sentence per line" style in text files.

## Installation

Place the file `electric-ospl.el` somewhere on your `load-path`, and `require` it (note, autoloading is also supported).
Enable `electric-ospl-mode` anywhere where you want to follow a "one-sentence per line" style, for instance, by adding to various mode hooks (I use `text-mode-hook` personally).

Additionally, a globalized minor mode, `global-electric-ospl-mode` is available, which will activate the mode based on the value of `electric-ospl-global-modes` (see below).

## Configuration

There are a couple of options which can be used to modify behavior (and speed) of the mode.

 - The first is `electric-ospl-regexps`, which sets the list of regular expressions defining how a sentence ends.
 - The next is `electric-ospl-ignored-abbreviations`, which is a (case-sensitive) list of abbrevations ending in a period that are not necessarily considered the end of a sentence.
 - Next, efficiency may be modified by changing `electric-ospl-maximum-lookback-chars`, which determines how far to look back to find the end of a sentence.
 - Finally, you can configure ignoring of OSPL in certain circumstances using the `ospl-ignore-electric-functions` hook.  This variable defaults to ignoring when the last thing was in the ignored abbreviations list.  It is used by checking, one-by-one for a function which returns non-nil.  An example use is below.
 
```elisp
(add-hook 'LaTeX-mode-hook (defun my/latex-ospl-config ()
                             (add-hook 'electric-ospl-ignore-electric-functions
                                       (defun my/ignore-ospl-in-some-latex-envs ()
                                         (cl-member (LaTeX-current-environment)
                                                    '("tabular" "tabularx" "tikzpicture")))
                                       -100 t)))
```
 
Additionally, where the globalized mode is enabled is configured using `electric-ospl-global-modes`, which has the following semantics.

 - If t, it will be enabled in all modes (except for special modes, ephemeral buffers, or the minibuffer).
 - If nil, it will not be enabled in any modes.
 - If the first symbol is `not`, `electric-ospl-mode` will not be enabled in buffers with the listed major modes or descending from the listed major modes.
 - Otherwise, it will be enabled only in the listed modes.

## Acknowledgements

This code is based quite significantly on Jan Seeger's [`twauctex`](https://github.com/jeeger/twauctex).
This package, however, is designed to simplify the logic and generalize it so that it is usable outside of `LaTeX-mode`.

## Errors and Patches

If you find an error or have a patch to improve this package, please send an email to `~swflint/public-inbox@lists.sr.ht`.
