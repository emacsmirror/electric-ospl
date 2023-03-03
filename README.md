[![REUSE status](https://api.reuse.software/badge/git.sr.ht/~swflint/electric-ospl-mode)](https://api.reuse.software/info/git.sr.ht/~swflint/electric-ospl-mode)

# electric-ospl-mode

A simple minor mode to automatically enforce a "one-sentence per line" style in text files.

## Installation

Place the file `electric-ospl.el` somewhere on your `load-path`, and `require` it (note, autoloading is also supported).
Enable `electric-ospl-mode` anywhere where you want to follow a "one-sentence per line" style, for instance, by adding to various mode hooks (I use `text-mode-hook` personally).

## Configuration

There are a couple of options which can be used to modify behavior (and speed) of the mode.

 - The first is `electric-ospl-regexps`, which sets the list of regular expressions defining how a sentence ends.
 - The next is `electric-ospl-ignored-abbreviations`, which is a (case-sensitive) list of abbrevations ending in a period that are not necessarily considered the end of a sentence.
 - Finally, efficiency may be modified by changing `electric-ospl-maximum-lookback-chars`, which determines how far to look back to find the end of a sentence.

## Acknowledgements

This code is based quite significantly on Jan Seeger's [`twauctex`](https://github.com/jeeger/twauctex).
This package, however, is designed to simplify the logic and generalize it so that it is usable outside of `LaTeX-mode`.

## Errors and Patches

If you find an error or have a patch to improve this package, please send an email to `~swflint/public-inbox@lists.sr.ht`.
