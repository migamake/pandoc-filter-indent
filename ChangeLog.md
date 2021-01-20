# Changelog for pandoc-filter-indent

## Unreleased changes

  - lambda in Haskell tokenizer
  - automatically append required LaTeX packages
  - Mapsto for |=>

## Release history

0.3.1.0 Jan 15 2021
  - Slightly improved formatting with SkyLighting and LaTeX

0.3.0.0 Jan 15 2021
  - Support default syntaxes from skylighting.

0.2.3.0 Jan 12 2021
  - fixed minor issue that disabled inline rendering.

0.2.2.0 Jan 12 2021
  - Fixed problem with declaring too few columns in LaTeX
  - Inline code formatting
  - Removed indent marks from parenthesised operators: "(+)"
  - Removed indent marks from functions promoted to operators like "`mappend`"
  - Fixed rendering of indent mark at the start of the column

0.1.0.0 Dec 1 2020
  - Initial release supporting LaTeX and HTML output

0.2.0.0 Dec 2 2020
  - Preliminary support for Tikzmarks
  - Updated README

0.2.1.0 Dec 2 2020
  - Extended code documentation
  - Cleanups
