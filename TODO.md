0. Alignment improvments:
  [ ] Comment start
  [ ] equals sign and :: bunched together
1. Unit tests for table coherency.
2. Unit tests for debug rendering.
3. Indent-only tokenizer.
4. Tokenizers for other languages:
  - YAML: https://hackage.haskell.org/package/HsYAML-0.2.1.0/docs/Data-YAML-Token.html
  - JSON: https://hackage.haskell.org/package/json-tokens-0.1.0.1/docs/Json-Token.html
  - Python:
      * http://hackage.haskell.org/package/language-python-0.5.8/docs/Language-Python-Version3-Lexer.html
      * http://hackage.haskell.org/package/language-python-0.5.8/docs/Language-Python-Version2-Lexer.html
  - SQL:
      * http://hackage.haskell.org/package/language-sqlite-1.1/docs/Language-SQL-SQLite.html#v:lexModuleArgument
      * http://hackage.haskell.org/package/hsql
5. Allow tokenizers from `skylighting`?
6. Support tikzmarks in HTML:
  - https://stackoverflow.com/questions/554167/drawing-arrows-on-an-html-page-to-visualize-semantic-links-between-textual-spans#623770
7. Post-tokenization stage to preprocess operators.
[+] 8. Prepend `header-includes`. (And document them.)
9. Split shorter README and documentation?

Simplifications proposed:
* no need for tableColumns, just total colspans

Found many issues in `pandoc-filter-indent`:
0. Pass the default syntax argument from metadata.
1. <$> and <*> in IFL/ACM style
2. Indent in JSON and other languages.
3. Replace '`Set.member`' by \elem?
4. Gray the comments
5. Option to mark inline typewriter as Haskell (for rendering).
6. Double newline as end of alignment block.
7. Treatment of '(==op)' (operator and parenthesis block.
8. Problem with `(|)`
9. Attach parenthesis to previous/current.
10. Unwrap attached parenthesis.
11. :: and = matching in this example:
    ```
    f  :: ty -> ty
    f x = x
    ```
12. Operator centering:
    ```
    data Alpha = Alpha
               | Beta
               | Gamma
    ```
13. Columns kept in absence of end of function (two CR CRs)
14. Graying out comments
15. Collapsing operators?
16. Emphasising inline code to distinguish it from text (emph?)
