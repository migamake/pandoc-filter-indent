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

Simplifications proposed:
* no need for tableColumns, just total colspans
