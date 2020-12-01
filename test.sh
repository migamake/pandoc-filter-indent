#!/bin/bash

build() {
  FILTER_EXE=$(stack path --local-install-root)/bin/filter-indent;
  stack build && \
  stack run < test/example.json > out.json &&
  pandoc -f json -t markdown  out.json -o out.md &&
  pandoc              -t html  test/example.md -o out.html --filter "${FILTER_EXE}" &&
  pandoc --standalone -t latex test/example.md -o out.tex  --filter "${FILTER_EXE}" &&
  pandoc              -t pdf   test/example.md -o out.pdf  --filter "${FILTER_EXE}" &&
  cat out.md
}

build
if [ "$1" == "--watch" ]; then 
  while inotifywait src/*.hs app/*.hs src/*/*.hs; do
    build;
  done;
fi;

