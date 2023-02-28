#!/bin/bash

NAME=example
#NAME=problem2

pandoc() {
  ~/.local/bin/pandoc ${*}
}

which pandoc
type pandoc

build() {
  FILTER_EXE=$(stack path --local-install-root)/bin/pandoc-filter-indent;
  stack build &&
  #stack run text < test/example.json > out.json &&
  echo "Filter is ${FILTER_EXE}" &&
  pandoc -f json      -t markdown test/${NAME}.json -o out.md &&
  pandoc              -t plain    test/${NAME}.md   -o out.txt         --filter "${FILTER_EXE}"                                          &&
  pandoc --standalone -t latex    test/${NAME}.md   -o out.tex         --filter "${FILTER_EXE}"                                          &&
  pandoc              -t html     test/${NAME}.md   -o out.html        --filter "${FILTER_EXE}"                                          &&
  pandoc              -t pdf      test/${NAME}.md   -o out.pdf         --filter "${FILTER_EXE}"                                          &&
  pandoc --standalone -t latex    test/${NAME}.md   -o out-acm.tex     --filter "${FILTER_EXE}" --template acm-template/new-template.tex &&
  pandoc --standalone -t pdf      test/${NAME}.md   -o out-acm.pdf     --filter "${FILTER_EXE}" --template acm-template/new-template.tex &&
  echo "Beamer"
  pandoc --standalone -t beamer   test/${NAME}.md   -o out-beamer.tex  --filter "${FILTER_EXE}"                                          &&
  pandoc              -t beamer   test/${NAME}.md   -o out-beamer.pdf  --filter "${FILTER_EXE}"                                          &&
  pandoc              -t s5       test/${NAME}.md   -o out.s5.html     --filter "${FILTER_EXE}"                                          &&
  pandoc              -t revealjs test/${NAME}.md   -o out.reveal.html --filter "${FILTER_EXE}"                                          &&
  cat out.md
}

build || exit 1
if [ "$1" == "--watch" ]; then 
  while inotifywait -e modify,move src/*.hs app/*.hs src/*/*.hs test/*.md; do
    build ${NAME};
  done;
fi;

