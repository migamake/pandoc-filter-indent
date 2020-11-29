#!/bin/bash

build() {
  stack build && \
  stack run < test/example.json > out.json &&
  pandoc -f json -t markdown out.json -o out.md &&
  cat out.md
}

build
if [ "$1" == "--watch" ]; then 
  while inotifywait src/*.hs app/*.hs src/*/*.hs; do
    build;
  done;
fi;

