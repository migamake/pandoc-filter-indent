#!/bin/bash

build() {
  stack run < test/example.json > out.json
  pandoc -f json -t markdown out.json -o out.md
  cat out.md
}

if [ "$1" == "--watch" ]; then 
  while inotifywait src/*.hs app/*.hs; do
    build;
  done;
else
  build
fi;

