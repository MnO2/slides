#!/bin/bash

pandoc --standalone --section-divs \
  --variable theme="white" \
  --variable transition="fade" \
  -s -f markdown -t revealjs \
  free-monad-refactoring.md -o free-monad-refactoring.html
