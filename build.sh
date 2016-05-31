#!/bin/bash

pandoc --standalone --section-divs \
  --variable theme="white" \
  --variable transition="fade" \
  -s -f markdown -t revealjs \
  neovim-plugin-in-haskell.md -o neovim-plugin-in-haskell.html 
