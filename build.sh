#!/bin/bash

SRC_DIR=src
OUT_DIR=derived

for e in $(ls $SRC_DIR/*.md)
do
  f=$(basename $e)
  filename="${f%.*}"
  pandoc --standalone --section-divs \
    --variable theme="white" \
    --variable transition="fade" \
    -s -f markdown -t revealjs \
    ${SRC_DIR}/${filename}.md -o ${OUT_DIR}/${filename}.html
done
