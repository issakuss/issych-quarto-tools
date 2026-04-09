#!/bin/bash

# 使い方：make_diff.sh [commit-id] [filename_without_ext]
# > make_diff.sh v1.0.0.0 manuscript

OLDCOMMIT=$1
FILENAME=$2
TEMP_DIR="_temp_diff_work"

mkdir -p "$TEMP_DIR"
find . -mindepth 2 -type f \( -name "*.cls" -o -name "*.bst" -o -name "*.bib" \) -exec cp -n {} "./$TEMP_DIR/" \;
cp ../../result/fig/* "./$TEMP_DIR/" 2>/dev/null
cp "$FILENAME.tex" "./$TEMP_DIR/"
cd "$TEMP_DIR"

latexdiff --config="PICTUREENV=(?:picture|tikzpicture|thebibliography|longtable)" \
          <(git show "$OLDCOMMIT:publish/paper/$FILENAME.tex") \
          "$FILENAME.tex" > diff.tex
pdflatex -interaction=nonstopmode diff.tex
bibtex diff
pdflatex -interaction=nonstopmode diff.tex
pdflatex -interaction=nonstopmode diff.tex

mv diff.pdf "../diff-$FILENAME.pdf"
cd ..
rm -rf "$TEMP_DIR"

echo "Done! diff-$FILENAME.pdf has been created."
