#!/bin/bash

shopt -s nullglob

for var in experiment*/; do
  files_001=( "$var"*.001 )
  files_7z=( "$var"*.7z )

  if [ ${#files_001[@]} -gt 0 ]; then
    7z e "${files_001[@]}" -o"$var"
    rm -r "${files_001[@]}"
    rm -r "${var}"*.00*
  elif [ ${#files_7z[@]} -gt 0 ]; then
    7z e "${files_7z[@]}" -o"$var"
    rm -r "${files_7z[@]}"
  fi
done

shopt -u nullglob
