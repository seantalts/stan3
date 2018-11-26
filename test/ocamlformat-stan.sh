#!/bin/bash

shopt -s nullglob
cd "src/stan";
for filename in *.ml; do
  ocamlformat -i "$filename";
done;
for filename in *.mli; do
  ocamlformat -i "$filename";
done;
cd ../..;
# TODO: Find a way to auto-format the .mll and .mly files as well.