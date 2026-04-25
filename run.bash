#!/bin/bash

echo
echo "run.bash: Start"
echo
chezscheme \
  --libdirs .:lib \
  --optimize-level 0 \
  --script main.scm
echo
echo "run.bash: End"
