#!/bin/bash

# --debug-on-exception

time chezscheme \
  --libdirs .:lib \
  --optimize-level 0 \
  --script main.scm
