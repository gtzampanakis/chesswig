#!/bin/bash

time chezscheme \
  --libdirs .:lib \
  --optimize-level 3 \
  --debug-on-exception \
  --script main.scm
