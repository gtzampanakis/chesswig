#!/bin/bash

time chezscheme \
  --libdirs .:lib \
  --optimize-level 3 \
  --script main.scm
