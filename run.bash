#!/bin/bash

time chezscheme \
  --libdirs .:lib \
  --optimize-level 0 \
  --script main.scm
