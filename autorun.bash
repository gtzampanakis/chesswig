#!/bin/bash

find . -type f -name '*.scm' | entr ./run.bash
