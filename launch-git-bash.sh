#!/bin/bash

# Args:
#    %1: path to git-bash.exe
#    %2: conda environment name

eval "$(conda shell.bash hook)"
conda activate %1
# %1
# CALL conda.bat activate %2
