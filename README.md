
# feature-sims

Builds simulation tests for open-source time-series feature
calculations.

## Motivation

This project is intended to explore the computational performance of
open-source time-series feature calculation packages available in the
open-source language R. This is primarily achieved through the
consolidated pipeline package
[`theft`](https://github.com/hendersontrent/theft).

## Analytical Pipeline

This project uses a modularised code base. The `setup.R` script loads
all R packages needed for the project, sets up the necessary subfolders
and directories, and loads any functions that were written for the
project. This file should be run from “source” whenever using code in
this repository.

The overall pipeline (to-date) is as follows:

1.  `setup.R` (this loads functions automatically from the `/R` folder)
2.  `/simulations` - holds the analysis code for computational time
    tests
3.  `/applied-probs` - holds the analysis code for applied problems like
    classification and regression on open data
4.  `/output` - holds any statistical graphics or calculation outputs
    generated from `/simulations` or `/applied-probs` scripts
