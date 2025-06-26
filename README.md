# ggdmcModel
ggdmcModel provides tools for specifying and examining experimental design associated with cognitive models (e.g., diffusion decision models) for use with the 'ggdmc' package.

# Getting Started

```
# Setting up a minimal LBA model
pkg <- c("lbaModel", "ggdmcPrior")
suppressPackageStartupMessages(tmp <- sapply(pkg, require, character.only = TRUE))

model <- BuildModel(
    p_map = list(A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "1", st0 = "1"),
    match_map = list(M = list(s1 = "r1", s2 = "r2")),
    factors = list(S = c("s1", "s2")),
    constants = c(st0 = 0, sd_v = 1),
    accumulators = c("r1", "r2"),
    type = "lba"
)

# Setting up a minimal DDM model
model <- BuildModel(
    p_map = list(
        a = "1", v = "1", z = "1", d = "1", sz = "1", sv = "1",
        t0 = "1", st0 = "1", s = "1", precision = "1"
    ),
    match_map = list(M = list(s1 = "r1", s2 = "r2")),
    factors = list(S = c("s1", "s2")),
    constants = c(d = 0, s = 1, st0 = 0, sv = 0, precision = 3),
    accumulators = c("r1", "r2"),
    type = "fastdm"
)

slotNames(model)
#  [1] "parameter_map"               "accumulators"               
#  [3] "factors"                     "match_map"                  
#  [5] "constants"                   "cell_names"                 
#  [7] "parameter_x_condition_names" "model_boolean"              
#  [9] "pnames"                      "npar"                       
# [11] "type"       

```
# Prerequisites
See DESCRIPTION for prerequisites

# Installation
From CRAN:

```
install.packages("ggdmcModel")
```