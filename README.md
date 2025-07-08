'ggdmcModel' provides a set of tools for specifying and examining experimental designs associated with choice response time models (e.g., the Diffusion Decision Model). This package allows users to define how experimental factors influence one or more model parameters using R-style model specification syntax, while also checking the logical consistency of these associations. Additionally, it integrates with the 'ggdmc' package, which employs Differential Evolution Markov Chain Monte Carlo (DE-MCMC) sampling to optimise the model parameter.


# Getting Started


```
# Setting up a minimal LBA model
pkg <- c("lbaModel", "ggdmcPrior")
sapply(pkg, require, character.only = TRUE)

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
R (>= 3.5.0), Rcpp (>= 1.0.7), methods, RcppArmadillo (>= 0.10.7.5.0), ggdmcHeaders (0.2.9.1)

# Installation

```
install.packages("ggdmcModel")
```
