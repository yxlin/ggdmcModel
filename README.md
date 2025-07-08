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

The following code showed how the model build allocates a parameter vecotr with two different drift rates corresponding to the two conditions from a stimulus factor, 'S' to the DDM drift rate parameter, 


```
pkg <- c("ggdmcModel")
sapply(pkg, require, character.only = TRUE)
cat("\nWorking directory: ", getwd(), "\n")

model <- BuildModel(
    p_map = list(a = "1", v = "S", z = "1", d = "1", sz = "1", sv = "1", t0 = "1", st0 = "1", s = "1"),
    match_map = list(M = list(s1 = "r1", s2 = "r2")),
    factors = list(S = c("s1", "s2")),
    constants = c(d = 1, s = 1, sv = 1, sz = 0.5, st0 = 0),
    accumulators = c("r1", "r2"),
    type = "fastdm"
)

pnames <- get_pnames(model)

p_vector <- c(a = 1, sv = 0.2, sz = 0.25, t0 = 0.15, v.s1 = 4, v.s2 = 2, z = .38)

# B, mean_v.true, t0
tmp_parameters <- c(0.8367, 0.0324, 3.8186, 2.8186, 0.1)
pmat <- table_parameters(model, tmp_parameters)
result <- lapply(pmat, function(x) {
    t(x)
})

print(result)
# $s1.r1
#         a d s st0 sv  sz     t0      v   z
# r1 0.8367 1 1   0  1 0.5 0.0324 3.8186 0.1
# r2 0.8367 1 1   0  1 0.5 0.0324 3.8186 0.1
# 
# $s1.r2
#         a d s st0 sv  sz     t0      v   z
# r1 0.8367 1 1   0  1 0.5 0.0324 3.8186 0.1
# r2 0.8367 1 1   0  1 0.5 0.0324 3.8186 0.1
# 
# $s2.r1
#         a d s st0 sv  sz     t0      v   z
# r1 0.8367 1 1   0  1 0.5 0.0324 2.8186 0.1
# r2 0.8367 1 1   0  1 0.5 0.0324 2.8186 0.1
# 
# $s2.r2
#         a d s st0 sv  sz     t0      v   z
# r1 0.8367 1 1   0  1 0.5 0.0324 2.8186 0.1
# r2 0.8367 1 1   0  1 0.5 0.0324 2.8186 0.1

```
# Prerequisites
R (>= 3.5.0), Rcpp (>= 1.0.7), methods, RcppArmadillo (>= 0.10.7.5.0), ggdmcHeaders (0.2.9.1)

# Installation

```
install.packages("ggdmcModel")
```
