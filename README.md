# ggdmcModel

<!-- Badges -->
[![CRAN Status](https://www.r-pkg.org/badges/version/ggdmcModel)](https://cran.r-project.org/package=ggdmcModel)
[![Downloads](https://cranlogs.r-pkg.org/badges/ggdmcModel)](https://cran.r-project.org/package=ggdmcModel)
[![License: GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![R-CMD-check](https://github.com/yxlin/ggdmcModel/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yxlin/ggdmcModel/actions/workflows/R-CMD-check.yaml)


**ggdmcModel** provides a suite of tools for specifying and examining experimental designs for choice response time models, such as the Diffusion Decision Model (DDM) and Linear Ballistic Accumulator (LBA).  
It enables users to define how experimental factors influence one or more model parameters using R-style formula syntax, while ensuring logical consistency of these associations.  

The package integrates with the [`ggdmc`](https://cran.r-project.org/package=ggdmc) package, which employs Differential Evolution Markov Chain Monte Carlo (DE-MCMC) sampling for parameter estimation in hierarchical Bayesian models.

---

## 📦 Prerequisites
- **R** (≥ 3.5.0)  
- **Rcpp** (≥ 1.0.7)  
- **methods**  
- **RcppArmadillo** (≥ 0.10.7.5.0)  
- **ggdmcHeaders** (≥ 0.2.9.1)  

---

## 📥 Installation

From CRAN:
```r
install.packages("ggdmcModel")
```

## 🚀 Getting Started
### Example 1 – Minimal LBA Model

```r
library(ggdmcModel)

model <- BuildModel(
    p_map = list(A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "1", st0 = "1"),
    match_map = list(M = list(s1 = "r1", s2 = "r2")),
    factors = list(S = c("s1", "s2")),
    constants = c(st0 = 0, sd_v = 1),
    accumulators = c("r1", "r2"),
    type = "lba"
)
```

### Example 2 – Minimal DDM Model

```r
library(ggdmcModel)

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
# [1] "parameter_map"               "accumulators"               
# [3] "factors"                     "match_map"                  
# [5] "constants"                   "cell_names"                 
# [7] "parameter_x_condition_names" "model_boolean"              
# [9] "pnames"                      "npar"                       
# [11] "type"       
```

### Example 3 – Factor-to-Parameter Mapping in DDM

The following example shows how `BuildModel` assigns two different drift rates (`v`) for two levels of a stimulus factor `S`:

```r
library(ggdmcModel)

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

tmp_parameters <- c(0.8367, 0.0324, 3.8186, 2.8186, 0.1)
pmat <- table_parameters(model, tmp_parameters)

result <- lapply(pmat, function(x) t(x))
print(result)

```

Example output:

```bash
$s1.r1
        a d s st0 sv  sz     t0      v   z
r1 0.8367 1 1   0  1 0.5 0.0324 3.8186 0.1
r2 0.8367 1 1   0  1 0.5 0.0324 3.8186 0.1

$s2.r2
        a d s st0 sv  sz     t0      v   z
r1 0.8367 1 1   0  1 0.5 0.0324 2.8186 0.1
r2 0.8367 1 1   0  1 0.5 0.0324 2.8186 0.1
```

## 📄 License
GPL (≥ 3)


