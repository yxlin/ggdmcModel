# q(save = "no")
cat("\n\n-------------------- Testing basic models --------------------")
rm(list = ls())
pkg <- c("ggdmcModel")
suppressPackageStartupMessages(tmp <- sapply(pkg, require, character.only = TRUE))
cat("\nWorking directory: ", getwd(), "\n")


# Model 0 - 3 Acc
model <- BuildModel(
    p_map = list(A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "M", st0 = "1"),
    match_map = list(M = list("s1" = "r1", "s2" = "r2", "s3" = "r3")),
    factors = list(S = c("s1", "s2", "s3")),
    constants = c(sd_v.false = 1, st0 = 0),
    accumulators = c("r1", "r2", "r3"),
    type = "lba"
)


testthat::expect_error(
    BuildModel(
        p_map = list(A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "M", st0 = "1"),
        match_map = list(M = list("s1" = "r1", "s2" = "r2", "s3" = "r3")),
        factors = list(S = c("s1", "s2", "s3")),
        constants = c(sd_v.false = 1, st0 = 0),
        accumulators = c("r1", "r2"),
        type = "lba"
    ),
    regexp = "match_map\\$M has index or name not in response names"
)

# Model 0 - 4 Acc
model <- BuildModel(
    p_map = list(A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "M", st0 = "1"),
    match_map = list(M = list("s1" = "r1", "s2" = "r2", "s3" = "r3", "s4" = "r4")),
    factors = list(S = c("s1", "s2", "s3", "s4")),
    constants = c(sd_v.false = 1, st0 = 0),
    accumulators = c("r1", "r2", "r3", "r4"),
    type = "lba"
)

testthat::expect_error(
    BuildModel(
        p_map = list(A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "M", st0 = "1"),
        match_map = list(M = list("s1" = "r1", "s2" = "r2", "s3" = "r3", "s4" = "r4")),
        factors = list(S = c("s1", "s2", "s3")),
        constants = c(sd_v.false = 1, st0 = 0),
        accumulators = c("r1", "r2", "r3", "r4"),
        type = "lba"
    ),
    regexp = "The number of levels for 'S' must match the number of accumulators (model_utils.cpp.)",
    fixed = TRUE # Forces exact match (no regex interpretation)
)
