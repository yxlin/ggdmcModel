# q(save = "no")

cat("\n\n----------- Testing bind_condition2parameters_r---------")
# rm(list = ls())
pkg <- c("ggdmcModel")
suppressPackageStartupMessages(tmp <- sapply(pkg, require, character.only = TRUE))
cat("\nWorking directory: ", getwd(), "\n")


p_map <- list(A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "1", st0 = "1")
factors <- list(S = c("s1", "s2"))
parameter_M <- bind_condition2parameters_r(p_map, factors)
is_parameter_x_condition
res <- is_parameter_x_condition(p_map, factors)

expected_pnames <- c("A", "B", "mean_v.false", "mean_v.true", "sd_v", "st0", "t0")
testthat::expect_true(all(parameter_M == expected_pnames))
testthat::expect_false(all(res))


p_map <- list(A = "1", B = "S", t0 = "1", mean_v = c("D", "M"), sd_v = "M", st0 = "1")
factors <- list(S = c("s1", "s2"), D = c("d1", "d2"))
parameter_M <- bind_condition2parameters_r(p_map, factors)

res <- is_parameter_x_condition(p_map, factors)

testthat::expect_equal(length(parameter_M), 11)
testthat::expect_equal(sum(res), 6)
