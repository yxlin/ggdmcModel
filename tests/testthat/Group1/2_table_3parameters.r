# q(save = "no")
pkg <- c("ggdmcModel")
suppressPackageStartupMessages(tmp <- sapply(pkg, require, character.only = TRUE))
cat("\nWorking directory: ", getwd(), "\n")

model <- BuildModel(
    p_map = list(A = "1", B = "1", mean_v = "M", sd_v = "1", st0 = "1", t0 = "1"),
    match_map = list(M = list(s1 = "r1", s2 = "r2")),
    factors = list(S = c("s1", "s2")),
    constants = c(A = 0.75, mean_v.false = 1.5, sd_v = 1, st0 = 0),
    accumulators = c("r1", "r2"),
    type = "lba"
)

pnames <- get_pnames(model)
pnames

# B, mean_v.true, t0
tmp_parameters <- c(0.8367, 3.8186, 0.0324)
pmat <- table_parameters(model, tmp_parameters)
result <- lapply(pmat, function(x) {
    t(x)
})

s1_r1 <- matrix(c(
    0.75, 1.5867, 3.8186,    1,   0, 0.0324,
    0.75, 1.5867, 1.5000,    1,   0, 0.0324
), nrow = 2, byrow = TRUE)

s1_r2 <- matrix(c(
    0.75, 1.5867, 1.5000,    1,   0, 0.0324,
    0.75, 1.5867, 3.8186,    1,   0, 0.0324
), nrow = 2, byrow = TRUE)

s2_r1 <- matrix(c(
    0.75, 1.5867, 1.5000,    1,   0, 0.0324,
    0.75, 1.5867, 3.8186,    1,   0, 0.0324
), nrow = 2, byrow = TRUE)

s2_r2 <- matrix(c(
    0.75, 1.5867, 3.8186,    1,   0, 0.0324,
    0.75, 1.5867, 1.5000,    1,   0, 0.0324
), nrow = 2, byrow = TRUE)

attr(s1_r1, "dimnames") <- list(c("r1", "r2"), c("A", "b", "mean_v", "sd_v", "st0", "t0"))
attr(s1_r2, "dimnames") <- list(c("r1", "r2"), c("A", "b", "mean_v", "sd_v", "st0", "t0"))
attr(s2_r1, "dimnames") <- list(c("r1", "r2"), c("A", "b", "mean_v", "sd_v", "st0", "t0"))
attr(s2_r2, "dimnames") <- list(c("r1", "r2"), c("A", "b", "mean_v", "sd_v", "st0", "t0"))

expected_values <- list(s1_r1, s1_r2, s2_r1, s2_r2)
for (i in seq_len(length(expected_values))) {
    testthat::expect_equal(result[[i]], expected_values[[i]])
}


res <- print_parameter_map(model)
