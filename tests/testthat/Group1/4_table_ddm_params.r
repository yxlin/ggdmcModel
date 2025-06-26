# q(save = "no")
pkg <- c("ggdmcModel")
suppressPackageStartupMessages(tmp <- sapply(pkg, require, character.only = TRUE))
cat("\nWorking directory: ", getwd(), "\n")

model <- ggdmcModel::BuildModel(
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

result
