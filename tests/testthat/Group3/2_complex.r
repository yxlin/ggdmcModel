# q(save = "no")
cat("\n\n-------------------- Testing v models --------------------")
# rm(list = ls())
# pkg <- c("ggdmcModel")
# suppressPackageStartupMessages(tmp <- sapply(pkg, require, character.only = TRUE))
cat("\nWorking directory: ", getwd(), "\n")

# Model 1 - 3 parameters
model <- BuildModel(
    p_map = list(A = "1", B = "S", t0 = "1", mean_v = c("D", "M"), sd_v = "M", st0 = "1"),
    match_map = list(M = list(s1 = "r1", s2 = "r2")),
    factors = list(S = c("s1", "s2"), D = c("d1", "d2")),
    constants = c(sd_v.false = 1, st0 = 0),
    accumulators = c("r1", "r2"),
    type = "lba"
)


# Model 2
model <- BuildModel(
    p_map = list(A = "1", B = "S", t0 = "E", mean_v = c("D", "M"), sd_v = "M", st0 = "1"),
    match_map = list(M = list(
        sti_1 = "resp_1", sti_2 = "resp_2",
        sti_3 = "resp_3", sti_4 = "resp_4"
    )),
    factors = list(S = c("sti_1", "sti_2", "sti_3", "sti_4"), D = c("d1", "d2"), E = c("e1", "e2")),
    constants = c(sd_v.false = 1, st0 = 0),
    accumulators = c("resp_1", "resp_2", "resp_3", "resp_4"),
    type = "lba"
)

# Set up 3-1 ----------------------------------------------
model <- BuildModel(
    p_map = list(A = "1", B = c("S", "G"), t0 = "E", mean_v = c("D", "M"), sd_v = "M", st0 = "1"),
    match_map = list(M = list(
        s1 = "resp_1", s2 = "resp_2",
        s3 = "resp_3"
    )),
    factors = list(
        S = c("s1", "s2", "s3"), D = c("d1", "d2"), E = c("e1", "e2"),
        G = c("g1", "g2", "g3")
    ),
    constants = c(st0 = 0, sd_v.false = 1),
    accumulators = c("resp_1", "resp_2", "resp_3"),
    type = "lba"
)
# Set up 3-2 ----------------------------------------------
# no t0 - E
model <- BuildModel(
    p_map = list(A = "1", B = c("S", "G"), t0 = "1", mean_v = c("1"), sd_v = "1", st0 = "1"),
    match_map = list(M = list(
        s1 = "resp_1", s2 = "resp_2",
        s3 = "resp_3"
    )),
    factors = list(
        S = c("s1", "s2", "s3"),
        G = c("g1", "g2", "g3")
    ),
    constants = c(sd_v = 1, st0 = 0),
    accumulators = c("resp_1", "resp_2", "resp_3"),
    type = "lba"
)

# Set up 4 -----------------------------------------------
model <- BuildModel(
    p_map = list(
        A = "H", B = c("S", "G"), t0 = "E", mean_v = c("D", "H", "M"),
        sd_v = c("D", "M"), st0 = "1"
    ),
    match_map = list(M = list(
        s1 = "r1", s2 = "r2",
        s3 = "r3"
    )),
    factors = list(
        S = c("s1", "s2", "s3"), D = c("d1", "d2"), E = c("e1", "e2"),
        G = c("g1", "g2", "g3"), H = c("h1", "h2", "h3", "h4", "h5")
    ),
    constants = c(st0 = 0, sd_v.d1.false = 1),
    accumulators = c("r1", "r2", "r3"),
    type = "lba"
)


model <- BuildModel(
    p_map = list(
        A = "H", B = c("S", "G"), t0 = "E", mean_v = c("D", "H", "M"),
        sd_v = c("D", "M"), st0 = "1"
    ),
    match_map = list(M = list(
        s1 = "r1", s2 = "r2",
        s3 = "r3"
    )),
    factors = list(
        S = c("s1", "s2", "s3"), D = c("d1", "d2"), E = c("e1", "e2"),
        G = c("g1", "g2", "g3"), H = c("h1", "h2", "h3", "h4", "h5")
    ),
    constants = c(st0 = 0, sd_v.d1.false = 1),
    accumulators = c("r1", "r2", "r3"),
    type = "lba"
)
