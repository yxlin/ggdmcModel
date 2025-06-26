test_model_boolean <- function(tc) {
    res <- build_model_boolean_r(tc$model@parameter_map, tc$model@factors, 
                                 tc$model@accumulators, tc$model@match_map)
    expect_equal(sum(res), tc$expected_boolean)
}


# Model 0 - 2 Acc
model0 <- BuildModel(
    p_map = list(A = "1", B = "1", mean_v = "M", sd_v = "1", st0 = "1", t0 = "1"),
    match_map = list(M = list(s1 = "r1", s2 = "r2")),
    factors = list(S = c("s1", "s2")),
    constants = c(A = 0.75, mean_v.false = 1.5, sd_v = 1, st0 = 0),
    accumulators = c("r1", "r2"),
    type = "lba"
)


# Model 0 - 3 Acc
model1 <- BuildModel(
    p_map = list(A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "M", st0 = "1"),
    match_map = list(M = list("s1" = "r1", "s2" = "r2", "s3" = "r3")),
    factors = list(S = c("s1", "s2", "s3")),
    constants = c(A = 0.75, mean_v.false = 1.5, sd_v = 1, st0 = 0),
    accumulators = c("r1", "r2", "r3"),
    type = "lba"
)

# Model 0 - 3 Acc
model2 <- BuildModel(
    p_map = list(A = "1", B = "S", t0 = "1", mean_v = c("D", "M"), sd_v = "M", st0 = "1"),
    match_map = list(M = list("s1" = "r1", "s2" = "r2")),
    factors = list(S = c("s1", "s2"), D = c("d1", "d2")),
    constants = c(sd_v.false = 1.5, st0 = 0),
    accumulators = c("r1", "r2"),
    type = "lba"
)

model3 <- BuildModel(
    p_map = list(A = "1", B = c("S", "G"), t0 = "1", mean_v = c("1"), sd_v = "1", st0 = "1"),
    factors = list(
        S = c("s1", "s2", "s3"),
        G = c("g1", "g2", "g3")
    ),
    accumulators = c("r1", "r2", "r3"),
    match_map = list(M = list(s1 = "r1", s2 = "r2", s3 = "r3")),
    constants = c(sd_v.false = 1, st0 = 0),
    type = "lba"
)
model4 <- BuildModel(
    p_map = list(A = "1", B = "S", t0 = "E", mean_v = c("D", "M"), sd_v = "M", st0 = "1"),
    factors = list(S = c("sti_1", "sti_2", "sti_3", "sti_4"), D = c("d1", "d2"), E = c("e1", "e2")),
    accumulators = c("resp_1", "resp_2", "resp_3", "resp_4"),
    match_map = list(M = list(sti_1 = "resp_1", sti_2 = "resp_2", sti_3 = "resp_3", sti_4 = "resp_4")),
    constants = c(sd_v.false = 1, st0 = 0),
    type = "lba"
)

model5 <- BuildModel(
    p_map = list(A = "1", B = c("S", "G"), t0 = "E", mean_v = c("D", "M"), sd_v = "M", st0 = "1"),
    factors = list(
        S = c("s1", "s2", "s3"), D = c("d1", "d2"), E = c("e1", "e2"),
        G = c("g1", "g2", "g3")
    ),
    accumulators = c("r1", "r2", "r3"),
    match_map = list(M = list(s1 = "r1", s2 = "r2", s3 = "r3")),
    constants = c(sd_v.false = 1, st0 = 0),
    type = "lba"
)
model6 <- BuildModel(
    p_map = list(
        A = "H", B = c("S", "G"), t0 = "E", mean_v = c("D", "H", "M"),
        sd_v = c("D", "M"), st0 = "1"
    ),
    factors = list(
        S = c("s1", "s2", "s3"), D = c("d1", "d2"), E = c("e1", "e2"),
        G = c("g1", "g2", "g3"), H = c("h1", "h2", "h3", "h4", "h5")
    ),
    accumulators = c("r1", "r2", "r3"),
    match_map = list(M = list(s1 = "r1", s2 = "r2", s3 = "r3")),
    constants = c(sd_v.false = 1, st0 = 0)
)

test_cases <- list(
    list(
        id = "mean_v x M, fixing 4 parameters",
        model = model0,
        expected_dim0 = 3, # free parameters
        expected_dim1 = 4, # conditions
        expected_dim2 = 7, # all parameters
        expected_boolean = 48
    ),
    list(
        id = "mean_v x M, three accumulators",
        model = model1,
        expected_dim0 = 5,
        expected_dim1 = 9,
        expected_dim2 = 8,
        expected_boolean = 162
    ),
    list(
        id = "B, mean_v and sd_v",
        model = model2,
        expected_dim0 = 9,
        expected_dim1 = 8,
        expected_dim2 = 11,
        expected_boolean = 96
    ),
    list(
        id = "B (S, G)",
        model = model3,
        expected_dim0 = 13,
        expected_dim1 = 27,
        expected_dim2 = 14,
        expected_boolean = 486
    ),
    list(
        id = "Flexible stimulus name",
        model = model4,
        expected_dim0 = 12,
        expected_dim1 = 64,
        expected_dim2 = 14,
        expected_boolean = 1536
    ),
    list(
        id = "B (S, G), t0, mean_v, sd_v",
        model = model5,
        expected_dim0 = 17,
        expected_dim1 = 108,
        expected_dim2 = 19,
        expected_boolean = 1944
    ),
    list(
        id = "B (2 factors), t0, mean_v (3 factors), sd_v (2 factors)",
        model = model6,
        expected_dim0 = 40,
        expected_dim1 = 540,
        expected_dim2 = 41,
        expected_boolean = 9720
    )
)



for (tc in test_cases) {
    cat(" Model: ", tc$id, "\n")
    test_that(tc$id, test_model_boolean(tc))
}
