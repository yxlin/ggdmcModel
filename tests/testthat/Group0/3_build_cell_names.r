# q(save = "no")
# Helper function to run tests
run_test <- function(tc) {
    result <- build_cell_names_r(tc$p_map, tc$factors, tc$responses)
    cell_names <- result[[1]]

    if (is.character(tc$expected)) {
        expect_equal(cell_names, tc$expected)
    } else if (is.numeric(tc$expected)) {
        expect_true(length(cell_names) == tc$expected)
    }
}

test_cases <- list(
    list(
        id = "Only mean_v and sd_v associated with the M factor",
        p_map = list(A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "M", st0 = "1"),
        factors = list(S = c("s1", "s2")),
        responses = c("r1", "r2"),
        expected = c("s1.r1", "s1.r2", "s2.r1", "s2.r2")
    ),
    list(
        id = "Three accumulator model",
        p_map = list(A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "M", st0 = "1"),
        factors = list(S = c("s1", "s2", "s3")),
        responses = c("r1", "r2", "r3"),
        expected = c("s1.r1", "s1.r2", "s1.r3", "s2.r1", "s2.r2", "s2.r3", "s3.r1", "s3.r2", "s3.r3")
    ),
    list(
        id = "B, mean_v and sd_v",
        p_map = list(A = "1", B = "S", t0 = "1", mean_v = c("D", "M"), sd_v = "M", st0 = "1"),
        factors = list(S = c("s1", "s2"), D = c("d1", "d2")),
        responses = c("r1", "r2"),
        expected = c(
            "s1.d1.r1", "s1.d1.r2", "s1.d2.r1", "s1.d2.r2", "s2.d1.r1", "s2.d1.r2", "s2.d2.r1",
            "s2.d2.r2"
        )
    ),
    list(
        id = "B (S, G)",
        p_map = list(A = "1", B = c("S", "G"), t0 = "1", mean_v = c("1"), sd_v = "1", st0 = "1"),
        factors = list(
            S = c("s1", "s2", "s3"),
            G = c("g1", "g2", "g3")
        ),
        responses = c("r1", "r2", "r3"),
        expected = c(
            "s1.g1.r1", "s1.g1.r2", "s1.g1.r3", "s1.g2.r1", "s1.g2.r2", "s1.g2.r3",
            "s1.g3.r1", "s1.g3.r2", "s1.g3.r3", "s2.g1.r1", "s2.g1.r2", "s2.g1.r3",
            "s2.g2.r1", "s2.g2.r2", "s2.g2.r3", "s2.g3.r1", "s2.g3.r2", "s2.g3.r3",
            "s3.g1.r1", "s3.g1.r2", "s3.g1.r3", "s3.g2.r1", "s3.g2.r2", "s3.g2.r3",
            "s3.g3.r1", "s3.g3.r2", "s3.g3.r3"
        )
    ),
    list(
        id = "Flexible stimulus name",
        p_map = list(A = "1", B = "S", t0 = "E", mean_v = c("D", "M"), sd_v = "M", st0 = "1"),
        factors = list(S = c("sti_1", "sti_2", "sti_3", "sti_4"), D = c("d1", "d2"), E = c("e1", "e2")),
        responses = c("resp_1", "resp_2", "resp_3", "resp_4"),
        expected = 64
    ),
    list(
        id = "B (S, G), t0, mean_v, sd_v",
        p_map = list(A = "1", B = c("S", "G"), t0 = "E", mean_v = c("D", "M"), sd_v = "M", st0 = "1"),
        factors = list(
            S = c("s1", "s2", "s3"), D = c("d1", "d2"), E = c("e1", "e2"),
            G = c("g1", "g2", "g3")
        ),
        responses = c("r1", "r2", "r3"),
        expected = 108
    ),
    list(
        id = "B (2 factors), t0, mean_v (3 factors), sd_v (2 factors)",
        p_map = list(
            A = "H", B = c("S", "G"), t0 = "E", mean_v = c("D", "H", "M"),
            sd_v = c("D", "M"), st0 = "1"
        ),
        factors = list(
            S = c("s1", "s2", "s3"), D = c("d1", "d2"), E = c("e1", "e2"),
            G = c("g1", "g2", "g3"), H = c("h1", "h2", "h3", "h4", "h5")
        ),
        responses = c("r1", "r2", "r3"),
        expected = 540
    )
)

# require(testthat)
# Run all test cases
for (tc in test_cases) {
    cat("Test case: ", tc$id, "\n")
    test_that(tc$id, run_test(tc))
}
