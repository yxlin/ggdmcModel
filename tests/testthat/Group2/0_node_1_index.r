# q(save = "no")

# test_stimulus_test <- function(tc) {
#     result <- get_stimulus_level_r(tc$p_map, tc$factors, tc$responses)
#     # print(result)
#     # print(length(result))

#     if (is.character(tc$expected_sti)) {
#         expect_equal(result, tc$expected_sti)
#     } else if (is.numeric(tc$expected_sti)) {
#         expect_true(length(result) == tc$expected_sti)
#     }
# }

# test_factor_cells_test <- function(tc) {
#     result <- get_factor_cells_r(tc$p_map, tc$factors, tc$responses)
#     n_factor_cell <- length(unlist(result))


#     if (is.character(tc$expected)) {
#         expect_equal(result, tc$expected)
#     } else if (is.numeric(tc$expected)) {
#         expect_true(n_factor_cell == tc$expected)
#     }
# }

test_node_1_index <- function(tc) {
    result <- get_node_1_index_r(tc$p_map, tc$factors, tc$responses)
    expect_equal(dim(result)[[1]], tc$expected_node1)
}


test_cases <- list(
    list(
        id = "Only mean_v and sd_v associated with the M factor",
        p_map = list(A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "M", st0 = "1"),
        factors = list(S = c("s1", "s2")),
        responses = c("r1", "r2"),
        expected = 4,
        expected_sti = 4,
        expected_node1 = 4
    ),
    list(
        id = "Three accumulator model",
        p_map = list(A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "M", st0 = "1"),
        factors = list(S = c("s1", "s2", "s3")),
        responses = c("r1", "r2", "r3"),
        expected = 9,
        expected_sti = 9,
        expected_node1 = 9
    ),
    list(
        id = "B, mean_v and sd_v",
        p_map = list(A = "1", B = "S", t0 = "1", mean_v = c("D", "M"), sd_v = "M", st0 = "1"),
        factors = list(S = c("s1", "s2"), D = c("d1", "d2")),
        responses = c("r1", "r2"),
        expected = 16,
        expected_sti = 8,
        expected_node1 = 8
    ),
    list(
        id = "B (S, G)",
        p_map = list(A = "1", B = c("S", "G"), t0 = "1", mean_v = c("1"), sd_v = "1", st0 = "1"),
        factors = list(
            S = c("s1", "s2", "s3"),
            G = c("g1", "g2", "g3")
        ),
        responses = c("r1", "r2", "r3"),
        expected = 54,
        expected_sti = 27,
        expected_node1 = 27
    ),
    list(
        id = "Flexible stimulus name",
        p_map = list(A = "1", B = "S", t0 = "E", mean_v = c("D", "M"), sd_v = "M", st0 = "1"),
        factors = list(S = c("sti_1", "sti_2", "sti_3", "sti_4"), D = c("d1", "d2"), E = c("e1", "e2")),
        responses = c("resp_1", "resp_2", "resp_3", "resp_4"),
        expected = 192,
        expected_sti = 64,
        expected_node1 = 64
    ),
    list(
        id = "B (S, G), t0, mean_v, sd_v",
        p_map = list(A = "1", B = c("S", "G"), t0 = "E", mean_v = c("D", "M"), sd_v = "M", st0 = "1"),
        factors = list(
            S = c("s1", "s2", "s3"), D = c("d1", "d2"), E = c("e1", "e2"),
            G = c("g1", "g2", "g3")
        ),
        responses = c("r1", "r2", "r3"),
        expected = 432,
        expected_sti = 108,
        expected_node1 = 108
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
        expected = 2700,
        expected_sti = 540,
        expected_node1 = 540
    )
)


# for (tc in test_cases) {
#     cat(" Stimulus levels: ", tc$id, "\n")
#     test_that(tc$id, test_stimulus_test(tc))
# }
# 
# 
# for (tc in test_cases) {
#     cat(" Factor cells: ", tc$id, "\n")
#     test_that(tc$id, test_factor_cells_test(tc))
# }

for (tc in test_cases) {
    cat(" Node 1 index matrix: ", tc$id, "\n")
    test_that(tc$id, test_node_1_index(tc))
}
