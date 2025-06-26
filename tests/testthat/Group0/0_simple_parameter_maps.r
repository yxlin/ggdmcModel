# q(save = "no")

test_that("\n== Associating the factor levels to the model parameter (simple cases) ==\n", {
    # cat("\nWorking directory: ", getwd(), "\n")
    cat("\n-------------------\n")
    cat("|   SIMPLE MAPS   |")
    cat("\n-------------------\n")


    run_test <- function(p_map, factors, msg, expected_result) {
        cat("\n--- Test Case:", msg, "---\n")
        cat("The user entered the parameter:\n")

        print(unlist(p_map))
        cat("\nFactors:\n")
        print(unlist(factors))

        res <- bind_condition2parameters_r(p_map, factors)
        cat("\nResult:\n")
        print(res)
        if (is.character(res)) {
            testthat::expect_equal(expected_result, res)
        } else if (is.numeric(tc$expected)) {
            stop("unknown situation")
            # expect_true(n_factor_cell == tc$expected)
        }

        invisible(res) # Return invisibly to avoid clutter
    }

    # Test 1: mean_v x M
    run_test(
        p_map = list(A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "1", st0 = "1"),
        factors = list(S = c("red", "blue")),
        msg = "1. mean_v x M",
        expected_result = c("A", "B", "mean_v.false", "mean_v.true", "sd_v", "st0", "t0")
    )
    # Test 2: Three accumulators - mean_v x M & sd_v x M
    run_test(
        p_map = list(A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "M", st0 = "1"),
        factors = list(S = c("red", "yellow", "green")),
        msg = "2. Three accumulators - mean_v x M & sd_v x M",
        expected_result = c("A", "B", "mean_v.false", "mean_v.true", "sd_v.false", "sd_v.true", "st0", "t0")
    )
    # Test 3: Four accumulators - mean_v x M & sd_v x M
    run_test(
        p_map = list(A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "M", st0 = "1"),
        factors = list(S = c("red", "blue", "yellow", "green")),
        msg = "3. Four accumulators - mean_v x M & sd_v x M",
        expected_result = c("A", "B", "mean_v.false", "mean_v.true", "sd_v.false", "sd_v.true", "st0", "t0")
    )

    # Test 4: B x S & mean_v x c(D, M), sd_v x M
    run_test(
        p_map = list(A = "1", B = "S", t0 = "1", mean_v = c("D", "M"), sd_v = "M", st0 = "1"),
        factors = list(S = c("red", "blue"), D = c("d1", "d2")),
        msg = "4. B x S & mean_v x c(D, M), sd_v x M",
        expected_result = c(
            "A", "B.blue", "B.red", "mean_v.d1.false", "mean_v.d1.true", "mean_v.d2.false",
            "mean_v.d2.true", "sd_v.false", "sd_v.true", "st0", "t0"
        )
    )

    # Test 5: B x S & mean_v x c(D, M), sd_v x M
    run_test(
        p_map = list(A = "1", B = "S", t0 = "E", mean_v = c("D", "M"), sd_v = "M", st0 = "1"),
        factors = list(S = c("sti_1", "sti_2", "sti_3", "sti_4"), D = c("d1", "d2"), E = c("quiet", "noise")),
        msg = "5. B x S & mean_v x c(D, M), sd_v x M, t0 x E",
        expected_result = c(
            "A", "B.sti_1", "B.sti_2", "B.sti_3", "B.sti_4", "mean_v.d1.false", "mean_v.d1.true",
            "mean_v.d2.false", "mean_v.d2.true", "sd_v.false", "sd_v.true", "st0", "t0.noise", "t0.quiet"
        )
    )

    # Test 6: B x S & mean_v x c(D, M), sd_v x M
    run_test(
        p_map = list(A = "1", B = c("S", "G"), t0 = "E", mean_v = c("D", "M"), 
                     sd_v = "M", st0 = "1"),
        factors = list(
            S = c("s1", "s2", "s3"), D = c("d1", "d2"), E = c("e1", "e2"),
            G = c("g1", "g2", "g3")
        ),
        msg = "6. Four factors: 'B', 'mean_v', 'sd_v', and 't0' ",
        expected_result = c("A", "B.s1.g1", "B.s1.g2", "B.s1.g3", "B.s2.g1", 
                            "B.s2.g2",  "B.s2.g3", "B.s3.g1",  "B.s3.g2",  
                            "B.s3.g3", "mean_v.d1.false", "mean_v.d1.true", 
                            "mean_v.d2.false", "mean_v.d2.true", "sd_v.false", 
                            "sd_v.true", "st0", "t0.e1", "t0.e2")
    )
})


cat("\n----------------------------\n")
cat("|   SIMPLE MAPS additional")
cat("\n----------------------------\n")

test_that("add_M handles basic cases", {
    p_map <- list(A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "M", st0 = "1")
    factors <- list(S = c("s1", "s2"))
    res <- bind_condition2parameters_r(p_map, factors)

    expect_true(is.vector(res))
    expect_equal(length(res), 8) # Check expected output
})

test_that("Model with 15 parameters", {
    p_map <- list(A = "1", B = c("S", "G"), t0 = "1", mean_v = "M", sd_v = "1", st0 = "1")
    factors <- list(S = c("s1", "s2", "s3"), G = c("g1", "g2", "g3"))
    res <- bind_condition2parameters_r(p_map, factors)

    expect_equal(length(res), 15) # 3 (S) * 3 (G)
})


test_that("A model design using 41 parameters", {
    p_map <- list(
        A = "H", B = c("S", "G"), t0 = "E", mean_v = c("D", "H", "M"),
        sd_v = c("D", "M"), st0 = "1"
    )

    factors <- list(
        S = c("s1", "s2", "s3"), D = c("d1", "d2"), E = c("e1", "e2"),
        G = c("g1", "g2", "g3"), H = c("h1", "h2", "h3", "h4", "h5")
    )


    res <- bind_condition2parameters_r(p_map, factors)

    expect_true(is.character(res))
    expect_equal(length(res), 41) # 3 (S) * 3 (G)
})
