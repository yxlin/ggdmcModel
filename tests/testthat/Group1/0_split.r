# q(save = "no")
cat("\n\n-------------------- Testing split --------------------")
rm(list = ls())
pkg <- c("ggdmcModel")
suppressPackageStartupMessages(tmp <- sapply(pkg, require, character.only = TRUE))
cat("\nWorking directory: ", getwd(), "\n")


test_split_parameter_x_condition <- function() {
    cat("\n===== Testing add_M(): simple cases =====\n")

    run_test <- function(p_map, factors, msg) {
        cat("\n--- Test Case:", msg, "---\n")
        cat("Parameters:\n")
        print(unlist(p_map))

        cat("\nFactors:\n")
        print(unlist(factors))

        parameter_M <- bind_condition2parameters_r(p_map, factors)
        res <- split_parameter_x_condition(parameter_M)
        cat("\nResult:\n")
        print(res)

        invisible(res) # Return invisibly to avoid clutter
    }

    # Test 1: Simple case
    run_test(
        p_map = list(A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "1", st0 = "1"),
        factors = list(S = c("s1", "s2")),
        msg = "Only mean_v with M factor"
    )

    run_test(
        p_map = list(A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "M", st0 = "1"),
        factors = list(S = c("s1", "s2", "s3")),
        msg = "Three accumulators"
    )

    run_test(
        p_map = list(A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "M", st0 = "1"),
        factors = list(S = c("s1", "s2", "s3", "s4")),
        msg = "Four accumulators"
    )

    # Test 2: Slightly more complex
    run_test(
        p_map = list(A = "1", B = "S", t0 = "1", mean_v = c("D", "M"), sd_v = "M", st0 = "1"),
        factors = list(S = c("s1", "s2"), D = c("d1", "d2")),
        msg = "Two factors associated with 'B' and 'mean_v'"
    )

    run_test(
        p_map = list(A = "1", B = "S", t0 = "E", mean_v = c("D", "M"), sd_v = "M", st0 = "1"),
        factors = list(S = c("sti_1", "sti_2", "sti_3", "sti_4"), D = c("d1", "d2"), E = c("e1", "e2")),
        msg = "Customised S factor levels"
    )


    # Test 3: More complex
    run_test(
        p_map = list(
            A = "1", B = c("S", "G"), t0 = "E", mean_v = c("D", "M"), sd_v = "M",
            st0 = "1"
        ),
        factors = list(
            S = c("s1", "s2", "s3"), D = c("d1", "d2"), E = c("e1", "e2"),
            G = c("g1", "g2", "g3")
        ),
        msg = "Four factors, 'B', 't0', and 'mean_v'"
    )

    # Test 4:
    run_test(
        p_map = list(
            A = "H", B = c("S", "G"), t0 = "E", mean_v = c("D", "H", "M"),
            sd_v = c("D", "M"), st0 = "1"
        ),
        factors = list(
            S = c("s1", "s2", "s3"), D = c("d1", "d2"), E = c("e1", "e2"),
            G = c("g1", "g2", "g3"), H = c("liberal", "conservative")
        ),
        msg = "Five factors"
    )


    run_test(
        p_map = list(
            A = "POLITICAL_VIEW", B = c("S", "G"), t0 = "E",
            mean_v = c("D", "POLITICAL_VIEW", "M"),
            sd_v = c("D", "M"), st0 = "1"
        ),
        factors = list(
            S = c("s1", "s2", "s3"), D = c("d1", "d2"), E = c("e1", "e2"),
            G = c("g1", "g2", "g3"), POLITICAL_VIEW = c("liberal", "conservative")
        ),
        msg = "Five factors"
    )


    run_test(
        p_map = list(
            A = "POLITICAL_VIEW",
            B = "S",
            t0 = "E",
            mean_v = c("S", "D", "POLITICAL_VIEW", "M"),
            sd_v = "1",
            st0 = "1"
        ),
        factors = list(
            S = c("s1", "s2", "s3"),
            D = c("d1", "d2"),
            E = c("e1", "e2"),
            POLITICAL_VIEW = c("liberal", "conservative")
        ),
        msg = "Four factors"
    )

    # Test 5:
    run_test(
        p_map = list(
            A = "POLITICAL_VIEW", B = c("S", "G"), t0 = c("HANDEDNESS", "MOTOR_SKILL"),
            mean_v = c("D", "POLITICAL_VIEW", "M"),
            sd_v = c("D", "M"), st0 = "1"
        ),
        factors = list(
            S = c("s1", "s2", "s3"), D = c("d1", "d2"),
            G = c("g1", "g2", "g3"),
            HANDEDNESS = c("left_hander", "right_hander"),
            MOTOR_SKILL = c("excellent", "good", "poor"),
            POLITICAL_VIEW = c("liberal", "conservative")
        ),
        msg = "Six factors"
    )


    run_test(
        p_map = list(
            A = "POLITICAL_VIEW",
            B = c("S", "G"),
            t0 = c("HANDEDNESS", "MOTOR_SKILL"),
            mean_v = c("D", "POLITICAL_VIEW", "M"),
            sd_v = c("D", "M"),
            st0 = "1"
        ),
        factors = list(
            S = c("sti_red", "sti_yellow", "sti_green"),
            D = c("d1", "d2"),
            G = c("g1", "g2", "g3"),
            HANDEDNESS = c("left_hander", "right_hander"),
            MOTOR_SKILL = c("excellent", "good", "poor"),
            POLITICAL_VIEW = c("liberal", "conservative")
        ),
        msg = "Six factors"
    )


    # Test 6
    run_test(
        p_map = list(
            A = "政黨傾向", B = c("S", "G"),
            t0 = c("HANDEDNESS", "MOTOR_SKILL"),
            mean_v = c("D", "政黨傾向", "M"),
            sd_v = c("D", "M"), st0 = "1"
        ),
        factors = list(
            S = c("sti_red", "sti_yellow", "sti_green"),
            D = c("d1", "d2"),
            G = c("g1", "g2", "g3"),
            HANDEDNESS = c("left_hander", "right_hander"),
            MOTOR_SKILL = c("excellent", "good", "poor"),
            政黨傾向 = c("自由派", "保守派")
        ),
        msg = "81 parameters"
    )

    # Test 7 and other
    run_test(
        p_map = list(
            A = "政党傾向", B = c("S", "G"),
            t0 = c("HANDEDNESS", "MOTOR_SKILL"),
            mean_v = c("D", "政党傾向", "M"),
            sd_v = c("D", "M"), st0 = "1"
        ),
        factors = list(
            S = c("sti_red", "sti_yellow", "sti_green"),
            D = c("d1", "d2"),
            G = c("g1", "g2", "g3"),
            HANDEDNESS = c("left_hander", "right_hander"),
            MOTOR_SKILL = c("excellent", "good", "poor"),
            政党傾向 = c("自由派", "保守派")
        ),
        msg = "Six factors"
    )


    run_test(
        p_map = list(
            A = "政党傾向", B = c("S", "G"),
            t0 = c("HANDEDNESS", "MOTOR_SKILL"),
            mean_v = c("D", "政党傾向", "M"),
            sd_v = c("D", "環境噪音", "M"), st0 = "1"
        ),
        factors = list(
            S = c("sti_red", "sti_yellow", "sti_green"),
            D = c("d1", "d2"),
            G = c("g1", "g2", "g3"),
            HANDEDNESS = c("left_hander", "right_hander"),
            MOTOR_SKILL = c("excellent", "good", "poor"),
            政党傾向 = c("自由派", "保守派"),
            環境噪音 = c("安靜", "中等", "吵雜")
        ),
        msg = "Seven factors"
    )


    run_test(
        p_map = list(
            A = "政黨傾向", B = c("S", "G"),
            t0 = c("HANDEDNESS", "MOTOR_SKILL"),
            mean_v = c("D", "政黨傾向", "M"),
            sd_v = c("D", "環境噪音", "M"), st0 = "1"
        ),
        factors = list(
            S = c("紅", "黃", "藍", "綠"),
            D = c("d1", "d2"),
            G = c("g1", "g2", "g3"),
            HANDEDNESS = c("left_hander", "right_hander"),
            MOTOR_SKILL = c("excellent", "good", "poor"),
            政黨傾向 = c("自由派", "保守派"),
            環境噪音 = c("安靜", "中等", "吵雜")
        ),
        msg = "Seven factors"
    )

    run_test(
        p_map <- list(
            A = "政党傾向",
            B = c("S", "G"),
            t0 = c("HANDEDNESS", "MOTOR_SKILL"),
            # DMC uses the user entered sequence here. The
            # new version uses std::map to enforce alphabetical order, with
            # specific design to handle key factors, S, R and M.
            mean_v = c("D", "E", "F", "政党傾向", "M"),
            sd_v = c("D", "環境噪音", "M"),
            st0 = "1"
        ),
        factors <- list(
            S = c("紅", "黃", "藍", "綠"),
            D = c("d1", "d2"),
            E = c("e1", "e2"),
            F = c("f1", "f2", "f3"),
            G = c("g1", "g2", "g3"),
            HANDEDNESS = c("left_hander", "right_hander"),
            MOTOR_SKILL = c("excellent", "good", "poor"),
            政党傾向 = c("自由派", "保守派"),
            環境噪音 = c("安靜", "中等", "吵雜")
        ),
        msg = "Largest model so far"
    )
}

# Run all tests
test_split_parameter_x_condition()
