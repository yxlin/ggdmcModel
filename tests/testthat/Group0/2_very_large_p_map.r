test_add_M <- function() {
    cat("\n================ Complex add_M() ================\n")

    run_test <- function(p_map, factors, msg) {
        cat("\n--- Test Case:", msg, "---\n")
        cat("Parameters:\n")
        print(unlist(p_map))
        cat("\nFactors:\n")
        print(unlist(factors))

        res <- bind_condition2parameters_r(p_map, factors)
        cat("\nResult:\n")
        print(res)

        invisible(res) # Return invisibly to avoid clutter
    }

    # Test 1:
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

    # Test 2:
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

test_add_M()

test_that("A design with 81 parameters", {
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
    )

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
    )


    res <- bind_condition2parameters_r(p_map, factors)

    expect_true(is.character(res))
    expect_equal(length(res), 81) # 3 (S) * 3 (G)
})
