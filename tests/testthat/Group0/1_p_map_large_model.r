test_add_M <- function() {
    cat("\n================ Slightly complex add_M() ================\n")

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
            #    B = "1", # 6-2 OK
            B = "S", # 6-3 OK
            t0 = "E", # 6-4 PK
            mean_v = c("S", "D", "POLITICAL_VIEW", "M"), # 6-5
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

    # Test 2:
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


    # Test 3: Mixing different languages
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
            政黨傾向 = c("liberal", "conservative")
        ),
        msg = "81 parameters"
    )
}

test_add_M()
