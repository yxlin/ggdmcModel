# q(save = "no")
cat("\n\n-------------------- Testing a very complex model --------------------")
rm(list = ls())
pkg <- c("ggdmcModel")
suppressPackageStartupMessages(tmp <- sapply(pkg, require, character.only = TRUE))
cat("\nWorking directory: ", getwd(), "\n")

model <- BuildModel(
    p_map = list(
        A = "政黨傾向",
        B = c("S", "G"),
        t0 = c("HANDEDNESS", "MOTOR_SKILL"),
        mean_v = c("D", "E", "F", "政黨傾向", "M"),
        sd_v = c("D", "環境噪音", "M"),
        st0 = "1"
    ),
    match_map = list(M = list(
        紅 = "反應東", 黃 = "反應南",
        藍 = "反應西", 綠 = "反應北"
    )),
    factors = list(
        S = c("紅", "黃", "藍", "綠"),
        D = c("d1", "d2"),
        E = c("e1", "e2"),
        F = c("f1", "f2", "f3"),
        G = c("g1", "g2", "g3"),
        HANDEDNESS = c("left_hander", "right_hander"),
        MOTOR_SKILL = c("excellent", "good", "poor"),
        政黨傾向 = c("自由派", "保守派"),
        環境噪音 = c("安靜", "中等", "吵雜")
    ),
    constants = c(st0 = 0, sd_v.d1.安靜.false = 1),
    accumulators = c("反應東", "反應南", "反應西", "反應北"),
    type = "lba"
)
# First 10 of 79 parameters (use method = 'sample' or 'all' for more):
# A.自由派        A.保守派        B.紅.g1 B.黃.g1 B.藍.g1 B.綠.g1 B.紅.g2 B.黃.g2 B.藍.g2 B.綠.g2
# ... (69 more omitted)

# First 10 of 20736 cell names (use method = 'sample' or 'all' for more):
# 紅.d1.e1.f1.g1.left_hander.excellent.自由派.安靜.反應東 紅.d1.e1.f1.g1.left_hander.excellent.自由派.安靜.反應南    紅.d1.e1.f1.g1.left_hander.excellent.自由派.安靜.反應西 紅.d1.e1.f1.g1.left_hander.excellent.自由派.安靜.反應北     紅.d1.e1.f1.g1.left_hander.excellent.自由派.中等.反應東 紅.d1.e1.f1.g1.left_hander.excellent.自由派.中等.反應南     紅.d1.e1.f1.g1.left_hander.excellent.自由派.中等.反應西 紅.d1.e1.f1.g1.left_hander.excellent.自由派.中等.反應北     紅.d1.e1.f1.g1.left_hander.excellent.自由派.吵雜.反應東 紅.d1.e1.f1.g1.left_hander.excellent.自由派.吵雜.反應南
# ... (20726 more omitted)
