# q(save = "no")
# Sys.setenv("R_TESTS" = "")
## Workaround for the error,
## "cannot open file 'startup.Rs': No such file or directory" in Windows 10

library(testthat)
library(ggdmcModel)
cat("\nRunning testthat.r in the directory: ")
cat(getwd(), "\n")

home_dir <- "/media/yslin/Tui/01_Projects/ggdmcModel/tests/testthat"

cat("\n================= Group 0 tests =======================\n\n")
Group0 <- "Group0"
file0 <- file.path(home_dir, Group0, "0_simple_parameter_maps.r")
file1 <- file.path(home_dir, Group0, "1_p_map_large_model.r")
file2 <- file.path(home_dir, Group0, "2_very_large_p_map.r")
file3 <- file.path(home_dir, Group0, "3_build_cell_names.r")
test_file(file0)
test_file(file1)
test_file(file2)
test_file(file3)


cat("\n================= Group 1 tests =======================\n\n")
Group1 <- "Group1"
file0 <- file.path(home_dir, Group1, "0_split.r")
file1 <- file.path(home_dir, Group1, "1_param_x_cond.r")
file2 <- file.path(home_dir, Group1, "2_table_3parameters.r")
file3 <- file.path(home_dir, Group1, "3_table_5parameters.r")
file4 <- file.path(home_dir, Group1, "4_table_ddm_params.r")
test_file(file0)
test_file(file1)
test_file(file2)
test_file(file3)
test_file(file4)


cat("\n================= Group 2 tests =======================\n\n")
Group2 <- "Group2"
file0 <- file.path(home_dir, Group2, "0_node_1_index.r")
file1 <- file.path(home_dir, Group2, "1_model_boolean.r")
test_file(file0)
test_file(file1)


cat("\n================= Group 3 tests =======================\n\n")
Group3 <- "Group3"
file0 <- file.path(home_dir, Group3, "0_simple_models.r")
file1 <- file.path(home_dir, Group3, "1_slightly_complex.r")
file2 <- file.path(home_dir, Group3, "2_complex.r")
file3 <- file.path(home_dir, Group3, "3_most_complex_model.r")
test_file(file0)
test_file(file1)
test_file(file2)
test_file(file3)
