# q(save = "no")
# Sys.setenv("R_TESTS" = "")
## Workaround for the error,
## "cannot open file 'startup.Rs': No such file or directory" in Windows 10

library(testthat)
library(ggdmcModel)
cat("\nRunning testthat.r in the directory: ")
cat(getwd(), "\n")

cat("\n================= Group 0 tests =======================\n\n")
test_file(path = "Group0/0_simple_parameter_maps.r")
test_file(path = "Group0/1_p_map_large_model.r")
test_file(path = "Group0/2_very_large_p_map.r")
test_file(path = "Group0/3_build_cell_names.r")

cat("\n================= Group 1 tests =======================\n\n")
test_file(path = "Group1/0_split.r")
test_file(path = "Group1/1_param_x_cond.r")
test_file(path = "Group1/2_table_3parameters.r")
test_file(path = "Group1/3_table_5parameters.r")
test_file(path = "Group1/4_table_ddm_params.r")

cat("\n================= Group 2 tests =======================\n\n")
test_file(path = "Group2/0_node_1_index.r")
test_file(path = "Group2/1_model_boolean.r")

cat("\n================= Group 3 tests =======================\n\n")
test_file(path = "Group3/0_simple_models.r")
test_file(path = "Group3/1_slightly_complex.r")
test_file(path = "Group3/2_complex.r")
test_file(path = "Group3/3_most_complex_model.r")
