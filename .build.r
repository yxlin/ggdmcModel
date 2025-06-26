q(save = "no")

tools::showNonASCIIfile("DESCRIPTION")
devtools::check()
usethis::use_gpl_license(version = 2) # or use_gpl_license(version = 2)


devtools::test()
devtools::build()
devtools::document()
devtools::install(quick = TRUE, upgrade = "never")
devtools::install()

remove.packages("ggdmcModel") # Remove existing installation
unlink("src/*.o") # Remove object files
unlink("src/*.so") # Remove shared objects
unlink("src/RcppExports.*") # Remove generated Rcpp files
devtools::document()
devtools::install()
