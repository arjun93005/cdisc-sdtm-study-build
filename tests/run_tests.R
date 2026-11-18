# Run unit tests from the project root:  Rscript tests/run_tests.R
source("R/utils/sdtm_utils.R")
testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE)
