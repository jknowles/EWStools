library(testthat)
library(EWStools)
test_check("EWStools")

# 
# test_file("tests/testthat/test-caretobjects.R")
# test_file("tests/testthat/test-glmobjects.R")
# test_file("tests/testthat/test-modsearch.R")
# test_file("tests/testthat/test-regression.R")
# test_file("tests/testthat/test-builddata.R")
# test_file("tests/testthat/test-DIStest.R")
#test_file("tests/testthat/test-utilities.R")

# test them all
test_file("tests/testthat/test-caretobjects.R", reporter = "minimal")
test_file("tests/testthat/test-glmobjects.R", reporter = "minimal")
test_file("tests/testthat/test-modsearch.R", reporter = "minimal")
test_file("tests/testthat/test-regression.R", reporter = "minimal")
test_file("tests/testthat/test-builddata.R", reporter = "minimal")
test_file("tests/testthat/test-DIStest.R", reporter = "minimal")
# test_file("tests/testthat/test-utilities.R", reporter = "minimal")