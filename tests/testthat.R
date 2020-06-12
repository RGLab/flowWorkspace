library(testthat)
library(flowWorkspace)

test_check("flowWorkspace")

#devtools::test("~/rglab/workspace/flowWorkspace")
#devtools::check_man()
test_file("/home/wjiang2/rglab/workspace/flowWorkspace/tests/testthat/test-main.R")
# test_file("/home/wjiang2/rglab/workspace/flowWorkspace/tests/testthat/GatingSetList-testSuite.R")
# test_file("/home/wjiang2/rglab/workspace/flowWorkspace/tests/testthat/GatingSet-testSuite.R")
# test_file("/home/wjiang2/rglab/workspace/flowWorkspace/tests/testthat/GatingHierarchy-testSuite.R")
# test_file("/home/wjiang2/rglab/workspace/flowWorkspace/tests/testthat/test-gs-parsed.R")
# test_file("/home/wjiang2/rglab/workspace/flowWorkspace/tests/testthat/test-add.R")
# test_file("/home/wjiang2/rglab/workspace/flowWorkspace/tests/testthat/test-compensate.R")
# test_file("/home/wjiang2/rglab/workspace/flowWorkspace/tests/testthat/InternalTestSuite.R")

