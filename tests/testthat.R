library(testthat)
library(flowWorkspace)

test_check("flowWorkspace")

#devtools::test("~/rglab/workspace/flowWorkspace")
#devtools::check_man()
# test_file("~/rglab/workspace/flowWorkspace/tests/testthat/test-cytoframe.R")
# test_file("~/rglab/workspace/flowWorkspace/tests/testthat/test-cytoset.R")
test_file("/home/wjiang2/rglab/workspace/flowWorkspace/tests/testthat/test-archive.R")
# test_file("/home/wjiang2/rglab/workspace/flowWorkspace/tests/testthat/GatingSetList-testSuite.R")
# test_file("/home/wjiang2/rglab/workspace/flowWorkspace/tests/testthat/GatingSet-testSuite.R")
#test_file("/home/wjiang2/rglab/workspace/flowWorkspace/tests/testthat/GatingHierarchy-testSuite.R")
# test_file("/home/wjiang2/rglab/workspace/flowWorkspace/tests/testthat/test-parseWorkspace.R")
# test_file("/home/wjiang2/rglab/workspace/flowWorkspace/tests/testthat/test-add.R")
#test_file("/home/wjiang2/rglab/workspace/flowWorkspace/tests/testthat/InternalTestSuite.R")

