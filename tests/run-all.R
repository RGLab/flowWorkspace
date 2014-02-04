library(testthat)
library(flowWorkspace)
library(data.table)

dataDir <- system.file("extdata",package="flowWorkspaceData")
resultDir <- system.file("tests/expect_result",package="flowWorkspace")
test_package("flowWorkspace")

#test_file("/home/wjiang2/rglab/workspace/flowWorkspace/inst/tests/test-archive.R")
#test_file("/home/wjiang2/rglab/workspace/flowWorkspace/inst/tests/test-parseWorkspace.R")