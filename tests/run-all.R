library(testthat)
library(flowWorkspace)
#require(flowWorkspaceData)

dataDir <- system.file("extdata",package="flowWorkspaceData")

test_package("flowWorkspace")


test_file("/home/wjiang2/rglab/workspace/flowWorkspace/inst/tests/test-GatingSet-accessors.R")