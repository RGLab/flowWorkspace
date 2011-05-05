library(testthat)
library(flowWorkspace)
require(flowWorkspaceData)

d<-system.file("extdata",package="flowWorkspaceData")
wsfile<-list.files(d,pattern="A2004Analysis.xml",full=TRUE)

test_package("flowWorkspace")

