library(data.table)
library(utils)
library(dplyr)

dataDir <- system.file("extdata",package="flowWorkspaceData")
resultDir <- "expect_result"
# resultDir <- "tests/testthat/expect_result/"
data("GvHD")
