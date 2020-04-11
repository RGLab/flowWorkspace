library(data.table)
library(utils)
library(dplyr)
library(flowCore)
library(Biobase)
dataDir <- system.file("extdata",package="flowWorkspaceData")
resultDir <- "expect_result"
# resultDir <- "tests/testthat/expect_result/"
data("GvHD")
win32_flag = .Platform$OS.type == "windows" && .Machine$sizeof.pointer != 8