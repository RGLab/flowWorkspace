library(data.table)
library(utils)
library(dplyr)
library(Biobase)
dataDir <- system.file("extdata",package="flowWorkspaceData")
resultDir <- "expect_result"
data("GvHD")
