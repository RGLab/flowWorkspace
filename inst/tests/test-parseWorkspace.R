#context("Workspace Functions")
#
#wsfile <- list.files(dataDir, pattern="manual.xml",full=TRUE)
#
#ws <- openWorkspace(wsfile);
#
#test_that("can load xml workspace",
#{
#expect_that(ws, is_a("flowJoWorkspace"))
#})
#
#
#G<-try(parseWorkspace(ws,execute=TRUE,path=dataDir,name=1));
#test_that("Can parse workspace",{
#	expect_that(G,is_a("GatingSet"));
#	expect_that(G[[1]],is_a("GatingHierarchy"));
#})
#
#test_that("Population proportions are valid",{
#	expect_that(all(sapply(getPopStats(G[[1]])[,1],function(x)!is.nan(x))),is_true())
#})
#
