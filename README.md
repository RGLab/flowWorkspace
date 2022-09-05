# <img src="logo_mid.png" align="right" />


# flowWorkspace: An infrastructure tool for the hierarchical gated flow cytometry data.     
[![Build Status](https://github.com/RGLab/flowWorkspace/workflows/build/badge.svg?branch=master)](https://github.com/RGLab/flowWorkspace/actions)

This package is designed to store, query and visualize the hierarchical gated flow data.

It also facilitates the comparison of automated gating methods against manual gating by 

importing basic flowJo workspaces into R and replicate the gating from flowJo using the flowCore functionality. Gating hierarchies,

groups of samples, compensation, and transformation are performed so that the output matches the flowJo analysis.

### Reporting Bugs or Issues
- Use the issue template in github when creating a new issue. 
- Follow the instructions in the template (do your background reading).
- Search and verify that the issue hasn't already been addressed.
- Check the Bioconductor support site. 
- Make sure your flow packages are up to date.
- THEN if your issue persists, file a bug report.

Otherwise, we may close your issue without responding.

### INSTALLATION

```r
# First, install it from bionconductor so that it will pull all the dependent packages automatically
biocManager::install("flowWorkspace") # may be older
# Then, install the latest version from github using devtools package 
install.packages("devtools") 
library(devtools) #load it
install_github("RGLab/flowWorkspace")

```

### Import flowJo workspace

```r
library(flowWorkspace)
dataDir <- system.file("extdata", package="flowWorkspaceData")
wsfile <- list.files(dataDir, pattern="manual.xml",full=TRUE)
ws <- openWorkspace(wsfile);
gs <- parseWorkspace(ws, path = dataDir, name = 4, subset = "CytoTrol_CytoTrol_1.fcs")
gs

#get the first sample
gh <- gs[[1]]
#plot the hierarchy tree
plot(gh)
#show all the cell populations(/nodes)
getNodes(gh)
#show the population statistics
getPopStats(gh)
#plot the gates
plotGate(gh) 

```
### More examples:
* [Importing flowJo Workspaces into R](http://bioconductor.org/packages/3.0/bioc/vignettes/flowWorkspace/inst/doc/flowWorkspace.pdf)
* [How to plot gated data](http://bioconductor.org/packages/3.0/bioc/vignettes/flowWorkspace/inst/doc/plotGate.html)
* [How to merge GatingSets](http://bioconductor.org/packages/3.0/bioc/vignettes/flowWorkspace/inst/doc/HowToMergeGatingSet.html)
