[![Build Status](https://travis-ci.org/RGLab/flowWorkspace.png?branch=trunk)](https://travis-ci.org/RGLab/flowWorkspace)

# flowWorkspace: An infrastructure tool for the hierarchical gated flow cytometry data.     

This package is designed to store, query and visualize the hierarchical gated flow data.

It also facilitates the comparison of automated gating methods against manual gating by 

importing basic flowJo workspaces into R and replicate the gating from flowJo using the flowCore functionality. Gating hierarchies,

groups of samples, compensation, and transformation are performed so that the output matches the flowJo analysis.

### INSTALLATION

```r
# First, install it from bionconductor so that it will pull all the dependent packages automatically
library(BiocInstalller)
bicLite(flowWorkspace) # may be older
# Then, install the latest version from github using devtools package 
install.packages("devtools") 
library(devtools) #load it
install_github("flowWorkspace/RGLab", ref="trunk")

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
