[![Build Status](https://travis-ci.org/RGLab/flowWorkspace.png?branch=trunk)](https://travis-ci.org/RGLab/flowWorkspace)

# flowWorkspace: An infrastructure tool for hierarchically gated flow cytometry data.     

This package is designed to store, query and visualize the hierarchical gated flow data.

It also facilitates the comparison of automated gating methods against manual gating by importing basic FlowJo 
workspaces into R and replicating the gating from flowJo using the flowCore functionality. Gating hierarchies, 
groups of samples, compensation, and transformation are performed so that the output matches the FlowJo analysis.

### INSTALLATION

```r
# First, install it from bionconductor so that it will pull all the dependent packages automatically
library(BiocInstaller)
biocLite(flowWorkspace) # may be older
biocLite(CytoML) # For parsing FlowJo workspaces
# Then, install the latest version from github using devtools package 
install.packages("devtools") 
library(devtools) #load it
install_github("RGLab/flowWorkspace", ref="trunk")
install_github("RGLab/CytoML", ref="trunk")

```

### Import flowJo workspace

```r
library(flowWorkspace)
library(CytoML)
dataDir <- system.file("extdata", package="flowWorkspaceData")
wsfile <- list.files(dataDir, pattern="manual.xml",full=TRUE)
ws <- open_flowjo_xml(wsfile);
gs <- flowjo_to_gatingset(ws, path = dataDir, name = 4, subset = "CytoTrol_CytoTrol_1.fcs")
gs

#get the first sample
gh <- gs[[1]]
#plot the hierarchy tree
plot(gh)
#show the gating paths to all the cell populations(/nodes)
gs_get_pop_paths(gh)
#show the population statistics
gh_pop_compare_stats(gh)
#plot the gates
autoplot(gh) 

```
### More examples:
* [Importing flowJo Workspaces into R](http://bioconductor.org/packages/3.0/bioc/vignettes/flowWorkspace/inst/doc/flowWorkspace.pdf)
* [How to plot gated data](http://bioconductor.org/packages/3.0/bioc/vignettes/flowWorkspace/inst/doc/plotGate.html)
* [How to merge GatingSets](http://bioconductor.org/packages/3.0/bioc/vignettes/flowWorkspace/inst/doc/HowToMergeGatingSet.html)
