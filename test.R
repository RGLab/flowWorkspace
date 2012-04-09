#unloadNamespace("flowWorkspace")
library(flowWorkspace)
library(Rgraphviz)

#source("/home/wjiang2/rglab/workspace/flowWorkspace/R/InternalClasses.R")
#source("/home/wjiang2/rglab/workspace/flowWorkspace/R/GatingHierarchyInternal-Methods.R")
#source("/home/wjiang2/rglab/workspace/flowWorkspace/R/GatingSetInternal-Methods.R")


macXML<-"/home/wjiang2/rglab/workspace/HIPC-Lyoplate/data/HIPC_trial.xml"
winXML<-"/home/wjiang2/rglab/workspace/flowWorkspace/fjWsExamples/LyoplateTest1Yale.wsp"


############################################################################### 
#R parser
###############################################################################

ws<-openWorkspace(macXML)
G1<-parseWorkspace(ws,name=1,subset=c(1:2),execute=T)
G1
length(G1)
gh1<-G1[[1]]
gh1
getSample(gh1)
getNodes(gh1)
getParent(gh1,3)
getParent(gh1,"96.Lymph")
getChildren(gh1,"96.Lymph")

head(getPopStats(gh1))
getProp(gh1,"96.Lymph")
getTotal(gh1,"96.Lymph")

plot(gh1)

pData(G1)<-data.frame(sample=getSamples(G1))
splitGatingSetByNgates(G1)
############################################################################### 
#cpp parser
###############################################################################
G<-parseWorkspace(winXML,groupID=1,dMode=2)
G

length(G)
getSamples(G)



gh<-G[[3]]
gh

getSample(gh)
getNodes(gh)
getNodes(gh,tsort=T)
getNodes(gh,isPath=T)
getParent(gh,3)
getParent(gh,"2.Lymphocytes")


getChildren(gh,"2.Lymphocytes")
getChildren(gh,"4.B-Cells")

getProp(gh,"2.Lymphocytes")
getTotal(gh,"2.Lymphocytes")

flowWorkspace:::.getPopStat(gh,1)
head(getPopStats(gh))


plot(G[[3]])


pData(G)<-data.frame(sample=getSamples(G))
G_list<-flowWorkspace:::splitGatingSetByNgates(G)

lapply(G_list,getPopStats,flowJo=T)


