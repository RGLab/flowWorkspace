#unloadNamespace("flowWorkspace")
library(flowWorkspace)
library(Rgraphviz)

#source("~/rglab/workspace/flowWorkspace/R/InternalClasses.R")
#source("~/rglab/workspace/flowWorkspace/R/GatingHierarchyInternal-Methods.R")
#source("~/rglab/workspace/flowWorkspace/R/GatingSetInternal-Methods.R")

path<-"/home/wjiang2/rglab/workspace/flowWorkspace/fjWsExamples"

macXML<-"HIPC_trial.xml"
macXML<-file.path(path,macXML)

winXML<-c("LyoplateTest1Yale.wsp","Exp1_Treg.wsp")
winXML<-file.path(path,winXML)
############################################################################### 
#cpp parser
###############################################################################
ws<-openWorkspace(winXML[1])

G<-parseWorkspace(ws,groupID=1,execute=F,dMode=2,requiregates=F)
G

length(G)
getSamples(G)
getPopStats(G,flowJo=T)


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


plot(G[[1]])


pData(G)<-data.frame(sample=getSamples(G))
G_list<-flowWorkspace:::splitGatingSetByNgates(G)

lapply(G_list,getPopStats,flowJo=T)



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
