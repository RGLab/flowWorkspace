#unloadNamespace("flowWorkspace")
library(flowWorkspace)
library(Rgraphviz)
library(ncdfFlow)
dyn.load("/home/wjiang2/R/r-devel/Rbuild/library/flowWorkspace/libs/flowWorkspace.so")

#source("~/rglab/workspace/flowWorkspace/R/InternalClasses.R")
#source("~/rglab/workspace/flowWorkspace/R/GatingHierarchyInternal-Methods.R")
#source("~/rglab/workspace/flowWorkspace/R/GatingSetInternal-Methods.R")

path<-"/home/wjiang2/rglab/workspace/flowWorkspace/fjWsExamples"

macXML<-"HIPC_trial.xml"
macXML<-file.path("/home/wjiang2/rglab/workspace/HIPC-Lyoplate/data",macXML)

winXML<-c("LyoplateTest1Yale.wsp","Exp1_Treg.wsp")
winXML<-file.path(path,winXML)
############################################################################### 
#cpp parser
###############################################################################
ws<-openWorkspace(macXML[1])
time1<-Sys.time()
G<-parseWorkspace(ws,name=1,execute=T,requiregates=F
					,subset=c(1:2)
					,isNcdf=T
					,useInternal=T,dMode=2)
Sys.time()-time1
G

length(G)
getSamples(G)
getPopStats(G,flowJo=T)


gh<-G[[1]]
gh

getSample(gh)
getNodes(gh)
getNodes(gh,tsort=T)
getNodes(gh,isPath=T)
getParent(gh,3)
getParent(gh,"2.Lymphocytes")


getChildren(gh,"2.Lymphocytes")
getChildren(gh,"4.CD4 subset")

getProp(gh,"2.Lymphocytes",flowJo=T)
getTotal(gh,"2.Lymphocytes",flowJo=T)

flowWorkspace:::.getPopStat(gh,2)
head(getPopStats(gh))


plot(G[[1]])


pData(G)<-data.frame(sample=getSamples(G))
G_list<-flowWorkspace:::splitGatingSetByNgates(G)

lapply(G_list,getPopStats,flowJo=T)

###gating
getGate(G[[1]],2)
getIndices(G[[1]],getNodes(gh)[3])
getData(G[[1]],2)
plotGate(G[[1]],2)



#############################################
#QUALIFIER
########################################
db<-new.env()
saveToDB(db,G)
getQAStats(db,isFlowCore=F)


############################################################################### 
#R parser
###############################################################################

ws<-openWorkspace(macXML)
time1<-Sys.time()
G1<-parseWorkspace(ws,name=1,execute=T,requiregates=F
#					,isNcdf=T
					,subset=c(1:2)
					,useInternal=F,dMode=2)
Sys.time()-time1
G1

##generate file for c++ debugging
nc1<-ncFlowSet(G1)
nc1[[1]]
nc2<-clone.ncdfFlowSet(nc1,fileName="output/test.nc",isEmpty=FALSE)
nc2[[1]]
write(file="output/colnames.txt",colnames(nc1))

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

###gating
getData(G1[[1]],2)
str(getGate(G1[[1]],2))
plotGate(G1[[1]],5)

##############
#verify stats
#############
all(sort(rownames(getPopStats(gh)))==sort(rownames(getPopStats(gh1))))
all(getPopStats(gh)[sort(rownames(getPopStats(gh))),c("flowJo.count","parent.total")]==getPopStats(gh1)[sort(rownames(getPopStats(gh1))),c("flowJo.count","parent.total")])

