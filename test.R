#unloadNamespace("flowWorkspace")
library(ncdfFlow)
library(flowWorkspace)

#library(Rgraphviz)

#dyn.load("/home/wjiang2/R/r-devel/Rbuild/library/flowWorkspace/libs/flowWorkspace.so")

##lapply(list.files("~/rglab/workspace/flowWorkspace/R",full=T,pattern="*.R$"),source)
#source("~/rglab/workspace/flowWorkspace/R/AllGenerics.R")
#source("~/rglab/workspace/flowWorkspace/R/AllMethods.R")
#source("~/rglab/workspace/flowWorkspace/R/InternalClasses.R")
#source("~/rglab/workspace/flowWorkspace/R/GatingHierarchyInternal-Methods.R")
#source("~/rglab/workspace/flowWorkspace/R/GatingSetInternal-Methods.R")
#source("~/rglab/workspace/flowWorkspace/R/bitVector.R")
#
#path<-"/home/wjiang2/rglab/workspace/flowWorkspace/data"

macXML<-"HIPC_trial.xml"
macXML<-file.path("/loc/no-backup/mike/HIPC/data/HIPC_trial/data",macXML)

winXML<-c("Yale/LyoplateTest1Yale.wsp")
winXML<-file.path(path,winXML)
############################################################################### 
#cpp parser
###############################################################################
ws<-openWorkspace(macXML[1])


for(i in 2:6)
#time1<-Sys.time()	
#Rprof()
time_sum<<-0
G<-parseWorkspace(ws,name=2,execute=T,requiregates=F
					,subset=c(1,2)
					,isNcdf=T
					,useInternal=T,dMode=0)
#Rprof(NULL)	
#summaryRprof()
#Sys.time()-time1
print(time_sum)
G

#performance report
pdf(file="../output/performance.pdf")
xx<-c(2,6,12,30)
cpp<-c(4.7,16,23,32)
rp<-c(19,46,97,182)
plot(xx,rp,type="o",xlab="samples",ylab="time(s)",ylim=c(0,200))
points(xx,cpp,type="o",col="red")
legend(legend=c("C++","R"),x="top",col=c("red","black"),bty="n",lty=1)

#without read.fcs
cpp<-c(0.41,0.99,1.97,32)
rp<-c(19,46,97,182)
plot(xx,rp,type="o",xlab="samples",ylab="time(s)",ylim=c(0,200))
points(xx,cpp,type="o",col="red")
legend(legend=c("C++","R"),x="top",col=c("red","black"),bty="n",lty=1)


sec<-c(0.3,0.45,0.35)
names(sec)<-c("parsing","compensating","transforming&gating")
pie(sec)

sec1<-c(0.42,6.11-0.42)
names(sec1)<-c("c++","R")
pie(sec1)
dev.off()
##accessors
length(G)
getSamples(G)


gh<-G[[1]]
gh

getSample(gh)
nodelist<-getNodes(gh)
nodelist
getNodes(gh,tsort=T)
getNodes(gh,isPath=T)
getParent(gh,3)
getParent(gh,nodelist[2])


getChildren(gh,nodelist[2])
getChildren(gh,nodelist[4])

#stats
getProp(G[[1]],nodelist[2],flowJo=T)
getTotal(G[[2]],nodelist[1],flowJo=F)
getTotal(G[[1]],nodelist[2],flowJo=F)

.getPopStat(G[[1]],2)
getPopStats(G[[1]])[,2:3]

##split into groups

pData(G)<-data.frame(sample=getSamples(G))
G_list<-flowWorkspace:::splitGatingSetByNgates(G)

lapply(G_list,getPopStats,flowJo=T)
getPopStats(G,flowJo=F)


###gates
getGate(G[[2]],getNodes(G[[2]])[8])
getGate(G1[[2]],getNodes(G1[[2]])[8])

getIndices(G[[1]],nodelist[3])
getDimensions(G[[1]],nodelist[2])

getBoundaries(G[[2]],getNodes(G[[2]])[8])
getBoundaries(G1[[2]],getNodes(G1[[2]])[8])

getData(G[[1]])


##plot
for(sampleName in getSamples(G[1:2]))
{
	
	gh<-G[[sampleName]]
	
	pdf(file=paste("../output/",sampleName,".pdf",sep=""))
	
	print(plot(gh))
	
	for(i in 2:length(getNodes(gh)))
	 print(plotGate(gh,i))
 
	
	print(plotPopCV(gh))
	
	dev.off()
}


getAxisLabels(G[[1]])
getAxisLabels(G1[[1]])
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
for(i in 2:3)

G1<-parseWorkspace(ws,name=i,execute=T,requiregates=F
					,isNcdf=T
#					,subset=c(1,2)
					,useInternal=F,dMode=0)
Sys.time()-time1
G1

##generate file for c++ debugging
nc1<-ncFlowSet(G1)
nc1[[1]]
#nc2<-clone.ncdfFlowSet(nc1,fileName="output/test.nc",isEmpty=FALSE)
#nc2[[1]]
write(file="../output/colnames.txt",colnames(nc1))

length(G1)
gh1<-G1[[2]]
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
getData(G1[[1]],5)

getGate(G1[[2]],4)
getGate(G[[1]],4)
plotGate(G1[[1]],5)

##############
#verify stats
#############
all(sort(rownames(getPopStats(gh)))==sort(rownames(getPopStats(gh1))))
all(getPopStats(gh)[sort(rownames(getPopStats(gh))),c("flowJo.count","parent.total")]==getPopStats(gh1)[sort(rownames(getPopStats(gh1))),c("flowJo.count","parent.total")])


vv1<-scan("/home/wjiang2/rglab/workspace/logicle/out.txt")
plot(vv)
plot(vv1)
biexp  <- biexponentialTransform("myTransform")
vv2<-biexp(as.numeric(0:256))

vv3<-exprs(GvHD[[1]])[,3]
plot(vv3)

(biexp(vv3))

hist(vv)
biexponentialTransform( a=.5, b=1, c=.5, d=1, f=0, w=0,
		tol=.Machine$double.eps^0.25, maxit=as.integer(5000))