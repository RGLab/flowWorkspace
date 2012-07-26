#unloadNamespace("flowWorkspace")
#library(ncdfFlow)
library(flowWorkspace)

#library(Rgraphviz)

#dyn.load("~/R/r-devel/Rbuild/library/flowWorkspace/libs/flowWorkspace.so")

#lapply(list.files("~/rglab/workspace/flowWorkspace/R",full=T,pattern="*.R$"),source)
#source("~/rglab/workspace/flowWorkspace/R/AllGenerics.R")
#source("~/rglab/workspace/flowWorkspace/R/AllMethods.R")
#source("~/rglab/workspace/flowWorkspace/R/InternalClasses.R")
#source("~/rglab/workspace/flowWorkspace/R/GatingHierarchyInternal-Methods.R")
#source("~/rglab/workspace/flowWorkspace/R/GatingSetInternal-Methods.R")
#source("~/rglab/workspace/flowWorkspace/R/bitVector.R")


macXML<-"/loc/no-backup/HVTN054/Workspace/054-wkspace_tmp_tr.xml"
#macXML<-"/loc/no-backup/HVTN054/FACSData/L02-060731-054-R1/L02-060731-054-R1.xml"
#path<-"~/rglab/workspace/flowWorkspace/data"
#
#macXML<-"HIPC_trial.xml"
#macXML<-file.path(path,"HIPC_trial/data",macXML)
#
#macXML<-c(macXML,"/loc/no-backup/mike/ITN029ST/QA_MFI_RBC_bounary_eventsV3.xml")
#
#winXML<-c("Blomberg/data/Exp2_Tcell.wsp")
#winXML<-file.path(path,winXML)
############################################################################### 
#cpp parser
###############################################################################
ws<-openWorkspace(macXML[1])

#subsetID<-flowWorkspace::getFJWSubsetIndices(ws,key="$FIL"
#											,value=c("01107122_F11_I003.fcs"
#													,"01177007_F02_I016.fcs")
#											,group=2)

#for(i in 2:6)
time1<-Sys.time()	
#Rprof()
#time_cpp<<-0
#G<-parseWorkspace(ws,name=2,execute=T,requiregates=F
#					,subset=1:2
#					,isNcdf=F
#					,useInternal=T,dMode=0)

############################################################################### 
##parse as template and apply to new data			
###############################################################################
G<-parseWorkspace(ws,name=27
					,execute=F
					,includeGates=T
					,subset=c(1)
					,isNcdf=F
					,useInternal=T
					,path
					,dMode=0
					)

newSamples<-getSamples(GT)
#datapath<-"/loc/no-backup/mike/ITN029ST/"
templateSample<-newSamples[1]
newSamples<-newSamples[-1]
gh_template<-GT[[templateSample]]

getPopStats(gh_template)[,2:3]

G<-GatingSet(gh_template
				,newSamples
#				,path=datapath
				,isNcdf=FALSE,dMode=0)

getPopStats(G[[1]])[,2:3]

for(curSample in newSamples)
{
#	browser()
	fj<-getPopStats(GT[[curSample]])[,2,F]
	fc<-getPopStats(G[[curSample]])[,3,drop=F]
	toDisplay<-as.matrix(cbind(fj,fc))
	toDisplay
	rownames(toDisplay)<-basename(rownames(toDisplay))
	barplot(t(toDisplay),beside=T,las=2,horiz=T,cex.names=0.8,main=curSample)
	browser()
	dev.off()
}
nodelist<-getNodes(G[[curSample]])
nodelist
plotGate(G[[curSample]],2,smooth=F,xbin=128,margin=T)
plotGate(G[[curSample]],13,smooth=F,xbin=40,margin=F)

getBoundaries(GT[[curSample]],nodelist[2])
getBoundaries(gh_template,nodelist[2])

head(getPopStats(G))
head(getPopStats(GT,flowJo=T))

ncFlowSet(G)
#Rprof(NULL)	
#summaryRprof()$by.total[1:20,]

#print(time_cpp)
Sys.time()-time1
G
##############################################
#performance report
##############################################
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

#############
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
getPopStats(G[[2]])[,2:3]

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

####comp and trans
cal<-getTransformations(G[[1]])
comp<-getCompensationMatrices(G[[1]])

##plot
for(curSample in getSamples(G[1:2]))
{
	
	gh<-G[[curSample]]
#	browser()
	
#	pdf(file=paste("output/",sampleName,".pdf",sep=""))
	
#	print(plot(gh))
	
	for(i in 7:length(getNodes(gh)))
	{
		
		 print(plotGate(gh,i,smooth=F,xbin=128,margin=F))
		 browser()
		 dev.off()
 	}
 	
#	print(plotPopCV(gh))
#	getNodes(gh)
#	dev.off()
}

manualGate<-function(){
	fr<-read.FCS(file="data/Blomberg/data/Exp2_Sp004_1_Tcell.fcs")
#	save(cal,file="../output/R/cal.rda")
	
#	save(fs,file="output/R/fs_comp.rda")
#	cal<-getTransformations(gh)
	load("output/R/fs1_comp.rda")
	
	data<-fs1[[1]]
#	data<-getData(gh)
	sampleName<-"Exp2_Sp004_1_Tcell.fcs"
	gh<-G[[1]]
	nrow(data)
	getPopStats(gh)[,2:3,drop=F]
	names(cal)
	
	f<-cal[[4]]
	
	g<-getGate(gh,2)
	g@boundaries
	data<-Subset(data,g)
	xyplot(`Comp-FITC-A`~`FSC-A`,data=data
			,filter=g
			,smooth=T
			,xbin=128
	)
	
	##gating through singlets and lymph since they don't require transform
	g<-getGate(gh,3)
	
	xyplot(`FSC-W`~`FSC-A`,data=getData(gh,2)
			,filter=g
			,smooth=F
			,xbin=128
	)
	data<-Subset(data,g)
	
	g<-getGate(gh,4)
	data<-Subset(data,g)
	xyplot(`SSC-A`~`FSC-A`,data=getData(gh,3)
			,filter=g
			,smooth=F
			,xbin=128
	)
	#check cd3+ gate
	g<-getGate(gh,5)
	curChannel<-"Comp-Pacific Blue-A"
	curChannel<-"Comp-FITC-A"
	#transform the channel
	tmp<-exprs(data)[,curChannel]
	tmp<-f(tmp)
	exprs(data)[,curChannel]<-tmp
	
	
	#transform the range slot
	ind<-match(curChannel,colnames(data))
	tmp<-pData(parameters(data))[ind,4:5]
	tmp<-f(tmp)
	pData(parameters(data))[ind,4:5]<-tmp
	
	#transform the gate
	
	tmp<-g@boundaries[,curChannel]
	tmp<-f(tmp)
	g@boundaries[,curChannel]<-tmp
	
	xyplot(`Comp-FITC-A`~`Comp-Pacific Blue-A`,data=getData(gh,4)
			,filter=g
			,smooth=F
			,xbin=128
	)
	
	#check cd4+ gate
	g<-getGate(gh,6)
#	curChannel<-"Comp-PerCP-Cy5-5-A"
	curChannel<-"Comp-APC-Cy7-A"
	length(which(filter(data,g)@subSet))
	xyplot(`Comp-APC-Cy7-A`~`Comp-PerCP-Cy5-5-A`,data=getData(gh,5)
			,filter=g
			,smooth=F
			,xbin=128
	)
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
#for(i in 2:3)

G1<-parseWorkspace(ws,name=2,execute=T,requiregates=F
					,isNcdf=T
					,subset=1:100
					,useInternal=F,dMode=0)
print(Sys.time()-time1)
d

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
hist(ff(uv$v))
plot(ff(uv$v))
ff<-splinefun(y=compCal,x=0:4096,method="natural")

biexp  <- biexponentialTransform("myTransform")
vv2<-biexp(as.numeric(0:256))

vv3<-exprs(GvHD[[1]])[,3]
plot(vv3)

(biexp(vv3))
setwd("/home/wjiang2/rglab/workspace/flowWorkspace/")

compCal<-scan("output/cpp/compCalTbl.txt")
head(compCal)


hist(vv)
biexponentialTransform( a=.5, b=1, c=.5, d=1, f=0, w=0,
		tol=.Machine$double.eps^0.25, maxit=as.integer(5000))