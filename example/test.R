


library(flowWorkspace)
library(XML)
library(devtools)
load_all("~/rglab/workspace/flowWorkspace/")

macXML<-"~/rglab/workspace/flowWorkspace/data/RV144/Batch 1264 RV144.xml"
macXML<-"~/rglab/workspace/flowWorkspace/data/Cytotrol/NHLBI/flowJo/NHLBI.xml"
macXML<-"/shared/silo_researcher/Gottardo_R/gfinak_working/Phenotyping/FACS Analysis/001-Y-Pheno-JK.xml"

macXML<-"/loc/no-backup/FlowWorkspaceTest/Lyoplate/Centralized B-cell.xml"
macXML<-list.files(pattern="xml",path="~/rglab/workspace/flowWorkspace/data/Newell/XML workspaces",full=TRUE)[1]

macXML<-"/loc/no-backup/ramey/Cytotrol/XML/CA_CytoTrol Treg.xml"

#macXML<-"/loc/no-backup/HVTN054/Workspace/054-wkspace_tmp_tr.xml"
#macXML<-"/loc/no-backup/HVTN054/FACSData/L02-060731-054-R1/L02-060731-054-R1.xml"
macXML<-"/loc/no-backup/remote_fred_hvtn/HVTN080/XML files/080 Batch 1057 M.xml"
#path<-"~/rglab/workspace/flowWorkspace/data"
#
#macXML<-"HIPC_trial.xml"
#macXML<-file.path(path,"HIPC_trial/data",macXML)

#
#macXML<-c(macXML,"/loc/no-backup/mike/ITN029ST/QA_MFI_RBC_bounary_eventsV3.xml")
macXML<-"/shared/silo_researcher/Gottardo_R/mike_working/ITN029ST/QA_template.xml"
#
macXML<-c("/home/wjiang2/rglab/workspace/flowWorkspace/data/PBMC/Blomberg/Exp2_Tcell.wsp")

macXML<-"/home/wjiang2/rglab/workspace/flowWorkspace/data/vX/Lesson_8_vX.wsp"
macXML<-"/home/wjiang2/rglab/workspace/flowWorkspace/fjWsExamples/20131206_Treg.wsp"
#winXML<-file.path(path,winXML)
############################################################################### 
#cpp parser
###############################################################################
ws <- openWorkspace(macXML[1],options=1)
getSamples(ws)
getSampleGroups(ws)
getKeywords(ws,"CytoTrol_CytoTrol_1.fcs")


#modify functions within package namespace
funcToinsert <- ".plotGate" 
funcSym <- as.symbol(funcToinsert)
eval(substitute(environment(ff) <- getNamespace("flowWorkspace"), list(ff = funcSym)))
assignInNamespace(funcToinsert, eval(funcSym), ns = "flowWorkspace")


############################################################################### 
##parse as template and apply to new data			
###############################################################################
time1<-Sys.time()	
#Rprof()
GT<-parseWorkspace(ws
                    ,name=1
					,execute=T
#					,includeGates=T
#                    ,subset= 1:2
#					,subset=c("517614.fcs")
#					,isNcdf=T
#                      ,path = "/shared/silo_researcher/Gottardo_R/gfinak_working/Phenotyping/FACS Analysis/001-Y-Pheno-JK/"
#                    ,path="/home/wjiang2/rglab/workspace/flowWorkspace/data/vX/"
                    ,path="/loc/no-backup/remote_fred_hvtn/HVTN080/FACS Data/1057-M-080/"
#                    ,path="~/rglab/workspace/flowWorkspace/data/RV144/1264-L-RV144"
#                    ,path="~/rglab/workspace/flowWorkspace/data/Newell"
#                      ,path="/shared/silo_researcher/Gottardo_R/mike_working/ITN029ST"
#                    ,path="~/rglab/workspace/flowWorkspace/data/Cytotrol/NHLBI/Bcell/"
#					,path="~/rglab/workspace/flowWorkspace/data/Cytotrol/NHLBI/Tcell/"
#                    ,path = "/loc/no-backup/ramey/Cytotrol/Treg FCS files/Treg BIIR/"
#                      , path = "/home/wjiang2/rglab/workspace/flowWorkspace/data/PBMC/Blomberg/data/"
					,dMode=4
                    ,extend_val=0
#                    ,column.pattern=colP
#                     ,prefix=F
					)
Sys.time()-time1                    
Rprof(NULL)
summaryRprof()
getSamples(ws)
getKeywords(GT,"CytoTrol_CytoTrol_1.fcs")
getKeywords(GT[[1]])
keyword(GT[[1]])
keyword(GT[[1]],"FILENAME")


klist <- keyword(GT)
klist <- keyword(GT,"FILENAME")
gslist <- GatingSetList(list(GT))
klist <- keyword(gslist)
keyword(gslist,"FILENAME")
pData(gslist)
gslist[1]
getGate(gslist,2)

gh <- GT[[1]]
getNodes(gh)
g <- getGate(GT[[1]],10)
add(GT,g,parent="4+",name="test")
plotGate(GT[[1]],"4+",bool=T)

length(which(getIndices(GT[[1]],"Excl")))
getGate(GT[[1]],"Excl")@boundaries
recompute(GT,"Excl/4+")
recompute(GT,"4+")

getPopStats(GT[[1]])[1:20,c(2:3), with = F]
str(getGate(GT[[1]],2))
getData(GT)[[1]]
flowData(GT)[[1]]
dev.off()
x11()
#Rprof()
#gh <- GT[1]
plotGate(GT[[1]],xbin=32, margin =T)
#plotGate(GT[[1]],3, xbin=32, margin =T)
#xyplot(`FSC-H`~`FSC-A`
#      ,getData(GT)[1]
#      ,getGate(GT,3)[1]
#      ,stats=T,smooth=F,xbin=32
#      ,defaultCond= NULL
#      )
#Rprof(NULL)
summaryRprof()


plot(GT[[1]])
getData(gg[[1]])
getData(gg)
#save to folder without tarring
tmp2<-file.path(tmpdir,"t1")
save_gs(GT,path=tmp2
        , overwrite =T
        )          
gg <- load_gs(path=tmp2)
list.files(tmp2)


gh<-GT[[1]]
getParent(gh,5)
getParent(gh,"3+")
getChildren(gh,5)
getChildren(gh,"3+")
getProp(gh,"3+")
getTotal(gh,"3+")
getGate(gh,"3+")
length(which(getIndices(gh,"3+")))
x11()
plotGate(gh,"3+")

getData(GT)
getSamples(GT)
pData(GT[c("CytoTrol_CytoTrol_1.fcs","CytoTrol_CytoTrol_2.fcs")])
gg <- .getGraph(GT[[1]])
plot(GT[[1]])
plotGate(GT[1],"CD3+"
        ,smooth=T
#          ,xlab="test",ylab="testy"
        )
plotGate(GT[1],"IL2+",smooth=T)
plotGate(GT[1],"8+/IL2+",smooth=T)
getNodes(GT[[1]])
Rm("Excl/4+",GT)
getGate(GT,"4+/TNFa+")
getNodes(GT[[1]],isPath=T)
getProp(GT[[1]],"4+/IL2+")
plotGate_labkey(GT,2,smooth=T,x="FSC-A",y="FSC-H"
          ,xlab="test",ylab="testy"
)        
plotGate(GT[[1]],2:5,smooth=T)	
getPopStats(GT[[1]])[1:10,c(1,4)]
getTransformations(GT[[1]])
getGate(GT[[1]],3)@boundaries
hist(exprs(getData(GT[[1]]))[,8])

##serialzation
save_gs(GT,path="/home/wjiang2/rglab/workspace/flowWorkspace/output/NHLBI/gs",overwrite=T)
G<- load_gs("~/rglab/workspace/flowWorkspace/output/NHLBI/gs")
tt<-logTransform(transformationId="log10-transformation", logbase=10, r=1, d=1)
fr_trans<-transform(fr,`Am Cyan-A`=tt(`Am Cyan-A`))
apply(exprs(fr_trans),2,range)

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



##plot
getNodes(G[[1]])
png(file="plotGate.png",width=800,height=800)
plotGate(GT
		,4:5
		,par.settings=list(par.main.text=list(cex=0.8)
									,par.xlab.text=list(cex=0.5)
									,par.ylab.text=list(cex=0.5))
		,stat=T
		,xbin=64
		)
getGate(G[[1]],4)		
getParent(G[[1]],8)
plotGate_labkey(GT,7,x="<R660-A>" ,y="<G560-A>")		
plotGate_labkey(GT,7,x="SSC-A" ,y="<G560-A>")
dev.off()
CairoX11()
plotGate(GT[[1]],6,xbin=64)
plotGate(GT[[1]],2:4,xbins=128)


nodelist<-getNodes(G[[curSample]])
nodelist
plotGate(G[[curSample]],smooth=F,xbin=128,margin=T)
plotGate(G[[curSample]],13,smooth=F,xbin=40,margin=F)

getBoundaries(GT[[curSample]],nodelist[2])
getBoundaries(gh_template,nodelist[2])

head(getPopStats(G))
head(getPopStats(GT,flowJo=T))

flowData(GT)
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

length(GT)
getSamples(GT)


gh<-GT[[1]]
gh

getSample(gh)
nodelist<-getNodes(gh)
nodelist
getNodes(gh)
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


###gates
getGate(GT[[2]],getNodes(GT[[2]])[8])


getIndices(GT[[1]],nodelist[3])

getData(GT[[1]])

####comp and trans
getTransformations(GT[[1]])
getCompensationMatrices(GT[[1]])


###############################
# clone and subsetting and rbind
################################

G1<-clone(GT[1])
G2<-clone(GT[2])
gslist<-GatingSetList(list(G1,G2))
G<-rbind2(gslist)
flowData(G)
plotGate(G,7,lattice=T,xbin=64)
G5<-clone(G)
flowData(G5)
plotGate(G5,7,lattice=T,xbin=64)
#############################################
#QUALIFIER
########################################
library(QUALIFIER)
db<-new.env()
saveToDB(db,G)
getQAStats(db,isFlowCore=F)
dt <- getQAStats(GT[[1]],isChannel=T)
dt[10:30,]

