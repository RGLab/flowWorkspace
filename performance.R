library(ncdfFlow)
library(flowWorkspace)

#library(Rgraphviz)

macXML<-"HIPC_trial.xml"
macXML<-file.path("/loc/no-backup/mike/HIPC/data/HIPC_trial/data",macXML)
macXML<-c(macXML,"/loc/no-backup/mike/ITN029ST/QA_MFI_RBC_bounary_eventsV3.xml")

############################################################################### 
#cpp parser
###############################################################################
ws<-openWorkspace(macXML[2])

res<-vector(mode="list",5)
for(i in c(10,50,100,500,1000))
{
	subRes<-vector(mode="list",3)
	for(j in 1:3)
	{
		time1<-Sys.time()	
		G<-parseWorkspace(ws,name=2,execute=T,requiregates=F
				,subset=1:i
				,isNcdf=T
				,useInternal=T,dMode=0)
				
		subRes[[j]]<-Sys.time()-time1
	}
	res[[i]]<-subRes
}
cat("done")

plotGate(G[[1]],2)