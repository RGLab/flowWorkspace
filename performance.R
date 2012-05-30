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

nIt<-c(10,50,100,500,1000)
res<-vector(mode="list",length(nIt))
for(i in 1:length(nIt))
{
	
	subRes<-vector(mode="list",3)
	for(j in 1:3)
	{
		time1<-Sys.time()	
		G<-parseWorkspace(ws,name=2,execute=T,requiregates=F
				,subset=1:nIt[i]
				,isNcdf=F
				,useInternal=F,dMode=0)
				
		subRes[[j]]<-Sys.time()-time1
#		file.remove(ncFlowSet(G)@file)
	}
	res[[i]]<-subRes
	
	
	
}
names(res)<-nIt
save(res,file="res_flowset.rda")
cat("done")
#getData(G[[1]])


########################inline Rcpp test
require(inline)
a<-matrix(as.numeric(1:10),ncol=2)
colnames(a)<-c("A","B")

testfun <- cxxfunction(
		signature(x="matrix"),
		body = '
		NumericMatrix mat(x);	
		List dimnames=mat.attr("dimnames");
	
		std::vector<std::string> params=dimnames[1];
		unsigned nEvents=mat.nrow();
	
		unsigned nChannls=params.size();
		unsigned nSize=nChannls*nEvents;
	
		for(unsigned i=0;i<nSize;i++)
				mat[i]=i*10;
		return wrap(mat.size());

		', plugin="Rcpp")

testfun(a)




