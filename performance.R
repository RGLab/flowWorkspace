library(ncdfFlow)
library(flowWorkspace)

#library(Rgraphviz)

macXML<-"HIPC_trial.xml"
macXML<-file.path("/loc/no-backup/mike/HIPC/data/HIPC_trial/data",macXML)
macXML<-c(macXML,"/loc/no-backup/mike/ITN029ST/QA_MFI_RBC_bounary_eventsV3.xml")

############################################################################### 
#cpp parser
###############################################################################
ws<-openWorkspace(macXML[1])

nIt<-c(10,50,100,500,1000)
res<-vector(mode="list",length(nIt))
for(i in 1:length(nIt))
{
	m<-3
	subRes<-vector(mode="list",m)
	for(j in 1:m)
	{
		time1<-Sys.time()	
		G<-parseWorkspace(ws,name=1,execute=F,requiregates=F
				,subset=1:nIt[i]
				,isNcdf=F
				,useInternal=T,dMode=0)
				
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

testfun <- cxxfunction(signature(),
		includes='
					class AA{
								public:
								int a;
								AA();
								~AA();
							};
					AA::AA(){a=4;};
					AA::~AA(){std::cout<<"destructor"<<std::endl;};	
					
			'
		,body='
		
		
		

		AA * v=new AA();
		
	    return(Rcpp::XPtr<AA>(v));	
		', plugin="Rcpp")

xp<-testfun()

funx <- cfunction(signature(x = "externalptr" )
				,includes='
						class AA{
						public:
						int a;
						AA();
						~AA();
						};
						AA::AA(){a=2;};
						AA::~AA(){std::cout<<"destructor"<<std::endl;};		
				 '
		 		,body='
				Rcpp::XPtr<AA> p(x) ;
				return( Rcpp::wrap( p->a) ) ;
				', Rcpp=TRUE, verbose=FALSE)
funx(xp)
rm(xp)
gc()

