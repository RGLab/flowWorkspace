# TODO: Add comment
# 
# Author: wjiang2
###############################################################################


setClass("GatingHierarchyInternal",contains="GatingHierarchy"
		,representation(tree="graphNEL"
				,nodes="character"
				,name="character"
				,flag="logical"
				,transformations="list"
				,compensation="matrix"
				,dataPath="character"
				,isNcdf="logical"
				,pointer="externalptr"
#				,dataEnv="environment"
				)
		)

setMethod("initialize","GatingHierarchyInternal"
		,function(.Object,name="New Sample",flag=FALSE
				,transformations=rep(list({f<-function(x){x};attr(f,"type")<-"identity";f}),8)
				,compensation=diag(8),dataPath="."
				,isNcdf=FALSE
				,fcsfile=NULL
				,pointer=NULL){
			.Object<-callNextMethod(.Object,name=name,flag=flag,transformations=transformations
			,compensation=compensation,dataPath=dataPath,isNcdf=isNcdf)
			
			.Object@pointer=pointer
			return(.Object)
		})

setClass("GatingSetInternal",contains="GatingSet"
		,representation(set="list"
				,metadata="AnnotatedDataFrame"
				,pointer="externalptr")
		,validity=function(object){
			all(unlist(lapply(object@set
									,function(y)inherits(y,"GatingHierarchy"))))
		})
###########################
#constructors for GatingSet
##########################
setGeneric("GatingSet",function(x,y,...)standardGeneric("GatingSet"))
#construct object from xml workspace file and a list of sampleIDs
setMethod("GatingSet",c("character","character"),function(x,y,includeGates=FALSE,dMode=1,sampNloc="keyword",...){
			
			xmlFileName<-x
			sampleIDs<-y
#			browser()
			sampNloc<-match(sampNloc,c("keyword","sampleNode"))
			if(is.na(sampNloc))
				sampNloc<-0
			stopifnot(!missing(xmlFileName))
			
			if(!file.exists(xmlFileName))
				stop(xmlFileName," not found!")
			Object<-new("GatingSetInternal")
			Object@pointer<-.Call("R_parseWorkspace",xmlFileName,sampleIDs,includeGates,as.integer(sampNloc),as.integer(dMode))
			
			return(Object)
})

##this class is mainly for method dispatching
		
setClass("GatingSetList",contains="list")	
## Constructor
GatingSetList <- function(x)
{
	flowCore:::checkClass(x, "list")
	x <- new("GatingSetList", .Data=x)
	return(x)
}
