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
				,pointer="externalptr"))

setMethod("initialize","GatingHierarchyInternal"
		,function(.Object,name="New Sample",flag=FALSE
				,transformations=rep(list({f<-function(x){x};attr(f,"type")<-"identity";f}),8)
				,compensation=diag(8),dataPath="."
				,isNcdf=FALSE
				,fcsfile=NULL
				,pointer=NULL){
#			callNextMethod(.Object,tree,nodes,name,flag,transformations,compensation,dataPath,isNcdf)
			.Object@pointer=pointer
			.Object@name<-name
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
setMethod("initialize","GatingSetInternal"
		,function(.Object
#				,set=list(new("GatingHierarchyInternal"))
#				,metadata=new("AnnotatedDataFrame")
#				,files=NULL
#				,isNcdf=FALSE
#				,flowSetId=NULL
				,xmlFileName=NULL
				,sampleIDs
				,execute=FALSE
				,dMode=1
				){

#			.Object@set<-set;
#			.Object@metadata<-metadata
#			browser()			
			stopifnot(!is.null(xmlFileName))
			if(!file.exists(xmlFileName))
				stop(xmlFileName," not found!")
			.Object@pointer<-.Call("R_parseWorkspace",xmlFileName,sampleIDs,execute,as.integer(dMode))
#	validObject(.Object)
			return(.Object)
		})
