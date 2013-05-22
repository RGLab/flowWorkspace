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
				,pointer="externalptr"
                ,guid = "character")
		,validity=function(object){
			all(unlist(lapply(object@set
									,function(y)inherits(y,"GatingHierarchy"))))
		})
.uuid_gen<-function(){
  system("uuidgen",intern = TRUE)
}    
###########################
#constructors for GatingSet
##########################
setGeneric("GatingSet",function(x,y,...)standardGeneric("GatingSet"))
#construct object from xml workspace file and a list of sampleIDs
setMethod("GatingSet",c("character","character"),function(x,y,includeGates=FALSE,dMode=1,sampNloc="keyword",xmlParserOption, ...){
			
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
			Object@pointer<-.Call("R_parseWorkspace",xmlFileName,sampleIDs,includeGates,as.integer(sampNloc),as.integer(xmlParserOption), as.integer(dMode))
            Object@guid <- .uuid_gen()
            
			return(Object)
})


#construct a gatingset with empty trees (just root node) 
setMethod("GatingSet",c("flowSet"),function(x,dMode=0,...){
      
      fs_clone<-flowCore:::copyFlowSet(x)
      samples<-sampleNames(fs_clone)
      G<-new("GatingSetInternal")
      G@pointer<-.Call("R_NewGatingSet_rootOnly",samples,dMode=as.integer(dMode))
      G@guid <- .uuid_gen()
      
      globalDataEnv<-new.env(parent=emptyenv())
      
      assign("ncfs",fs_clone,globalDataEnv)
#           nFiles<-length(samples)
#           set<-vector(mode="list",nFiles) 
#
#           for(i in 1:nFiles)
#           {
#               file<-files[i]      
#               sampleName<-samples[i]
#               gh<-new("GatingHierarchyInternal",pointer=G@pointer,name=sampleName)
      ##          browser()
#               localDataEnv<-nodeDataDefaults(gh@tree,"data")
#               localDataEnv$data<-globalDataEnv
#               
#               gh@flag<-FALSE #set gate flag
#               set[[i]]<-gh
#           }
#           names(set)<-samples
#           G@set<-set
      
      G@set<-sapply(samples,function(sampleName){
            gh<-new("GatingHierarchyInternal",pointer=G@pointer,name=sampleName)
            localDataEnv<-nodeDataDefaults(gh@tree,"data")
            localDataEnv$data<-globalDataEnv
            gh@flag<-TRUE #set gate flag
            gh
          })
      recompute(G)
      G
      
    })