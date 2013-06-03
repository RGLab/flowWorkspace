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
    
    
setClass("GatingSetList"
    ,representation=representation(
        data = "list"
        ,samples="character" #this determine the order of samples exposed to user
    ))

validGatingSetListObject <- function(object){
  
  gs_list <- object@data
  #check overlapping samples
  gs_samples <- unlist(lapply(gs_list,getSamples))
  if(any(duplicated(gs_samples))){
    return ("There are overlapping samples across GatingSets!")
  }
  
  
  gs1 <- gs_list[[1]]
  
  #compare GatingSets
  
  res <- sapply(gs_list[-1],function(this_gs){
        
        .compareGatingSet(this_gs,gs1)
      })
  
  
  is_error <- sapply(res,function(this_res){
        is.na(as.logical(this_res))
      })
  if(any(is_error)){
    this_error_ind <- which(is_error)[1]
    return (paste("GatingSet 1 and",this_error_ind+1,":",res[this_error_ind]))
  }
  #check sample vector
  if(!.isValidSamples(object@samples,gs_list)){
    return ("'samples' slot is not consisitent with sample names from GatingSets!")
  }          
  return (TRUE)
}

setValidity("GatingSetList", validGatingSetListObject)     


.flattenedGatingHiearchy<-function(gh){
  this_nodes <- getNodes(gh,isPath=T)
  paste(this_nodes,collapse = "")
}        
#TODO:gating tree comparison needs to be improved        
.compareGatingHierarchy<-function(gh1,gh2){
  if(identical(.flattenedGatingHiearchy(gh1),.flattenedGatingHiearchy(gh2))){
    return (TRUE)
  }else{
    return (paste("gating structure doesn't match:",getSample(gh1),getSample(gh2)))
  }
}
.compareFlowData<-function(fs1,fs2){
  #it is strange that colnames doesn't dispatch properly without namespace prefix
  col1 <- flowCore::colnames(fs1)
  col2 <- flowCore::colnames(fs2)
  if(!identical(col1,col2)){
#    sCol1 <- paste(col1,collapse=" ")
#    sCol2 <- paste(col2,collapse=" ")
    msg <- paste("colnames of flowSets don't match!")
    return (msg)
  }
  if(!identical(colnames(pData(fs1)),colnames(pData(fs2)))){
    return ("pData of flow data doesn't match!")
  }
  return  (TRUE)
  
}
.compareGatingSet<-function(gs1,gs2){
  gh1 <- gs1[[1]]
  gh2 <- gs2[[1]]
  res <- .compareGatingHierarchy(gh1,gh2)
  if(class(res) == "character"){
    return (res)
  }
  fs1 <- getData(gs1)
  fs2 <- getData(gs2)
  .compareFlowData(fs1,fs2)
}
#validity check for samples slot        
.isValidSamples<-function(samples,object){
  
  return (setequal(unlist(lapply(object,getSamples)),samples))
}

## Constructor
GatingSetList <- function(x,samples = NULL)
{
  names(x)<-NULL#strip names from the list because rbind2 doesn't like it
  flowCore:::checkClass(x, "list")
  if(is.null(samples)){
    samples <- unlist(lapply(x,getSamples))
  }
  x <- new("GatingSetList", data = x, samples = samples)
  return(x)
}