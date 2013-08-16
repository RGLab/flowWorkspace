#' @include AllGenerics.R

#All class definitions
setOldClass("XMLInternalDocument")
setClass("flowJoWorkspace",representation(version="character",file="character",.cache="environment",path="character",doc="XMLInternalDocument",options="integer"))

setClass("GatingHierarchy"
        ,representation(tree="graphNEL"
                        ,nodes="character"
                        ,name="character"
                        ,flag="logical"
                        ,transformations="list"
                        ,compensation="matrix"
                        ,dataPath="character"
                        ,isNcdf="logical"
                        ,pointer="externalptr")
                )
                

                
setMethod("initialize","GatingHierarchy"
          ,function(.Object
                      ,tree=new("graphNEL",edgemode="directed"),nodes=character()
                      ,name="New Sample"
                      ,flag=FALSE
                      ,transformations=rep(list({f<-function(x){x};attr(f,"type")<-"identity";f}),8)
                      ,compensation=diag(8)
                      ,dataPath="."
                      ,isNcdf=FALSE
                      ,fcsfile=NULL
                      ,pointer=NULL){
	#callNextMethod(.Object,tree,nodes,name,flag,transformations,compensation,dataPath,isNcdf)
	m<-new.env(parent=emptyenv())
	
	multiassign(c("compID","fcsfile","fjName","gate","negated","isBooleanGate","thisIndices","parentTot","thisTot","isGated","keywords"),list(NULL,fcsfile,fcsfile,NA,FALSE,FALSE,list(),NA,0,FALSE,NULL),env=m)
	nodeDataDefaults(tree,"metadata")<-m
	d<-new.env(parent=emptyenv());
	data<-new.env(parent=emptyenv());
	assign("data",data,env=d);
	if(!isNcdf){
		assign("data",new("flowSet"),env=data)
	}else{
		assign("ncfs",new("ncdfFlowSet"),env=data);
	}
	nodeDataDefaults(tree,"data")<-d
	.Object@nodes<-nodes
	.Object@tree<-tree
	.Object@name<-name
	if (!is.null(fcsfile)&name=="New Sample"){
		.Object@name<-fcsfile
	}
	.Object@flag<-flag
	.Object@transformations<-transformations
	.Object@compensation<-compensation
	.Object@dataPath<-dataPath
	.Object@isNcdf<-isNcdf
    .Object@pointer <- pointer
	validObject(.Object)
	return(.Object)
})


.uuid_gen<-function(){
#  system("uuidgen",intern = TRUE)
  flowCore:::guid()
}    

setClass("GatingSet"
          ,representation(set="list"
                          ,metadata="AnnotatedDataFrame"
                          ,pointer="externalptr"
                          ,guid = "character"
                          )
          ,validity=function(object){
	all(unlist(lapply(object@set,function(y)inherits(y,"GatingHierarchy"))))
})

setMethod("initialize","GatingSet",function(.Object,set=list(new("GatingHierarchy")),metadata=new("AnnotatedDataFrame"),files=NULL,isNcdf=FALSE,flowSetId=NULL){
	if(!is.null(files)){
		if(isNcdf){
			stopifnot(length(grep("ncdfFlow",loadedNamespaces()))!=0)
			fs<-read.ncdfFlowSet(files,flowSetId=ifelse(is.null(flowSetId),"New FlowSet",flowSetId))
		}else{
			fs<-read.flowSet(files)
		}
		set<-vector("list",length(fs))
		for(i in seq_along(fs)){
			set[[i]]<-new("GatingHierarchy",fcsfile=files[i],dataPath=dirname(files[i]),name=basename(files[i]),isNcdf=isNcdf)
			setData(set[[i]],fs)
		}
	}
	.Object@set<-set;
	.Object@metadata<-metadata
	validObject(.Object)
	return(.Object)
})



#' constructors for GatingSet
#' construct object from xml workspace file and a list of sampleIDs
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
#' validity check for samples slot        
.isValidSamples<-function(samples,object){
  
  return (setequal(unlist(lapply(object,getSamples)),samples))
}

#' Constructor
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


#' BooleanGate class
#' 
#' A class describing logical operation (& or |) of the reference populations 
setClass("booleanFilter"
		,contains=c("expressionFilter")
)

#' Constructor: We allow for the following inputs:
#'  expr is always an expression
#'  ... are further arguments to the expression
booleanFilter <- function(expr, ..., filterId="defaultBooleanFilter")
{
	subs <- substitute(expr)
	if(missing(filterId)){
		filterId <- deparse(subs)
		if(length(filterId)>1)
			filterId <- paste(gsub("^ *", "", filterId[2]), "...", sep="")
	}else flowCore:::checkClass(filterId, "character", 1)
	new("booleanFilter", filterId=filterId, expr=as.expression(subs),
			args=list(...), deparse=deparse(subs))
}

#' Constructor from a character string: We allow for the following inputs:
#'  expr is always a character string
char2booleanFilter <- function(expr, ..., filterId="defaultBooleanFilter") {
  flowCore:::checkClass(expr, "character", 1)
  subs <- parse(text=expr)
  if (missing(filterId)) {
    filterId <- expr
  }
  else {
    flowCore:::checkClass(filterId, "character", 1)
  }
  new("booleanFilter", filterId = filterId, expr = subs, args = list(...),
      deparse = expr)
}


setMethod("show",signature("booleanFilter"),function(object){
			
			msg <- paste("booleanFilter filter '", identifier(object),
					"' evaluating the expression:\n",
					paste(object@deparse, collapse="\n"), sep="")
			cat(msg)
			cat("\n")
			invisible(msg)
		})
