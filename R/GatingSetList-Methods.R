#' @include GatingSet-Methods.R
NULL

setMethod("rbind2",
    signature=signature("GatingSetList","missing"),
    definition=function(x,y="missing",...)
    {
#           browser()
      isNcdfList<-lapply(x,isNcdf, level = 1)
      if(all(duplicated(unlist(isNcdfList))[-1])){
#               browser()
        #combine flowset/ncdfFlowSet
        fsList <- lapply(x, getData, level =1)
        if(isNcdfList[[1]])
          fs<-rbind2(as(fsList,"ncdfFlowList"),...)
        else
        {
          ##using original flowCore::rbind2 for flowSet
          fs<-fsList[[1]]
          for(i in 2:length(fsList))
            fs<-rbind2(fs,fsList[[i]])
        }
        
        #combine tree structure
        ptrlist <- lapply(x,function(gs)gs@pointer, level =1)
        sampleList <- lapply(x, sampleNames, level =1)
        pointer <- .Call("R_combineGatingSet",ptrlist,sampleList,package="flowWorkspace")
        G <- new("GatingSet")
        G@pointer <- pointer
        G@guid <- .uuid_gen()
        G@flag <- TRUE
        G@axis <- unlist(lapply(x,slot,"axis",level = 1),recursive = FALSE)
        #combine R objects
        
        flowData(G) <- fs
        
      }else{
        stop("Can't combine gating sets. They should all use the same storage method. (Netcdf, or not..)")
      }
      return(G);  
      
    })
setMethod("show",
    signature = signature(object="GatingSetList"),
    definition = function(object) { 
      cat("A GatingSetList with", length(object@data),"GatingSet\n")
      cat("containing", length(unique(sampleNames(object))), " unique samples.") 
      cat("\n")
    })


setMethod("getSamples","GatingSetList",function(x){
      stop("'getSamples' is defunct.\nUse 'sampleNames' instead.")
    })

setMethod("sampleNames", 
    signature = signature(object = "GatingSetList"),
    function(object) {
      object@samples      
    })


#' @param X \code{GatingSet} or \code{GatingSetList} object
#' @param FUN \code{function} to apply
#' @param level \code{numeric}. When \code{X} is a \code{GatingSetList}, \code{level} 2 (default value)
#' \code{FUN} is applied to each individual \code{GatingHierarchy}. When it is set to 1, \code{FUN} is applied to each \code{GatingSet}  
#' 
#' @rdname lapply-methods
#' @export 
#' @aliases 
#' lapply,GatingSetList-method
setMethod("lapply","GatingSetList",function(X,FUN, level = 2,...){
      if(level == 1)
        lapply(X@data,FUN,...)
      else
      {
        sapply(sampleNames(X),function(thisSample,...){
              gh <- X[[thisSample]]
              FUN(gh, ...)
            }, simplify = FALSE, ...)
      }
    })

setMethod("[[",c(x="GatingSetList",i="numeric"),function(x,i,j,...){
      #convert non-character indices to character
#      browser()
      this_samples <- sampleNames(x)
      nSamples <- length(this_samples)
      if(i > nSamples){
        stop(i, " is larger than the number of samples: ", nSamples)
      }
        x[[this_samples[i]]]
      
    })

setMethod("[[",c(x="GatingSetList",i="logical"),function(x,i,j,...){
      #convert non-character indices to character
      
      x[[sampleNames(x)[i]]]
      
    })
setMethod("[[",c(x="GatingSetList",i="character"),function(x,i,j,...){
      #convert non-character indices to character
      gh <- NULL
      for(gs in x@data){
#              browser()
            this_samples <- sampleNames(gs)
            ind <- match(i,this_samples)
            if(!is.na(ind)){
              gh <- gs[[ind]]
            }
      }
      if(is.null(gh)){
        stop(i, " not found in GatingSetList!")
      }else{
        return (gh)
      }
    })
setMethod("[",c(x="GatingSetList",i="numeric"),function(x,i,j,...){
#      browser()
      x[sampleNames(x)[i]]
      
    })

setMethod("[",c(x="GatingSetList",i="logical"),function(x,i,j,...){
      
      x[sampleNames(x)[i]]
   
    })

setMethod("[",c(x="GatingSetList",i="character"),function(x,i,j,...){
#      browser()
      samples <- sampleNames(x)
      matchInd <- match(i,samples)
      noFound <- is.na(matchInd)
      if(any(noFound)){
        stop(i(noFound), "not found in GatingSetList!")
      }
      res <- lapply(x,function(gs){
#            browser()
                  this_samples <- sampleNames(gs)
                  ind <- match(i,this_samples)
                  this_subset <- i[!is.na(ind)] 
                  if(length(this_subset)>0){
                    return (gs[this_subset])
                  }else{
                    NULL
                  }
                }, level =1)
      res <- res[!unlist(lapply(res,is.null))]
      res <- GatingSetList(res)
      res@samples <- samples[matchInd]
      res
    })


setMethod("getData",c(obj="GatingSetList",y="missing"),function(obj,y,...){
      stop("node index 'y' is missing!")
    })

#' @param  max \code{numeric} The maximum number of samples to be returned. It is used as a threshold to prevent huge memory consumption due to the coersion from ncdfFlowSet to flowSet 
#' @aliases 
#' getData,GatingSetList,missing-method
#' getData,GatingSetList,numeric-method
#' getData,GatingSetList,character-method
#' @rdname getData-methods
setMethod("getData",signature(obj="GatingSetList",y="numeric"),function(obj,y,max=30,...){

      if(length(sampleNames(obj))>max){
        stop("You are trying to return a flowSet for more than ", max, " samples!Try to increase this limit by specifing 'max' option if you have enough memory.")
      }
      
      res <- lapply(obj,function(gs){
            ncfs <- getData(gs,y, ...)
            as.flowSet(ncfs)
          }, level =1)
      fs<-res[[1]]
      if(length(res)>1){
        for(i in 2:length(res))
          fs<-rbind2(fs,res[[i]])
      }
      
      fs
    })
setMethod("getData",c(obj="GatingSetList",y="character"),function(obj, y,  ...){

      getData(obj,.getNodeInd(obj[[1]],y),...)
      
    })

#' @aliases
#' pData,GatingSetList-method
#' pData<-,GatingSetList,data.frame-method
#' @rdname pData-methods
setMethod("pData","GatingSetList",function(object){

      res <- lapply(object,pData, level =1)
#            browser()
      res <- do.call(rbind,res)
      res[object@samples,,drop=FALSE]
    })

setReplaceMethod("pData",c("GatingSetList","data.frame"),function(object,value){
      if(!.isValidSamples(rownames(value),object))
        stop("The sample names in data.frame are not consistent with the GatingSetList!")
        
      res <- lapply(object,function(gs){
                    this_pd <- subset(value,name%in%sampleNames(gs))
                    pData(gs) <- this_pd
                    gs
                  }, level =1)
              
      res <- GatingSetList(res)
      res        
    })

setMethod("getGate",signature(obj="GatingSetList",y="numeric"),function(obj,y,tsort=FALSE){
      res <- lapply(obj,function(gs){
            getGate(gs,y)      
          }, level =1)
      unlist(res,recur=FALSE)
    })
setMethod("getGate",signature(obj="GatingSetList",y="character"),function(obj,y,tsort=FALSE){
      
      getGate(obj,.getNodeInd(obj[[1]],y))
      
      
    })

#' @aliases
#' plotGate,GatingSetList,numeric-method
#' plotGate,GatingSetList,character-method
#' @rdname plotGate-methods
setMethod("plotGate",signature(x="GatingSetList",y="numeric"),function(x,y, ...){
      selectMethod("plotGate",signature = c(x="GatingSet",y="numeric"))(x=x, y=y, ...)
      
    })

setMethod("plotGate",signature(x="GatingSetList",y="character"),function(x,y, ...){
      selectMethod("plotGate",signature = c(x="GatingSet",y="character"))(x=x, y=y, ...)
    })

setMethod("getPopStats","GatingSetList",function(x,...){
      res <- lapply(x,getPopStats, level =1,...)
      res<-Reduce(function(x,y)
        {
          merge(x,y,all=TRUE)
        },
             lapply(res,function(x)
               {
                rn<-rownames(x);
                x<-data.table(x);
                x$key<-rn;
                setkeyv(x,"key")
                }))
      rn<-res$key
      res[,key:=NULL]
      res<-as.matrix(res)
      rownames(res)<-rn
      res
    })

setMethod("keyword",c("GatingSetList", "missing"),function(object,keyword = "missing"){
      selectMethod("keyword",signature = c(x="GatingSet",y="missing"))(object, keyword)
      
    })

setMethod("keyword",c("GatingSetList","character"),function(object,keyword){
      selectMethod("keyword",signature = c(x="GatingSet",y="character"))(object, keyword)
    })

#' @aliases 
#' length,GatingSetList-method
#' @rdname length-methods
setMethod("length","GatingSetList",function(x){
      length(sampleNames(x));
    })
#' @rdname save_gs
#' @export
save_gslist<-function(gslist,path,...){
    
  if(!file.exists(path))
    dir.create(path = path)
  #do the dir normalization again after it is created
  path <- normalizePath(path,mustWork = TRUE)
  lapply(gslist,function(gs){
#        this_dir <- tempfile(pattern="gs",tmpdir=path)
#        dir.create(path = this_dir)
#        browser()
        guid <- gs@guid
        if(length(guid)==0){
          gs@guid <- flowWorkspace:::.uuid_gen()
          guid <- gs@guid
        }
        this_dir <- file.path(path,guid) 

#        invisible(flowWorkspace:::.save_gs(gs,path = this_dir, ...))
        suppressMessages(save_gs(gs,path = this_dir, ...))
      }, level =1)
#  browser()
  #save sample vector
  saveRDS(gslist@samples,file=file.path(path,"samples.rds"))
  message("Done\nTo reload it, use 'load_gslist' function\n")
  
  
}

#' @rdname save_gs
#' @export
load_gslist<-function(path){
#  browser()
  path <- normalizePath(path,mustWork = TRUE)
  if(!file.exists(path))
    stop(path,"' not found!")
  dirs<-list.dirs(path,full.names = TRUE, recursive = FALSE)
#   browser()
  res <- lapply(dirs,function(this_dir){
#        browser()
        flowWorkspace:::.load_gs(output = this_dir, files = list.files(this_dir))$gs      
      })
  samples <- readRDS(file.path(path,"samples.rds"))
  GatingSetList(res, samples = samples)
  
}
