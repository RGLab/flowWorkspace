#' @include GatingSet-Methods.R
NULL

#' @param y \code{missing} not used.
#' @param ... other arguments passed to \code{rbind2} method for \code{ncdfFlowList}
#' @importMethodsFrom ncdfFlow lapply rbind2 pData<-
#' @rdname GatingSetList-class 
#' @export 
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
          fs<-rbind2(ncdfFlowList(fsList), ...)
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



#' @rdname GatingSet-class 
#' @export
setMethod("[",c(x="GatingSetList",i="ANY"),function(x,i,j,...){
      object <- callNextMethod()
      as(object, "GatingSetList")
    })


#' @name pData
#' @rdname pData-methods
#' @usage \S4method{pData}{GatingSetList,data.frame}(object) <- value
#' @aliases 
#' pData<-,GatingSetList,data.frame-method
#' @export
setReplaceMethod("pData",c("GatingSetList","data.frame"),function(object,value){
      res <- callNextMethod()
      as(res, "GatingSetList")
    })

#' @rdname recompute
#' @export
setMethod("recompute",c("GatingSetList"),function(x, ...){
      selectMethod("recompute", signature = c("GatingSet"))(x, ...)
      
    })

#' @rdname getData-methods
#' @export
setMethod("getData",signature(obj="GatingSetList",y="ANY"),function(obj,y, ...){
      
      samples_orig <- obj@samples
      if(missing(y))
        y <- NULL
      else if(!is.character(y))
        stop(" 'numeric` indexing is no longer safe . Please use node name instead!")        
      res <- lapply(obj,function(gs){
            
            if(is.null(y))
              ncfs <- getData(gs, ...)
            else
              ncfs <- getData(gs,y, ...)
            ncfs
          }, level =1)
      ncdfFlowList(res, samples_orig)
      
    })




#' @rdname getGate
#' @export
setMethod("getGate",signature(obj="GatingSetList",y="character"),function(obj,y){
      
      res <- lapply(obj,function(gs){
            getGate(gs,y)      
          }, level =1)
      unlist(res,recur=FALSE)
      
    })

#' @export 
#' @rdname plotGate-methods
setMethod("plotGate",signature(x="GatingSetList",y="character"),function(x,y, ...){
      selectMethod("plotGate",signature = c(x="GatingSet",y="character"))(x=x, y=y, ...)
    })
#' @rdname getPopStats
#' @export
setMethod("getPopStats","GatingSetList",function(x, format = c("long", "wide"), ...){
      
      format <- match.arg(format)
      res <- lapply(x,getPopStats, level =1, format = format,...)
      
      if(format == "long"){
#        browser()
        res <- rbindlist(res)
      }else{
        
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
      }
      res
    })
#' @rdname keyword
#' @export
setMethod("keyword",c("GatingSetList", "missing"),function(object,keyword = "missing"){
      selectMethod("keyword",signature = c(x="GatingSet",y="missing"))(object, keyword)
      
    })
#' @rdname keyword
#' @export
setMethod("keyword",c("GatingSetList","character"),function(object,keyword){
      selectMethod("keyword",signature = c(x="GatingSet",y="character"))(object, keyword)
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
  saveRDS(names(gslist@samples),file=file.path(path,"samples.rds"))
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

#' Replace a single marker name with another
#' Scan through a gating set list and rename all flowFrames with marker \code{match}
#' to marker \code{replace}
#'@return a \code{GatingSetList}
.renameMarker<-function(g=NA,match=NA,replace=NA){
  if(!inherits(g,"GatingSetList"))
    stop("g must be a GatingSetList")
  listofgs<-lapply(g@data,function(x,m=match,r=replace){
    samps<-sampleNames(flowData(x))
    fd<-flowData(x)
    for(i in samps){
      f <- fd@frames[[i]]
      adf <- parameters(f)
      pd <- pData(adf)
      mtch <- as.matrix(pd["desc"])%in%m
      if(any(mtch)){
        pd[mtch,"desc"]<-r
        pData(adf)<-pd
        parameters(f)<-adf
        fd@frames[[i]]<-f
      }
    }
    flowData(x)<-fd
    x
  })
  listofgs
  g@data<-listofgs
  g
}
#' @rdname getSingleCellExpression
#' @export
setMethod("getSingleCellExpression",signature=c("GatingSetList","character"),function(x, nodes, ...){
      
      res <- lapply(x, function(gs)getSingleCellExpression(gs, nodes, ...), level = 1)
      unlist(res, recursive = FALSE)
      
    })

#' @rdname transform
setMethod("transform",
    signature = signature(`_data` = "GatingSetList"),
    definition = function(`_data`, ...)
    {
      res <- lapply(`_data`, function(gs)transform(gs, ...), level = 1)
      GatingSetList(res)
    })
