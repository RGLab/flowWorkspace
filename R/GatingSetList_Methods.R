#' @include GatingSet_Methods.R
NULL

#' @templateVar old rbind2
#' @templateVar new gslist_to_gs
#' @template template-depr_pkg
NULL
#' @param y \code{missing} not used.
#' @rdname gslist_to_gs 
#' @export 
setMethod("rbind2",
    signature=signature("GatingSetList","missing"),
    definition=function(x,y="missing",...)
    {
		.Deprecated("gslist_to_gs")
		gslist_to_gs(x, ...)
	})
#' Merge a GatingSetList into a single GatingSet
#' 
#' @param x GatingSetList
#' @param ... other arguments passed to \code{gslist_to_gs} method for \code{ncdfFlowList}
#' @rdname gslist_to_gs 
#' @export 
gslist_to_gs <- function(x,...){
#           browser()
      #combine tree structure
      ptrlist <- lapply(x,function(gs)gs@pointer, level =1)
      sampleList <- lapply(x, sampleNames, level =1)
      new("GatingSet", pointer = .cpp_combineGatingSet(ptrlist,sampleList))
    }
	
#' @rdname GatingSet-class 
#' @export
setMethod("[",c(x="GatingSetList",i="ANY"),function(x,i,j,...){
      object <- callNextMethod()
      as(object, "GatingSetList")
    })


#' @export
setReplaceMethod("pData",c("GatingSetList","data.frame"),function(object,value){
      res <- callNextMethod()
      as(res, "GatingSetList")
    })



#' @export
setMethod("getData",signature(obj="GatingSetList",y="ANY"),function(obj,y, ...){
			.Deprecated("gs_pop_get_data")
			gs_pop_get_data(obj, y)
			
		})
      



#' @rdname gh_pop_get_gate
#' @export
setMethod("getGate",signature(obj="GatingSetList",y="character"),function(obj,y){
			.Deprecated("gs_pop_get_gate")
			gs_pop_get_gate(obj, y)
		})


#' @export 
#' @rdname plotGate-methods-defunct
setMethod("plotGate",signature(x="GatingSetList",y="character"),function(x,y, ...){
          .Defunct("ggcyto::autoplot", "flowWorkspace")
    })
.gslist_get_pop_stats <- function(x, format = c("long", "wide"), ...){
      
      format <- match.arg(format)
      res <- lapply(x,gs_pop_get_count_fast, level =1, format = format,...)
      
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
    }
#' @export
setMethod("keyword",c("GatingSetList", "missing"),function(object,keyword = "missing", ...){
      selectMethod("keyword",signature = c(x="GatingSet",y="missing"))(object, keyword, ...)
      
    })
#' @export
setMethod("keyword",c("GatingSetList","character"),function(object,keyword){
      selectMethod("keyword",signature = c(x="GatingSet",y="character"))(object, keyword)
    })



#' Replace a single marker name with another
#' Scan through a gating set list and rename all flowFrames with marker \code{match}
#' to marker \code{replace}
#'@return a \code{GatingSetList}
#' @noRd 
.renameMarker<-function(g=NA,match=NA,replace=NA){
  if(!inherits(g,"GatingSetList"))
    stop("g must be a GatingSetList")
  listofgs<-lapply(g@data,function(x,m=match,r=replace){
    samps<-sampleNames(gs_cyto_data(x))
    fd<-gs_cyto_data(x)
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
    gs_cyto_data(x)<-fd
    x
  })
  listofgs
  g@data<-listofgs
  g
}

#' @export
setMethod("transform",
    signature = signature(`_data` = "GatingSetList"),
    definition = function(`_data`, ...)
    {
      res <- lapply(`_data`, function(gs)transform(gs, ...), level = 1)
      GatingSetList(res)
    })

#' @rdname compensate
setMethod("compensate", signature=signature(x="GatingSetList", spillover="ANY"),
    definition=function(x, spillover){
      res <- lapply(x, function(gs)compensate(gs, spillover), level = 1)
      GatingSetList(res)
    })
