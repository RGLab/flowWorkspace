#' @include GatingSet_Methods.R
NULL
#' Class \code{"GatingSetList"}
#' 
#'   A list of of \code{GatingSet} objects. This class exists for method dispatching.
#' 
#' @details 
#'   Objects store a collection of GatingSets,which usually has the same gating trees and markers.
#'   Most GatingSets methods can be applied to GatingSetList.
#' 
#' @seealso
#'   \code{\linkS4class{GatingSet}}
#'   \code{\linkS4class{GatingHierarchy}}
#' 
#' @examples
#'   \dontrun{
#'     #load several GatingSets from disk
#'    gs_list<-lapply(list.files("../gs_toMerge",full=T) ,function(this_folder){
#'                      load_gs(this_folder)
#'                      })
#'     
#'    #gs_list is a list
#'     gs_groups <- merge(gs_list)
#'     #returns a list of GatingSetList objects
#'     gslist2 <- gs_groups[[2]]
#'     #gslist2 is a GatingSetList that contains multiple GatingSets and they share the same gating and data structure
#'     gslist2
#'     class(gslist2)
#'     sampleNames(gslist2)
#'     
#'     #reference a GatingSet by numeric index
#'     gslist2[[1]]
#'     #reference a GatingSet by character index
#'     gslist2[["30104.fcs"]]
#'     
#'     #loop through all GatingSets within GatingSetList
#'     lapply(gslist2,sampleNames)
#'     
#'     #subset a GatingSetList by [
#'     sampleNames(gslist2[c(4,1)])
#'     sampleNames(gslist2[c(1,4)])
#'     gslist2[c("30104.fcs")]
#'     
#'     #get flow data from it
#'     gs_pop_get_data(gslist2)
#'     #get gated flow data from a particular popoulation 
#'     gs_pop_get_data(gslist2, "3+")
#'     
#'     #extract the gates associated with one popoulation
#'     gs_pop_get_gate(gslist2,"3+")
#'     gs_pop_get_gate(gslist2,5)
#'     
#'     #extract the pheno data
#'     pData(gslist2[3:1])
#'     #modify the pheno data
#'     pd <- pData(gslist2)
#'     pd$id <- 1:nrow(pd)
#'     pData(gslist2) <- pd
#'     pData(gslist2[3:2])
#' 
#'     #plot the gate
#'     autoplot(gslist2[1:2],5)
#'     
#'     #remove cerntain gates by loop through GatingSets
#'     gs_get_pop_paths(gslist2[[1]])
#'     lapply(gslist2,function(gs)gs_pop_remove("Excl",gs = gs))
#'     
#'     #extract the stats
#'     gs_pop_get_count_fast(gslist2)
#'     #extract statistics by using getQAStats defined in QUALIFIER package
#'     res<-getQAStats(gslist2[c(4,2)],isMFI=F,isSpike=F,nslaves=1)
#'     
#'     #archive the GatingSetList
#'     save_gslist(gslist2, path ="~/rglab/workspace/flowIncubator/output/gslist",overwrite=T)
#'     gslist2 <- load_gslist(path ="~/rglab/workspace/flowIncubator/output/gslist")
#'     
#'     #convert GatingSetList into one GatingSet by merge_list_to_gs
#'     gs_merged2 <- merge_list_to_gs(gslist2)
#'     gs_merged2
#'   }
#' 
#' @name GatingSetList-class
#' @rdname GatingSetList-class
#' @exportClass GatingSetList
#' @aliases 
#' GatingSetList-class
#' GatingSetList
setClass("GatingSetList", contains = "ncdfFlowList", slots = c(guid = "character"))

validGatingSetListObject <- function(object){
	
	gs_list <- object@data
	#check overlapping samples
	gs_samples <- unlist(lapply(gs_list, sampleNames))
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
	if(!ncdfFlow:::.isValidSamples(names(object@samples),object)){
		return ("'samples' slot is not consisitent with sample names from GatingSets!")
	}          
	return (TRUE)
}

setValidity("GatingSetList", validGatingSetListObject)     
#' @description use \code{GatingSetList} constructor to create a GatingSetList from a list of GatingSet
#' 
#' @param x a \code{list} of \code{GatingSet}
#' @param samples \code{character} vector specifying the order of samples.
#'                 if not specified, the samples are ordered as the underlying stored order.
#'
#' @rdname GatingSetList-class 
#' @export 
#' @examples 
#' \dontrun{
#' samleNames(gsA) # return A1, A2
#' samleNames(gsB) # return B1, B2
#' gs.list <- list(gsA, gsB)
#' gslist<- GatingSetList(gs.list)
#' sampleNames(gslist) #return A1,A2,B1,B2
#' 
#' #set different order when create the GatingSetList
#' gslist<- GatingSetList(gs.list, samples = c("A1","B1", "A2", "B2"))
#' sampleNames(gslist) #return A1,B1,A2,B2
#' }
GatingSetList <- function(x,samples = NULL)
{
	.Deprecated("merge_list_to_gs")
	names(x)<-NULL#strip names from the list because rbind2 doesn't like it
	flowCore:::checkClass(x, "list")
	
	if(is.null(samples)){
		x <- ncdfFlowList(x)
	}else{
		x <- ncdfFlowList(x, samples)
		
	}
	
	x<- as(x, "GatingSetList")
	
	if(validObject(x)){
		gslist <- x@data
		# make sure the column names of flow data are in the same order
		cols <- flowCore::colnames(gs_cyto_data(gslist[[1]]))
		gslist <- lapply(gslist, function(gs){
					gs_cyto_data(gs) <- gs_cyto_data(gs)[,cols]
					gs
				})
		x@data <- gslist
		x@guid <- .uuid_gen()
		x
	}
	
}

#' @templateVar old rbind2
#' @templateVar new gslist_to_gs
#' @template template-depr_pkg
NULL

#' @export 
setMethod("rbind2",
    signature=signature("GatingSetList","missing"),
    definition=function(x,y="missing",...)
    {
		.Defunct("merge_list_to_gs")
			})

#' Merge a GatingSetList into a single GatingSet
#' 
#' @name gslist_to_gs
#' @aliases rbind2,GatingSetList,missing-method
#' @param x GatingSetList
#' @param ... other arguments passed to \code{gslist_to_gs} method for \code{ncdfFlowList}
#' @export 
gslist_to_gs <- function(x,...){
	.Defunct("merge_list_to_gs")
	#combine tree structure
	ptrlist <- lapply(x,function(gs)gs@pointer, level =1)
	sampleList <- lapply(x, sampleNames, level =1)
	new("GatingSet", pointer = cpp_combineGatingSet(ptrlist,sampleList))
}

#' @name recompute
#' @export
recompute.GatingSetList <- function(x, ...){
			invisible(lapply(x, recompute, ..., level = 1))
		}

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
      



#' @export
setMethod("getGate",signature(obj="GatingSetList",y="character"),function(obj,y){
			.Deprecated("gs_pop_get_gate")
			gs_pop_get_gate(obj, y)
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

#' @export
setMethod("compensate", signature=signature(x="GatingSetList", spillover="ANY"),
    definition=function(x, spillover){
      res <- lapply(x, function(gs)compensate(gs, spillover), level = 1)
      GatingSetList(res)
    })
