#' @include cytoset.R
NULL

#' @useDynLib flowWorkspace,.registration = TRUE
NULL

#' @name flowWorkspace-package
#' @aliases 
#' flowWorkspace-package
#' flowWorkspace
#' @docType package
#' @importFrom Rcpp evalCpp
#' @importFrom cytolib LdFlags
#' @title Import and replicate flowJo workspaces and gating schemes using flowCore.
#' @description Import flowJo workspaces into R. Generate the flowJo gating hierarchy and gates using flowCore functionality. Transform and compensate data in accordance with flowJo settings. Plot gates, gating hierarchies,  population statistics, and compare flowJo vs flowCore population summaries.
#' @details
#'   \tabular{ll}{
#'     Package: \tab flowWorkspace\cr
#'     Type: \tab Package\cr
#'     Version: \tab 0.5.40\cr
#'     Date: \tab 2011-03-04\cr
#'     License: \tab Artistic 2.0 \cr
#'     LazyLoad: \tab yes\cr
#'     Depends: \tab R (>= 2.16.0),Rcpp (>= 0.9.9)\cr
#'   }
#' @author Greg Finak, Mike Jiang
#' @references \url{http://www.rglab.org/}
NULL

#' @importClassesFrom XML XMLInternalDocument
setOldClass("XMLInternalDocument")

.uuid_gen<-function(len = 20){
#  system("uuidgen",intern = TRUE)
  flowCore:::guid(len)
}    

#' Class \code{"GatingSet"}
#' 
#' GatingSet holds a set of \code{GatingHierarchy} objects, representing a set of samples and the gating scheme associated with each.
#' 
#' @details 
#' Objects stores a collection of GatingHierarchies and represent a group in a flowJo workspace.
#' A GatingSet can have two ``states''. After a call to flowjo_to_gatingset(...,execute=FALSE)
#' , the workspace is imported but the data is not. Setting \code{execute} to \code{TRUE} is needed in order to load, 
#' transform, compensate, and gate the associated data. Whether or not a GatingHierarchy has been applied to data is encoded in the \code{flag} slot. Some methods will warn the user, or may not function correctly if the GatingHierarchy has not been executed.
#' This mechanism is in place, largely for the purpose of speed when working with larger workspaces. 
#' It allows the use to load a workspace and subset desired samples before proceeding to load the data. 
#' 
#' @section Slots:
#' 
#' \describe{
#'     \item{\code{pointer}:}{Object of class \code{"externalptr"}. points to the gating hierarchy stored in C data structure.}
#'     \item{\code{transformation}:}{Object of class \code{"list"}. a list of transformation objects used by GatingSet.}
#'   }
#' 
#' @seealso
#'   \code{\linkS4class{GatingHierarchy}}
#'  
#' @examples
#' \dontrun{
#'   require(flowWorkspaceData)
#'   d<-system.file("extdata",package="flowWorkspaceData")
#'   wsfile<-list.files(d,pattern="A2004Analysis.xml",full=TRUE)
#' 	library(CytoML)
#'   ws <- open_flowjo_xml(wsfile);
#'   G<-try(flowjo_to_gatingset(ws,execute=TRUE,path=d,name=1));
#'   gs_plot_pop_count_cv(G);
#' }
#' @name GatingSet-class
#' @rdname GatingSet-class
#' @exportClass GatingSet
#' @aliases 
#' GatingSet-class
setClass("GatingSet", representation(pointer = "externalptr", transformation = "list"))

setClass("GatingSet_legacy"
         ,representation(pointer = "externalptr"
                         ,FCSPath = "character"
                         ,data = "flowSet"
                         ,flag = "logical"
                         ,axis = "list"
                         ,guid = "character"
                         , transformation = "ANY"
                         , compensation = "ANY"
         )
         ,prototype(FCSPath =""
                    ,data = NULL
                    ,flag = FALSE
                    ,axis = list()
                    ,guid = ""
                    , transformation = list()
                    , compensation = NULL
         )                                       
)
#' Class GatingHierarchy
#' 
#' GatingHierarchy is a class for representing the gating hierarchy,which can be either imported from a flowJo workspace or constructed in R.
#'  
#' @details 
#' There is a one-to-one correspondence between GatingHierarchy objects and FCS files in the flowJo workspace. 
#' Each sample (FCS file) is associated with it's own GatingHierarchy. It is also more space efficient by storing gating results as logical/bit vector instead of copying the raw data.
#' 
#' Given a GatingHierarchy, one can extract the data associated with any subpopulation, extract gates, plot gates, and extract population proportions. This facilitates the comparison of manual gating methods with automated gating algorithms.
#'   
#' @seealso
#' \code{\linkS4class{GatingSet}}
#' 
#' @examples
#' \dontrun{
#' 	require(flowWorkspaceData)
#' 	d<-system.file("extdata",package="flowWorkspaceData")
#' 	wsfile<-list.files(d,pattern="A2004Analysis.xml",full=TRUE)
#' library(CytoML)
#' 	ws <- open_flowjo_xml(wsfile);
#' 	G<-try(flowjo_to_gatingset(ws,path=d,name=1));
#'  gh <- G[[1]]
#' 	gh_pop_compare_stats(gh);
#' 	gh_plot_pop_count_cv(gh)
#'  nodes <- gs_get_pop_paths(gh)
#'  thisNode <- nodes[4]
#'  require(ggcyto)
#' 	autoplot(gh,thisNode);
#' 	gh_pop_get_gate(gh,thisNode);
#' 	gh_pop_get_data(gh,thisNode)
#' }
#' @name GatingHierarchy-class
#' @rdname GatingHierarchy-class
#' @exportClass GatingHierarchy
#' @aliases 
#' show,GatingHierarchy-method
setClass("GatingHierarchy", contains = "GatingSet")

setGeneric("GatingSet",function(x,y,...)standardGeneric("GatingSet"))
        
#' constructors for GatingSet
#' 
#' construct a gatingset with empty trees (just root node)
#' 
#' @rdname GatingSet-methods
#' @export 
#' @examples 
#' \dontrun{
#' #fdata could be a flowSet or ncdfFlowSet
#' gs <- GatingSet(fdata)
#' }
setMethod("GatingSet",c("cytoset"),function(x){
      
      gs <- new("GatingSet", pointer = x@pointer)
	  suppressMessages(recompute(gs))
	  gs
	  
    })
setMethod("GatingSet",c("flowSet"),function(x){
      
      GatingSet(flowSet_to_cytoset(x))
      
    })
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
#'     #convert GatingSetList into one GatingSet by gslist_to_gs
#'     gs_merged2 <- gslist_to_gs(gslist2,ncdfFile=path.expand(tempfile(tmpdir="~/rglab/workspace/flowIncubator/output/",fileext=".nc")))
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

.getNodes_removeHidden <- function(gh){
  complete <- gs_get_pop_paths(gh, showHidden = TRUE)
  sub <- gs_get_pop_paths(gh, showHidden = FALSE)
  hiddenInd <- which(!complete%in%sub)
  #remove hidden node from paths
  for(i in hiddenInd){
    thisHidden <- complete[i]
    hiddenPopName <- basename(thisHidden)
    parent <- gs_pop_get_parent(gh, thisHidden)
    sub <- gsub(thisHidden, parent, sub)
  }
  sub
}        
#TODO:gating tree comparison needs to be improved 
# can't use RBGL::isomorphism since it will not take care of the hidden nodes
.compareGatingHierarchy<-function(gh1,gh2){
  tree1 <- .getNodes_removeHidden(gh1)
  tree2 <- .getNodes_removeHidden(gh2)
  
  if(setequal(tree1, tree2)){
    return (TRUE)
  }else{
    return (paste("gating structure doesn't match:",sampleNames(gh1),sampleNames(gh2)))
  }
}
.compareFlowData <- function(fs1,fs2){
  #it is strange that colnames doesn't dispatch properly without namespace prefix
  col1 <- flowCore::colnames(fs1)
  col2 <- flowCore::colnames(fs2)
  if(!setequal(col1,col2)){
    msg <- paste("colnames of flowSets don't match!")
    return (msg)
  }
  if(!setequal(colnames(pData(fs1)),colnames(pData(fs2)))){
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
  fs1 <- gs_pop_get_data(gs1)
  fs2 <- gs_pop_get_data(gs2)
  .compareFlowData(fs1,fs2)
}

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


#' A class describing logical operation (& or |) of the reference populations
#' 
#' \code{booleanFilter} class inherits class \code{\link{expressionFilter}} and exists for the purpose of methods dispatching.
#' 
#' @seealso \code{\link{add}} \code{\linkS4class{GatingHierarchy}}
#' @name booleanFilter-class
#' @rdname booleanFilter-class
#' @exportClass booleanFilter
#' @aliases 
#' show,booleanFilter-method
setClass("booleanFilter"
		,contains=c("expressionFilter")
)

#' \code{booleanFilter} is a constructor from an expression
#' @param expr \code{expression}
#' @param ... further arguments to the expression
#' @param filterId \code{character} identifier
#' @rdname booleanFilter-class
#' @export 
#' @examples 
#' # "4+/TNFa+" and "4+/IL2+" are two existing gates
#' #note: no spaces between node names and & , ! operators
#' booleanFilter(`4+/TNFa+&!4+/IL2+`)
#' 
#' #programmatically 
#' n1 <- "4+/TNFa+"
#' n2 <- "4+/IL2+"
#' exprs <- paste0(n1, "&!", n2)
#' call <- substitute(booleanFilter(v), list(v = as.symbol(exprs)))
#' eval(call)
booleanFilter <- function(expr, ..., filterId="defaultBooleanFilter")
{
  	
  if(missing(expr))
    new("booleanFilter", filterId=filterId)
  else
  {
    subs <- substitute(expr)
    if(is(subs, "character"))
      stop("booleanFilter constructor doesn't take character!Please use the experssion (enclosed by backticks if the special characters are present.)")
    if(missing(filterId)){
      filterId <- deparse(subs)
      if(length(filterId)>1)
        filterId <- paste(gsub("^ *", "", filterId[2]), "...", sep="")
    }else flowCore:::checkClass(filterId, "character", 1)
    
    new("booleanFilter", filterId=filterId, expr=as.expression(subs),
        args=list(...), deparse=deparse(subs))
  }
    
}

#' \code{char2booleanFilter} is a constructor from a character string
#' @rdname booleanFilter-class 
#' @export 
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

#' @param object \code{booleanFilter}
#' @rdname booleanFilter-class
#' @export 
setMethod("show",signature("booleanFilter"),function(object){
			
			msg <- paste("booleanFilter filter '", identifier(object),
					"' evaluating the expression:\n",
					paste(object@deparse, collapse="\n"), sep="")
			cat(msg)
			cat("\n")
			invisible(msg)
		})
