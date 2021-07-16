#' @include cytoset.R
NULL

#' @useDynLib flowWorkspace,.registration = TRUE
NULL

#' @name flowWorkspace-package
#' @aliases 
#' flowWorkspace-package
#' flowWorkspace
#' @docType package
#' @importFrom cytolib cytolib_LdFlags
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
#'     Depends: \tab R (>= 2.16.0)\cr
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
#'  require(flowWorkspaceData)
#'  d<-system.file("extdata",package="flowWorkspaceData")
#'  wsfile<-list.files(d,pattern="A2004Analysis.xml",full=TRUE)
#'  library(CytoML)
#'  ws <- open_flowjo_xml(wsfile);
#'  G<-try(flowjo_to_gatingset(ws,execute=TRUE,path=d,name=1));
#'  gs_plot_pop_count_cv(G);
#' }
#' @name GatingSet-class
#' @rdname GatingSet-class
#' @exportClass GatingSet
#' @aliases 
#' GatingSet-class GatingSet
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
#'  require(flowWorkspaceData)
#'  d<-system.file("extdata",package="flowWorkspaceData")
#'  wsfile<-list.files(d,pattern="A2004Analysis.xml",full=TRUE)
#'  library(CytoML)
#'  ws <- open_flowjo_xml(wsfile);
#'  G<-try(flowjo_to_gatingset(ws,path=d,name=1));
#'  gh <- G[[1]]
#'  gh_pop_compare_stats(gh);
#'  gh_plot_pop_count_cv(gh)
#'  nodes <- gs_get_pop_paths(gh)
#'  thisNode <- nodes[4]
#'  require(ggcyto)
#'  autoplot(gh,thisNode);
#'  gh_pop_get_gate(gh,thisNode);
#'  gh_pop_get_data(gh,thisNode)
#' }
#' @name GatingHierarchy-class
#' @rdname GatingHierarchy-class
#' @exportClass GatingHierarchy
#' @aliases 
#' show,GatingHierarchy-method GatingHierarchy
setClass("GatingHierarchy", contains = "GatingSet")

setGeneric("GatingSet",function(x,y,...)standardGeneric("GatingSet"))
        
#' constructors for GatingSet
#' 
#' construct a gatingset with empty trees (just root node)
#' 
#' @name GatingSet-methods
#' @aliases GatingSet,cytoset,ANY-method GatingSet,flowSet-method GatingSet,flowSet,ANY-method
#' @param x a flowSet, ncdfFlowSet, or cytoset
#' @param ... arguments passed to flowSet_to_cytoset() when x is a flowSet
#' @export 
#' @examples 
#' \dontrun{
#' #fdata could be a flowSet, ncdfFlowSet, or GatingSet
#' gs <- GatingSet(fdata)
#' }
setMethod("GatingSet",c("cytoset"),function(x){
      
      gs <- new("GatingSet", pointer = x@pointer)
	  suppressMessages(recompute(gs))
	  gs
	  
    })
setMethod("GatingSet",c("flowSet"),function(x, ...){
      
      GatingSet(flowSet_to_cytoset(x, ...))
      
    })


#' A class describing logical operation (& or |) of the reference populations
#' 
#' \code{booleanFilter} class inherits class \code{\link{expressionFilter}} and exists for the purpose of methods dispatching.
#' 
#' @name booleanFilter-class
#' @seealso \code{\link{add}} \code{\linkS4class{GatingHierarchy}}
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

#' @export 
setMethod("show",signature("booleanFilter"),function(object){
			
			msg <- paste("booleanFilter filter '", identifier(object),
					"' evaluating the expression:\n",
					paste(object@deparse, collapse="\n"), sep="")
			cat(msg)
			cat("\n")
			invisible(msg)
		})
