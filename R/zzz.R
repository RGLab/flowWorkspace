## Store state info in this internal global environment
flowWorkspace.state <- new.env(hash = FALSE)
flowWorkspace.state[["par"]] <- list()

#' workspace version is parsed from xml node '/Workspace/version' in flowJo workspace
#' and matched with this list to dispatch to the one of the three workspace parsers 
flowWorkspace.par.init <- function(){
  flowWorkspace.state[["par"]][["flowJo_versions"]] <- list(win = c("1.61", "1.6")
      ,mac = c("2.0")
      ,vX = c("1.8", "20.0")
  )
}

## call the init function
flowWorkspace.par.init()                                                           

#' flowWorkspace.par.set sets a set of parameters in the flowWorkspace package namespace.
#' 
#' @param value A named list of values to set for category name or a list of such lists if name is missing.
#' @rdname flowWorkspace.par.get
#' @export
flowWorkspace.par.set <- function (name, value) 
{
    old <- flowWorkspace.state[["par"]]
    if(name%in%names(old)){
      flowWorkspace.state[["par"]][[name]] <- lattice:::updateList(old[[name]], value)  
    }else
      stop(name, " is not a valid flowWorkspace parameters!")
    
  invisible()
}

#' Query and set session-wide parameter defaults.
#' 
#' flowWorkspace.par.get gets a set of parameters in the flowWorkspace package namespace.
#' 
#' It is currently used to add/remove the support for a specific flowJo versions (parsed from xml node '/Workspace/version' in flowJo workspace)
#' 
#' @param name The name of a parameter category to get or set.
#'
#' 
#' @examples
#'  #get the flowJo versions currently supported 
#'  old <- flowWorkspace.par.get("flowJo_versions")
#' 
#'  #add the new version
#'  old[["win"]] <- c(old[["win"]], "1.7")    
#'  flowWorkspace.par.set("flowJo_versions", old)
#'  
#'  flowWorkspace.par.get("flowJo_versions")
#' 
#' @export 
#' @rdname flowWorkspace.par.get
flowWorkspace.par.get <- function (name = NULL) 
{
  lPars <- flowWorkspace.state[["par"]]
  if (is.null(name)) 
    lPars
  else if (name %in% names(lPars)) 
    lPars[[name]]
  else NULL
}

