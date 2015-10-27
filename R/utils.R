#' Update the channel information of a GatingSet (c++ part)
#' 
#' It updates the channels stored in gates,compensations and transformations
#' based on given mapping between the old and new channel names.
#' 
#' @param gs a GatingSet object
#' @param map \code{data.frame} contains the mapping from old (case insensitive) to new channel names
#'                             Note: Make sure to remove the '<' or '>' characters from 'old` name because the API tries 
#'                                   to only look at the raw channel name so that the gates with both prefixed and non-prefixed names could be updated. 
#' @param all \code{logical} whether to update the flow data as well
#'                                    
#' @return when 'all' is set to TRUE, it returns a new GatingSet but it still shares the same underling c++ tree structure with the original GatingSet                                  
#'  otherwise it returns nothing (less overhead.)
#' @examples 
#' \dontrun{
#'   ##this will update both "Qdot 655-A" and "<Qdot 655-A>"
#'  gs <- updateGateParameter(gs, map = data.frame(old = c("Qdot 655-A")
#'                                          , new = c("QDot 655-A")
#'                                          )
#'                      )  
#'}
#' @export 
 updateChannels <- function(gs, map, all = TRUE){
   
   map <- .preprocessMap(gs, map)
   
   #update gates and comps ,trans
   .updateChannels(gs, map)
   
   #update flow data
  if(all){
    fs <- flowData(gs)  
    cols <- colnames(fs)
    
    newCols <- sapply(cols, function(col){
                      colInd <- match(col, map[["old"]])
                      ifelse(is.na(colInd), col, map[["new"]][colInd])
                    }, USE.NAMES = F)
    
          
    colnames(flowData(gs)) <- newCols
    gs
  }
  
}

#' validity check and add prefixed entries when applicable
.preprocessMap <- function(gs, map){
  if(!is.data.frame(map))
    stop("'map' must be a data.frame!")
  if(!setequal(c("old", "new"), colnames(map)))
     stop("'map' must contain (only) 'old' and 'new' columns!")
  
  map[["old"]] <- as.character(map[["old"]]) 
  map[["new"]] <- as.character(map[["new"]]) 
  
  
  #add prefixed entries
  # we do it unconditionally to all entries
  #since user may update C++ part and flow data separately
  #and we have to make sure updated comp.chnls
  #doesn't interfere the second run of updateChannels
  
  sn <- .Call("R_getSamples", gs@pointer)[1] #can't use sampleNames(gs) since flow data may not be bound to gs yet when it is used within parseWorkspace
  comp <- .Call("R_getCompensation", gs@pointer, sn)
#   comp.chnl <- comp$parameters
  prefix <- comp$prefix
  suffix <- comp$suffix
  map %>% 
    group_by(old) %>%
    do({
      
#       if(.[["old"]]%in%comp.chnl){

        orig <- .
        paste0(prefix, unlist(orig), suffix) %>% rbind(orig)
#       }else
#         .
    })
  
}

# copied from plyr to avoid the dependency on plyr
compact <- function (l) 
  Filter(Negate(is.null), l)