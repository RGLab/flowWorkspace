## Store state info in this internal global environment
flowWorkspace.state <- new.env(hash = FALSE)
flowWorkspace.state[["par"]] <- list()

#' workspace version is parsed from xml node '/Workspace/version' in flowJo workspace
#' and matched with this list to dispatch to the one of the three workspace parsers 
flowWorkspace.par.init <- function(){
  
  fj_ver <- list(win = c("1.61", "1.6")
                , macII = c("2.0")
                , macIII = c("3.0")
                , vX = c("1.8", "20.0")
                )
                          
   mac_II_path <- list(group = "/Workspace/Groups/GroupNode"# abs path
                    , sampleRef = ".//SampleRef"#relative GroupNode
                    , sample = "/Workspace/SampleList/Sample"#abs path
                    , sampleNode = "./SampleNode"#relative to sample
                    , popNode = "./Population"#relative to sampleNode
                    , attrName = "name"
                    , compMatName = "name"
                    , compMatChName = "name"
                    , compMatVal = "value"
                    )
  mac_II_path[["sampleID"]] <- mac_II_path[["sample"]]
                    
  #mac version 3.0 (flowJo version 9.7.2-9.7.4)
  mac_III_path <- mac_II_path
  mac_III_path[["sample"]] <- sub("SampleList", "Samples", mac_III_path[["sample"]]) 
  mac_III_path[["attrName"]] <- "nodeName"
  mac_III_path[["compMatName"]] <- "matrixName"
  mac_III_path[["compMatChName"]] <- "fluorName"
  mac_III_path[["compMatVal"]] <- "spillValue"
  mac_III_path[["sampleID"]] <- mac_III_path[["sample"]]
  ####windows version
  #inherit most paths from mac                                      
  win_path <- mac_II_path
  win_path[["popNode"]] <- "./*/Population"
  win_path[["gateDim"]] <- "*[local-name()='dimension']"#relative to gateNode
  win_path[["gateParam"]] <- "*[local-name()='parameter']"#relative to dimNode
  win_path[["sampleID"]] <- file.path(win_path[["sample"]],"DataSet")

  ####version X
  #inherit most paths from win
  vX_path <- win_path
  vX_path[["gateParam"]] <- "*[local-name()='fcs-dimension']";                                        
  
  flowWorkspace.state[["par"]] <- list(flowJo_versions = fj_ver 
                                      , nodePath = list(win = win_path
                                                        , macII = mac_II_path
                                                        , macIII = mac_III_path
                                                        , vX = vX_path
                                                        )
                                      , theme.novpadding = list(layout.heights = list(top.padding = 0,
                                                                                       main.key.padding = 0,
                                                                                       key.axis.padding = 0,
                                                                                       axis.xlab.padding = 0,
                                                                                       xlab.key.padding = 0,
                                                                                       key.sub.padding = 0,
                                                                                       bottom.padding = 0)
                                                                 , layout.widths = list(left.padding = 0,
                                                                                        key.ylab.padding = 0,
                                                                                        ylab.axis.padding = 0,
                                                                                        axis.key.padding = 0,
                                                                                        right.padding = 0)
                                                                 , par.xlab.text = list(cex = 0.7, col = gray(0.3))
                                                                 , par.ylab.text = list(cex = 0.7,  col = gray(0.3))
                                                                 , par.main.text = list(cex = 0.8)
                                                                 , axis.components = list(bottom = list(tck =0.5)
                                                                                          , left = list(tck =0.5))
                                                                 , axis.text = list(cex = 0.5)
                                                                )
                                      , plotGate = list(arrange = TRUE)
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

