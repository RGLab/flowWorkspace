#' @include AllClasses.R
NULL

#' Open/Close a flowJo workspace
#' 
#' Open a flowJo workspace and return a \code{flowJoWorkspace} object.
#' Close a flowJoWorkspace, destroying the internal representation of the XML document, and freeing the associated memory.
#' 
#' @param file Full path to the XML flowJo workspace file.
#' @param options xml parsing options passed to \code{\link{xmlTreeParse}}
#' @param ... other arguments passed to \code{\link{xmlTreeParse}}
#' @param workspace A \code{flowJoWorkspace}
#' @details
#' 	Open an XML flowJo workspace file and return a \code{flowJoWorkspace} object. The workspace is represented using a \code{XMLInternalDocument} object.
#' 	Close a flowJoWorkpsace after finishing with it. This is necessary to explicitly clean up the C-based representation of the XML tree. (See the XML package).
#' @return  a \code{flowJoWorkspace} object.
#' @examples
#' \dontrun{
#' 	file<-"myworkspace.xml"
#' 	ws<-openWorkspace(file);
#' 	class(ws); #flowJoWorkspace
#' 	closeWorkspace(ws);
#' }
#' 
#' @importFrom XML xmlTreeParse xmlAttrs xmlGetAttr xmlTreeParse xmlRoot xmlValue xpathApply
#' 
#' @aliases openWorkspace
#' @rdname openWorkspace
#' @export 
setMethod("openWorkspace",signature=signature(file="character"),definition= function(file,options = 0,...){
 	#message("We do not fully support all features found in a flowJo workspace, nor do we fully support all flowJo workspaces at this time.")
	tmp<-tempfile(fileext=".xml")
    if(!file.exists(file))
      stop(file, " not found!")
	if(!file.copy(file,tmp))
      stop("Can't copy ", file, " to ", tmp)
    
	if(inherits(file,"character")){
		x<-xmlTreeParse(tmp,useInternalNodes=TRUE,options = options, ...);
	}else{
		stop("Require a filename of a workspace, but received ",class(x)[1]);
	}
	ver <- xpathApply(x,"/Workspace",function(x)xmlGetAttr(x,"version"))[[1]]
	x<-new("flowJoWorkspace",version=ver,.cache=new.env(parent=emptyenv()),file=basename(file),path=dirname(file),doc=x, options = as.integer(options))
	x@.cache$flag=TRUE;
	return(x);
})

#setAs("list", "GatingSet", function(from, to ){
#	if(!all(unlist(lapply(from,function(y)class(y)=="GatingHierarchy"),use.names=FALSE))){
#		stop("Can't coerce this list to class GatingSet");
#	}
#	new(to, set=from)
#})

setMethod("show",c("flowJoWorkspace"),function(object){
	cat("FlowJo Workspace Version ",object@version,"\n");
	cat("File location: ",object@path,"\n");
	cat("File name: ",object@file,"\n");
	if(object@.cache$flag){
		cat("Workspace is open.","\n");
		cat("\nGroups in Workspace\n");
        
        sg <- getSampleGroups(object)
		tbl<-table(Name=sg$groupName,GroupID=sg$groupID)
		print(data.frame(Name=rownames(tbl),"Num.Samples"=diag(tbl)))
	}else{	
		cat("Workspace is closed.","\n")
	}
})

#' @importFrom XML free
#' @rdname openWorkspace
#' @export
#' @aliases closeWorkspace
setMethod("closeWorkspace","flowJoWorkspace",function(workspace){
	free(workspace@doc);
	workspace@.cache$flag<-FALSE;
})


#' match version string to the support list
#' 
#' @param wsversion version string to match
#' 
#' @return the macthed workspace type
.getWorkspaceType <- function(wsversion){
  curSupport <- unlist(flowWorkspace.par.get("flowJo_versions"))
  ver_ind <- match(wsversion, curSupport)
  if(is.na(ver_ind))
    stop("Unsupported version: ", wsversion)
  else{
    wsType <- names(curSupport[ver_ind])  
  }
  gsub("[0-9]", "", wsType)
}

#' Parse a flowJo Workspace
#' 
#' Function to parse a flowJo Workspace, generate a \code{GatingHierarchy} or \code{GatingSet} object, and associated flowCore gates. The data are not loaded or acted upon until an explicit call to \code{recompute()} is made on the \code{GatingHierarchy} objects in the \code{GatingSet}.
#' @param obj A \code{flowJoWorkspace} to be parsed.
#' @param ...
#'      \itemize{
#'      	\item name \code{numeric} or \code{character}. The name or index of the group of samples to be imported. If \code{NULL}, the groups are printed to the screen and one can be selected interactively. Usually, multiple groups are defined in the flowJo workspace file.
#'      	\item execute \code{TRUE|FALSE} a logical specifying if the gates, transformations, and compensation should be immediately calculated after the flowJo workspace have been imported. TRUE by default. 
#'      	\item isNcdf \code{TRUE|FALSE} logical specifying if you would like to use netcdf to store the data, or if you would like to keep all the flowFrames in memory. For a small data set, you can safely set this to FALSE, but for larger data, we suggest using netcdf. You will need the netcdf C library installed.
#'      	\item subset \code{numeric} vector specifying the subset of samples in a group to import.
#'                        Or a \code{character} specifying the FCS filenames to be imported.
#'                        Or an \code{expression} to be passed to 'subset' function to filter samples by 'pData' (Note that the columns referred by the expression must also be explicitly specified in 'keywords' argument)  
#'      	\item requiregates \code{logical} Should samples that have no gates be included?
#'      	\item includeGates \code{logical} Should gates be imported, or just the data with compensation and transformation?
#'      	\item path either a \code{character} scalar or \code{data.frame}. When \code{character}, it is a path to the fcs files that are to be imported. The code will search recursively, so you can point it to a location above the files. 
#'                                                          When it is a \code{data.frame}, it is expected to contain two columns:'sampleID' and 'file', which is used as the mapping between 'sampleID' and FCS file (absolute) path. When such mapping is provided, the file system searching is avoided.
#'      	\item sampNloc a \code{character} scalar indicating where to get sampleName(or FCS filename) within xml workspace. It is either from "keyword" or "sampleNode". 

#'      	\item compensation=NULL: a \code{matrix} that allow the customized compensation matrix to be used instead of the one specified in flowJo workspace.    
#'      	\item options=0: a \code{integer} option passed to \code{\link{xmlTreeParse}}
#'          \item channel.ignore.case a \code{logical} flag indicates whether the colnames(channel names) matching needs to be case sensitive (e.g. compensation, gating..)
#'          \item extend_val \code{numeric} the threshold that determine wether the gates need to be extended. default is 0. It is triggered when gate coordinates are below this value.
#'          \item extend_to \code{numeric} the value that gate coordinates are extended to. Default is -4000. Usually this value will be automatically detected according to the real data range.
#'                                  But when the gates needs to be extended without loading the raw data (i.e. \code{execute} is set to FALSE), then this hard-coded value is used.
#'          \item leaf.bool a \code{logical} whether to compute the leaf boolean gates. Default is TRUE. It helps to speed up parsing by turning it off when the statistics of these leaf boolean gates are not important for analysis. (e.g. COMPASS package will calculate them by itself.)
#'                                           If needed, they can be calculated by calling \code{recompute} method at later stage.
#'          \item additional.keys \code{character} vector:  The keywords (parsed from FCS header) to be combined(concatenated with "_") with FCS filename
#'                                                          to uniquely identify samples. Default is '$TOT' (total number of cells) and more keywords can be added to make this GUID.
#'          \item keywords \code{character} vector specifying the keywords to be extracted as pData of GatingSet
#'          \item keywords.source \code{character} the place where the keywords are extracted from, can be either "XML" or "FCS"
#'          \item keyword.ignore.case a \code{logical} flag indicates whether the keywords matching needs to be case sensitive.    
#'      	\item ...: Additional arguments to be passed to \link{read.ncdfFlowSet} or \link{read.flowSet}.
#'      	}
#' @details
#' A flowJoWorkspace is generated with a call to \code{openWorkspace()}, passing the name of the xml workspace file. This returns a \code{flowJoWorkspace}, which can be parsed using the \code{parseWorkspace()} method. The function can be called non-interactively by passing the index or name of the group of samples to be imported via \code{parseWorkspace(obj,name=x)}, where \code{x} is either the numeric index, or the name. 
#' The \code{subset} argument allows one to select a set of files from the chosen sample group. The routine will take the intersection of the files in the sample group, the files specified in \code{subset} and the files available on disk, and import them. 
#' @return 
#'  a \code{GatingSet}, which is a wrapper around a list of \code{GatingHierarchy} objects, each representing a single sample in the workspace. The \code{GatingHierarchy} objects contain \code{graphNEL} trees that  represent the gating hierarchy of each sample. Each node in the \code{GatingHierarchy} has associated data, including the population counts from flowJo, the parent population counts, the \code{flowCore} gates generated from the flowJo workspace gate definitions. Data are not yet loaded or acted upon at this stage. To execute the gating of each data file, a call to \code{execute()} must be made on each \code{GatingHierarchy} object in the \code{GatingSet}. This is done automatically by default, and there is no more reason to set this argument to FALSE. 
#' @seealso \code{\link{getSampleGroups}},\code{\link{GatingSet}}
#' @examples
#' \dontrun{
#' 	 #f is a xml file name of a flowJo workspace
#' 	ws <- openWorkspace(f)
#'  #parse the second group
#' 	gs <- parseWorkspace(ws, name = 2); #assume that the fcs files are under the same folder as workspace
#' 
#'  
#'  gs <- parseWorkspace(ws, name = 4
#'                         , path = dataDir     #specify the FCS path 
#'                         , subset = "CytoTrol_CytoTrol_1.fcs"     #subset the parsing by FCS filename
#'                         , isNcdf = FALSE)#turn off cdf storage mode (normally you don't want to do this for parsing large dataset)
#' 
#'  
#' 
#'  gs <- parseWorkspace(ws, path = dataDir, name = 4
#'                          , keywords = c("PATIENT ID", "SAMPLE ID", "$TOT", "EXPERIMENT NAME") #tell the parser to extract keywords as pData
#'                          , keywords.source = "XML" # keywords are extracted from xml workspace (alternatively can be set to "FCS")
#'                          , additional.keys = c("PATIENT ID") #use additional keywords together with FCS filename to uniquely identify samples
#'                          , execute = F) # parse workspace without the actual gating (can save time if just want to get the info from xml)
#' 
#' #subset by pData (extracted from keywords)
#' gs <- parseWorkspace(ws, path = dataDir, name = 4
#'                          , subset = `TUBE NAME` %in% c("CytoTrol_1", "CytoTrol_2")
#'                          , keywords = "TUBE NAME")
#' }
#'
#' @aliases parseWorkspace
#' @rdname parseWorkspace
#' @export 
#' @importFrom utils menu
setMethod("parseWorkspace",signature("flowJoWorkspace"),function(obj, ...){
      .preprocessor(obj, ...)
    })
#' @importFrom dplyr group_by do %>%
.preprocessor <- function(obj, name = NULL
                              , subset = NULL
                              , requiregates = TRUE
                              , sampNloc = "keyword"
                              , additional.keys = "$TOT"
                              , keywords = NULL, keywords.source = "XML"
                              , execute = TRUE
                              , path = obj@path
                              , keyword.ignore.case = FALSE
                              , ...)
{	
    
    
    
    sampNloc <- match.arg(sampNloc, c("keyword", "sampleNode"))	
	
    if(is.element("ignore.case", names(list(...)))) 
      stop("ignore.case is deprecated by channel.ignore.case!")
  	x<-obj@doc;
  	
  	wsversion <- obj@version
    
    wsType <- .getWorkspaceType(wsversion)
    wsNodePath <- flowWorkspace.par.get("nodePath")[[wsType]]
    
    #sample info  
    allSamples <- .getSamples(x, wsType, sampNloc = sampNloc)
    # group Info
    g <- .getSampleGroups(x, wsType)
    # merge two
  	sg <- merge(allSamples, g, by="sampleID");
  	
      #requiregates - exclude samples where there are no gates? if(requiregates==TRUE)
  	if(requiregates){
  		sg <- sg[sg$pop.counts>0,]
  	}
  	
    # filter by group name
  	sg$groupName<-factor(sg$groupName)
  	groups<-levels(sg$groupName)
      
  	if(is.null(name)){
  	message("Choose which group of samples to import:\n");
      groupInd <- menu(groups,graphics=FALSE);
  	}else if(is.numeric(name)){
  		if(length(groups)<name)
  			stop("Invalid sample group index.")
  		groupInd <- name
  	}else if(is.character(name)){
  		if(is.na(match(name,groups)))
  			stop("Invalid sample group name.")
            groupInd <- match(name,groups)
  	}
    group.name <- groups[groupInd]
    
    subset <- substitute(subset)
    sg <- subset(sg, groupName == group.name)
    
    #parse the pData and apply the filter (subset)
    pd <- .parse.pData(obj = obj, keywords = keywords, sg = sg
                      , keywords.source = keywords.source
                      , execute = execute
                      , additional.keys = additional.keys
                      , path = path
                      , keyword.ignore.case = keyword.ignore.case
                      , subset = subset
                      , ... #arguments for read.FCSheader
                      )
    
	.parseWorkspace(xmlFileName=file.path(obj@path,obj@file)
                    , pd = pd 
                    ,xmlParserOption = obj@options
                    ,wsType=wsType
                    ,ws = obj
                    , sampNloc = sampNloc
                    , execute = execute
                    ,...)
		
}

.parse.pData <- function(obj, keywords, sg, keywords.source, execute, additional.keys, path, keyword.ignore.case, subset, emptyValue = TRUE
                          , ... #other arguments ignored
                         )
{
  
  #handle the non-expression 'subset' (e.g. character, numerical or logical index)
  subset_evaluated <- try(eval(subset,  parent.frame(n = 2)), silent = TRUE) #eval it outside of pd first
  if(class(subset_evaluated) != "try-error" && !is.null(subset_evaluated)){
    # when factor convert it to character
    if(is.factor(subset_evaluated)){
      subset_evaluated <- as.character(subset_evaluated)
    }
    # when character, evaluate it as sample names
    # and convert it to logical vector
    if(is.character(subset_evaluated)){
      subset_evaluated <- eval(substitute(name %in% s, list(s = subset_evaluated)), sg)
    }
    #do the filtering
    if(is.numeric(subset_evaluated)||is.logical(subset_evaluated))
      sg <- sg[subset_evaluated, ,drop = FALSE]
    else
      stop("Unsupported 'subset' type!")
    
    #set it to NULL to avoid repeatedly evaluating later
    subset_evaluated <- NULL
  }
  nSample <- nrow(sg)
  if(nSample == 0)
    stop("No samples in this workspace to parse!")  
  
  #check duplicated sample keys
  isDup <- duplicated(sg[["guid"]])
  if(any(isDup))
    stop("Duplicated sample names detected within group: ", paste(sg[["guid"]][isDup], collapse = " "), "\n Please check if the appropriate group is selected.")
  
  isDup <- duplicated(sg[["sampleID"]])
  if(any(isDup))
    stop("duplicated samples ID within group: ", paste(sg[["sampleID"]][isDup], collapse = " "))
  
  
  searchByKeyword <- TRUE #a deprecated argument
  
  #use other keywords in addition to name to uniquely identify samples 
  if(!is.null(additional.keys)){
    sg[["guid"]] <- as.vector(apply(sg, 1, function(row){
              sampleID <- as.numeric(row[["sampleID"]])
              sn <- row[["name"]]
              kw <- unlist(getKeywords(obj, sampleID)[additional.keys])
              paste(c(sn,kw), collapse = "_")
            }))  
  }else
    sg[["guid"]] <- sg[["name"]]
  
  samples <- sg[,c("name", "sampleID", "guid")]
  
  keywords.source <- match.arg(keywords.source, c("XML", "FCS"))
  
  #matching FCS files to the samples of xml
  if(execute)
  {
    
    
    if(is.data.frame(path)){
      #use the file path provided by mapping
      
      #validity check for path
      mapCols <- c("sampleID", "file")
      if(!setequal(colnames(path), c("sampleID", "file")))
        stop("When 'path' is a data.frame, it must contain columns: ", paste(dQuote(mapCols), collapse = ","))
      if(class(path[["sampleID"]])!="numeric")
        stop("'sampleID' column in 'path' argument must be numeric!")
      
      path[["file"]] <- as.character(path[["file"]])
      
      samples <- merge(samples, path, by = "sampleID")
      if(nrow(samples) > 0)
        samples[["nFound"]] <- 1
      else
        stop("No samples in this workspace to parse!")
      
    }else{
      
      #search file system to resolve the file path
      key.env <- new.env(parent = emptyenv())
      samples <- samples %>%
          group_by(guid) %>%
          do({
                
                filename <- .[["name"]]
                guid <- .[["guid"]]
                sampleID <- .[["sampleID"]]
                
                #########################################################
                #get full path for each fcs
                #########################################################
#                                  browser()
                ##escape "special" characters
                charToEsc <- c("?","]","-","+",")","(") %>% 
                    paste0("\\", .) %>%  #prepend \\
                    paste(collapse = "|") %>%# concatenate them
                    paste0("(", . , ")") #construct pattern 
                
                
                
                #try to search by file name first
                filename.pattern <- gsub(charToEsc, "\\\\\\1", filename)
                absPath <- list.files(pattern=paste("^",filename.pattern,"",sep=""),path=path,recursive=TRUE,full.names=TRUE)
                nFound <- length(absPath)
                isFileNameSearchFailed <- nFound == 0
                #searching file by keyword $FIL when it is enabled
                if(isFileNameSearchFailed && searchByKeyword){
                  
                  #read FCS headers if key.fils  has not been filled yet
                  if(is.null(key.env[["key.fils"]])){
                    all.files <- list.files(pattern= ".fcs",path=path,recursive=TRUE,full.names=TRUE)  
                    if(length(all.files) > 0)
                      key.env[["key.fils"]] <- read.FCSheader(all.files, keyword = "$FIL", emptyValue = emptyValue)
                  }
                  key.fils <- key.env[["key.fils"]]
                  #$FIL is optional keyword according to FCS3.1 standard
                  #so some FCS may not have this thus need to remove NA entries
                  key.fils <- key.fils[!is.na(key.fils)] 
#                                    message(filename," not found in directory: ",path,". Try the FCS keyword '$FIL' ...")  
                  absPath <- names(key.fils[key.fils == filename])
                  nFound <- length(absPath)
                  
                }
                
                if(nFound >= 1){
                  #try to use additional keywords to further prune the matching results
                  if(!is.null(additional.keys))
                  {
                    guids.fcs <-  sapply(absPath, function(thisPath){
                          # get keyword from FCS header
                          kws <- as.list(read.FCSheader(thisPath, emptyValue = emptyValue)[[1]])
                          kw <- trimws(unlist(kws[additional.keys]))
                          # construct guids
                          thisFile <- ifelse(isFileNameSearchFailed, kws["$FIL"], basename(thisPath))
                          paste(c(thisFile, as.vector(kw)), collapse = "_")
                        }, USE.NAMES = F)  
                    matchInd <- which(guid == guids.fcs) #do strict matching instead grep due to the special characters
                    nFound <- length(matchInd)
                    if(nFound == 1)
                      absPath <- absPath[matchInd]
                  }
                }
                
                if(nFound != 1)
                  absPath <- ""
                
                cbind(., data.frame(file = absPath, nFound = nFound, stringsAsFactors = FALSE))
              })
    }
    
  }else{
    samples[["file"]] <- ""
    samples[["nFound"]] <- 1
  }
  
  
  
  if(!is.null(keywords)){
    #somehow dplyr doesn't like rownames and dplyr::select won't drop the first column(which might be on purpose since it could be used as index internally)
    #and these should be fine since we need to preserve guid column and convert it to rownames later on anyway (deu to the legacy code) 
    pd <- samples %>% 
        group_by(guid) %>% 
        do({
              if(keywords.source == "XML")
              {  #parse pData from workspace
                kws <- getKeywords(obj, .[["sampleID"]]) #whitespace is already removed
              }else{
                if(execute){
                  #skip those non-unique matched samples
                  if(.[["nFound"]] == 1){
                    #parse pData by reading the fcs headers
                    kws <- read.FCSheader(.[["file"]], emptyValue = emptyValue)[[1]] %>% trimws %>% as.list 
                  }else
                    kws <- NULL
                  
                }else
                  stop("Please set 'execute' to TRUE in order to parse pData from FCS file!")
              }
              if(is.null(kws))
                data.frame()
              else{
                keynames <- names(kws)
                if(keyword.ignore.case)
                  keyInd <- match(tolower(keywords), tolower(keynames))
                else
                  keyInd <- match(keywords, keynames)
                key.na <- is.na(keyInd)
                if(any(key.na))
                  warning("keyword not found in ", .[["guid"]], ": ", paste(sQuote(keywords[key.na]), collapse = ","))
                
                
                
                kw <- kws[keyInd] %>% as.list %>% `names<-`(value = keywords) %>% 
                    lapply(function(i)ifelse(is.null(i), NA, i)) %>% #replace NULL with NA
                    data.frame(check.names = F, stringsAsFactors = F)
                cbind(., kw)
              }
              
              
            })
    
  }else{
    pd <- samples
  }
  
  #handle 'subset' as an expression
  if(!is.null(subset_evaluated)){ #check if it is already evaluated
    if(class(subset_evaluated) == "try-error"){#if fails, then try it as an expression to be evaluated within pData
      message("filter samples by pData...")
      subset_evaluated <- eval(subset,  pd)  
      pd <- pd[subset_evaluated, ]
    }  
  }  
  
  #check if there are samples to parse
  missingInd <- pd[["nFound"]] == 0
  nMissing <- sum(missingInd)
  if(nMissing > 0){
    warning("Can't find the FCS files for the following samples:\n", pd[missingInd, ][["guid"]] %>% paste(collapse = "\n"))
  }
  
  dupInd <- pd[["nFound"]] > 1
  nDup <- sum(dupInd)
  if(nDup > 0){
    warning("Multiple FCS files are matched to these samples:\n", pd[nDup, ][["guid"]] %>% paste(collapse = "\n"))
  }
  
  pd <- pd[!(missingInd|dupInd),]
  
  
  pd <- droplevels(pd)
  nSample <- nrow(pd)
  
  
  
  message("Parsing ", nSample," samples");
  
  return(pd)
}
.parseWorkspace <- function(xmlFileName, execute, isNcdf = TRUE, includeGates = TRUE,sampNloc="keyword",xmlParserOption, wsType, ws, pd, ...){
  
  
#	message("calling c++ parser...")
  
  
  gs <- GatingSet(x = xmlFileName
      , y = as.character(pd[["sampleID"]])
      , guids = pd[["guid"]]
      , includeGates = includeGates
      , sampNloc = sampNloc
      , xmlParserOption = xmlParserOption
      , wsType = wsType
  )
  
#	message("c++ parsing done!")
  
  #gating
  gs <- .addGatingHierarchies(gs,pd,execute,isNcdf, wsType = wsType, sampNloc = sampNloc, ws = ws, ...)
  

  #attach pData
  rownames(pd) <- pd[["guid"]] 
  pd[["guid"]] <- NULL
  #remove temporary columns
  pd[["sampleID"]] <- NULL
  pd[["nFound"]] <- NULL
  pd[["file"]] <- NULL
  class(pd) <- "data.frame"
  pData(gs) <- pd
    
  message("done!")

  # It is probably unnecessary since the previous check on
  # the $TOT keywords (through default 'keywords' argument) should be sufficient

  #compare the root counts between fc and fj                  
  #double check if the correct raw file is used 
#   if(execute){
#     invisible(lapply(gs, function(gh){
#       
#       sn <- sampleNames(gh)
#       fj.count <- as.integer(getTotal(gh, "root", flowJo = T))
#       fc.count <- as.integer(getTotal(gh, "root", flowJo = F))
#       
#       if(fj.count == -1){
#         warning("root count for flowJo is not available: ", sn)
#       }else{
#         if(fj.count != fc.count)
#           stop("Total event counts mismatched between flowJo and flowCore!", sn)  
#       }        
#       
#     }))  
#   }
  
  
  #we don't want to return the splitted gs since they share the same cdf and externalptr
  #thus should be handled differently(more efficiently) from the regular gslist
  
#    # try to post process the GatingSet to split the GatingSets(based on different the gating trees) if needed                
  gslist <- suppressMessages(.groupByTree(gs))
  if(length(gslist) > 1)
    warning("GatingSet contains different gating tree structures and must be cleaned before using it! ")
#    if(length(gslist) == 1){
#      return (gslist[[1]])      
#    }else
  {
#      warning("Due to the different gating tree structures, a list of GatingSets is returned instead!")
#      return (gslist)
  }
  gs
}


getFileNames <- function(ws){
  if(class(ws)!="flowJoWorkspace"){
    stop("ws should be a flowJoWorkspace")
  }else{
    x <- ws@doc
    wsversion <- ws@version
    wsType <- .getWorkspaceType(wsversion)
    .getFileNames(x, wsType)
  }
}
.getFileNames <- function(x, wsType){
  wsNodePath <- flowWorkspace.par.get("nodePath")[[wsType]]
  unlist(xpathApply(x, file.path(wsNodePath[["sample"]], "Keywords/Keyword[@name='$FIL']")
                    , function(x)xmlGetAttr(x,"value")
                    )
        ,use.names=FALSE
        )

}
#' make a formula from a character vector
#' 
#' construct a valid formula to be used by flowViz::xyplot 
#' 
#' @param dims a \code{character} vector that contains y , x axis, if it is unnamed, then treated as the order of c(y,x)
#' @param isChar \code{logical} flag indicating whehter to return a formula or a pasted string
#' @return when \code{isChar} is TRUE, return a character, otherwise coerce it as a \code{formula}
#' @examples 
#' all.equal(mkformula(c("SSC-A", "FSC-A")),`SSC-A` ~ `FSC-A`)#unamed vecotr
#' all.equal(mkformula(c(x = "SSC-A", y = "FSC-A")),`FSC-A` ~ `SSC-A`)#named vector
#' @export 
mkformula<-function(dims,isChar=FALSE){
	if(length(dims)==1){
		form<-paste(c("",sapply((dims), function(x) paste("`",x, "`", sep = ""))), collapse = "~")
	}else{
      
        dnames <- names(dims)
        if(!is.null(dnames)){
          if(isTRUE(all.equal(sort(dnames), c("x", "y"))))
            dims <-  dims[rev(order(names(dims)))]
          else
            warning("invalid axis names: ", paste(dnames, collapse = ","), "(expect 'x' or 'y')")
        }
		form <- paste(sapply((dims),function(x)paste("`",x,"`",sep="")),collapse="~")
	}
	if(!isChar)
		form<-as.formula(form)	
	return(form)
}





#' Get Keywords
#' 
#' Retrieve keywords associated with a workspace
#' 
#' @param obj A \code{flowJoWorkspace}
#' @param y c\code{character} or \code{numeric} specifying the sample name or sample ID
#' @param ... other arguments
#'      sampNloc a \code{character} the location where the sample name is specified. See \code{parseWorkspace} for more details.
#' 
#' @details
#'   Retrieve a list of keywords from a \code{flowJoWorkspace}  
#' @return A list of keyword - value pairs. 
#' @examples
#'   require(flowWorkspaceData)
#'   d<-system.file("extdata",package="flowWorkspaceData")
#'   wsfile<-list.files(d,pattern="manual.xml",full=TRUE)
#'   ws <- openWorkspace(wsfile);
#'   
#'   getSamples(ws)
#'   res <- try(getKeywords(ws,"CytoTrol_CytoTrol_1.fcs"), silent = TRUE)
#'   print(res[[1]])
#'   getKeywords(ws, 1)
#' @aliases getKeywords
#' @rdname getKeywords
#' @export 
setMethod("getKeywords",c("flowJoWorkspace","character"),function(obj,y, ...){
      if(length(y) > 1)
        stop("getKeywords can only work with one sample at a time!")
      x <- obj@doc
      wsversion <- obj@version
      wsType <- .getWorkspaceType(wsversion)
      wsNodePath <- flowWorkspace.par.get("nodePath")[[wsType]]
      .getKeywordsBySampleName(obj@doc,y, wsNodePath[["sample"]], ...)
})
#' @rdname getKeywords
#' @export 
setMethod("getKeywords",c("flowJoWorkspace","numeric"),function(obj,y, ...){
      if(length(y) > 1)
        stop("getKeywords can only work with one sample at a time!")
      x <- obj@doc
      wsversion <- obj@version
      wsType <- .getWorkspaceType(wsversion)
      wsNodePath <- flowWorkspace.par.get("nodePath")[[wsType]]
      .getKeywordsBySampleID(obj@doc,y, wsNodePath[["sampleID"]],...)
    })

.getKeywordsBySampleID <- function(obj, sid, sampleIDPath = "/Workspace/SampleList/Sample"){
  
  xpath <- sprintf(paste0(sampleIDPath,"[@sampleID='%s']"),sid)
  
  if(basename(sampleIDPath)!= "Sample")
    xpath <- file.path(xpath, "..")
  
  sampleNode <- xpathApply(obj, xpath)
  if(length(sampleNode) == 0)
    stop("sampleID ", sid, " not found!")
  
  kw <- xpathApply(sampleNode[[1]], "Keywords/Keyword", function(i){
                                      attr <- xmlAttrs(i)
                                      attrValue <- attr[["value"]]
                                      names(attrValue) <- attr[["name"]]
                                      trimWhiteSpace(attrValue)
                                    })
  as.list(unlist(kw))
  
}
.getKeywordsBySampleName <- function(doc, y, samplePath = "/Workspace/SampleList/Sample", sampNloc = "keyword"){
  sampNloc <- match.arg(sampNloc, c("keyword", "sampleNode"))  
  
  if(sampNloc == "keyword"){
    w <- which(xpathApply(doc, file.path(samplePath, "Keywords/Keyword[@name='$FIL']"),function(x)xmlGetAttr(x,"value"))%in%y)
    if(length(w)==0)
      warning("Sample name ", y," not found in Keywords of workspace")
  }else{
    w <- which(xpathApply(doc, file.path(samplePath,"SampleNode") ,function(x)xmlGetAttr(x,"name"))%in%y)
    if(length(w)==0)
      warning("Sample name ", y," not found in SampleNode of workspace")
  }
  
  if(length(w)==0){
    ##Use the DataSet tag to locate the sample
    w <- which(xpathApply(doc, file.path(samplePath,"DataSet") ,function(x)xmlGetAttr(x,"uri"))%in%y)
    if(length(w)==0)
      stop("failed to locate Sample ",y," within workspace")      
  }
  
  if(length(w) > 1)
    stop("Character '", y, "' can't uniquely identify sample within the workspace!Try to set 'y' to a numeric sampleID instead.")
  l <- xpathApply(doc,paste(samplePath,"[",w,"]/Keywords/node()",sep=""),xmlAttrs)
  names(l) <- lapply(l,function(x)x[["name"]])
  l <- lapply(l,function(x)x[["value"]])
  return(l)
}

.getKeyword <- function(ws,x, samplePath = "/Workspace/SampleList/Sample"){
	if(inherits(ws,"flowJoWorkspace")&class(x)=="character"){
		 unlist(xpathApply(ws@doc,paste(file.path(samplePath, "Keywords/Keyword[@name='"), x, "']",sep=""),function(x)xmlGetAttr(x,"value")),use.names=FALSE)
	}else{
		stop("No such keyword")
	}
	
}

#' Fetch the indices for a subset of samples in a flowJo workspace, based on a keyword value pair
#'
#' Returns an index vector into the samples in a flowJo workspace for use with parseWorkspace(subset=), based on a keyword/value filter in a specific group of samples.
#'
#' @description 
#' This function will calculate the indices of a subset of samples in a flowJoWorkspace, based on a keyword/value filter. It is applied to a specific group of samples in the workspace. The output is meant to be passed to the subset= argument of parseWorkspace.
#' 
#' @param ws \code{flowJoWorkspace} object
#' @param key \code{character} The name of the keyword. 
#' @param value \code{character} The value of the keyword.
#' @param group \code{numeric} The group of samples to subset. 
#' @param requiregates \code{TRUE} or \code{FALSE}, specifying whether we include only samples that have gates attached or whether we include any sample in the workspace.
#' 
#' @return A numeric vector of indices.
#' @seealso \code{\link{parseWorkspace}}
#' @export 
getFJWSubsetIndices<-function(ws,key=NULL,value=NULL,group,requiregates=TRUE){
	if(!is.numeric(group)){
		stop("group must be numeric")
	}
	if(!is.character(key)&!is.null(key)){
		stop("keyword must be character")
	}
	if(!is.character(value)&!is.null(value)){
		stop("value must be character")
	}
	if(!class(ws)=="flowJoWorkspace"){
		stop("ws must be a flowJoWorkspace object")
	}
    x <- ws@doc
    wsversion <- ws@version
    wsType <- .getWorkspaceType(wsversion)
    wsNodePath <- flowWorkspace.par.get("nodePath")[[wsType]]
    
	s<- .getSamples(x, wsType);
	#TODO Use the actual value of key to name the column
	if(!is.null(key)){
	s$key <- .getKeyword(ws,key, wsNodePath[["sample"]])
	}
	g<- .getSampleGroups(x, wsType)
	sg<-merge(s,g,by="sampleID")
	if(requiregates){
	sg<-sg[sg$pop.counts>0,]
	}
	sg$groupName<-factor(sg$groupName)
	groups<-levels(sg$groupName)
	if(group>length(groups)){
		stop("group is invalid, out of range")
	}
	sg<-sg[sg$groupName%in%groups[group],]
	if(!is.null(key)&!is.null(value)){
	return(which(sg$key%in%value))
	}
	return(sg)
}




#Taken from limma (don't want to import and create a dependency)
trimWhiteSpace<-function (x) 
{
    sub("[ \t\n\r]*$", "", sub("^[ \t\n\r]*", "", x))
}

#' Get a list of samples from a flowJo workspace
#' 
#' Return  a data frame of samples contained in a flowJo workspace
#' @param x A \code{flowJoWorkspace}
#' @param sampNloc \code{character} either "keyword" or "sampleNode". see \link{parseWorkspace}
#' @details
#' Returns a \code{data.frame} of samples in the \code{flowJoWorkspace}, including their 
#' \code{sampleID}, \code{name}, and \code{compID} (compensation matrix ID). 
#' 
#' @return 
#' A \code{data.frame} with columns \code{sampleID}, \code{name}, and \code{compID} if \code{x} is a \code{flowJoWorkspace}.
#' 
#' @examples
#'       \dontrun{
#'         #ws is a flowJoWorkspace
#'         getSamples(ws);
#'       }
#' @aliases getSamples
#' @rdname getSamples
#' @export  
setMethod("getSamples","flowJoWorkspace",function(x, sampNloc="keyword"){
      sampNloc <- match.arg(sampNloc, c("keyword", "sampleNode"))
      
      wsversion <- x@version
      wsType <- .getWorkspaceType(wsversion)
      x <- x@doc
      .getSamples(x, wsType = wsType, sampNloc = sampNloc)
})

#' Get a table of sample groups from a flowJo workspace
#' 
#'   Return a data frame of sample group information from a flowJo workspace
#' @param x A \code{flowJoWorkspace} object.
#' @details Returns a table of samples and groups defined in the flowJo workspace
#' @return  
#'   A \code{data.frame} containing the \code{groupName}, \code{groupID}, and \code{sampleID} for each sample in the workspace. Each sample may be associated with multiple groups.
#' @seealso \code{\link{flowJoWorkspace-class}} \code{\link{openWorkspace}}
#' 
#' @examples
#'   \dontrun{
#'     #ws is a flowJoWorkspace
#'     getSampleGroups(ws);
#'   }
#' @aliases getSampleGroups
#' @rdname getSampleGroups
#' @export 
setMethod("getSampleGroups","flowJoWorkspace",function(x){
            wsversion <- x@version      
            x <- x@doc
            wsType <- .getWorkspaceType(wsversion)
			.getSampleGroups(x, wsType = wsType)
		})

#' @importFrom stats na.omit
.getSampleGroups<-function(x, wsType){
  
  wsNodePath <- flowWorkspace.par.get("nodePath")[[wsType]]
  
	if(grepl("mac", wsType)){
		do.call(rbind,xpathApply(x, wsNodePath[["group"]],function(x){
  
							gid <- c(xmlGetAttr(x, wsNodePath[["attrName"]])
                                  ,xmlGetAttr(x,"groupID")
                                  )
							sid <- do.call(c,xpathApply(x, wsNodePath[["sampleRef"]],function(x){
												as.numeric(xmlGetAttr(x,"sampleID"))
											}))
							if(is.null(sid)){
								sid<-NA;
							}

							groups<-na.omit(data.frame(groupName=gid[[1]]
                                                      ,groupID=as.numeric(gid[2])
                                                      ,sampleID=as.numeric(sid)
                                                      )
                                            );
						}))
	}else{
		#Note that groupID is from order of groupNode instead of from xml attribute 
		counter <- 1
		do.call(rbind,xpathApply(x, wsNodePath[["group"]],function(x){

							gid <- c(xmlGetAttr(x, wsNodePath[["attrName"]])
                                  , xmlGetAttr(x,"groupID")
                                  )
							sid <- do.call(c,xpathApply(x, wsNodePath[["sampleRef"]],function(x){
												as.numeric(xmlGetAttr(x,"sampleID",default=NA))
											}))
							if(is.null(sid)){
								sid<-NA;
							}

							groups<-na.omit(data.frame(groupName=gid[[1]]
                                                      ,groupID=counter
                                                      ,sampleID=as.numeric(sid)
                                                      )
                                              )
							counter <- counter+1
							groups
						}))
	}
}



.getSamples<-function(x, wsType, sampNloc="keyword"){
    
    wsNodePath <- flowWorkspace.par.get("nodePath")[[wsType]]
	lastwarn<-options("warn")[[1]]
	options("warn"=-1)
	top <- xmlRoot(x)
    
    
	s <- do.call(rbind,xpathApply(top, file.path(wsNodePath[["sample"]],wsNodePath[["sampleNode"]]),function(x){
        		    attrs <- xmlAttrs(x);
        	        data.frame(tryCatch(as.numeric(attrs[["sampleID"]]), error=function(x)NA)
                                ,tryCatch(attrs[[wsNodePath[["attrName"]]]], error=function(x)NA)
                                 ,tryCatch(as.numeric(attrs[["count"]]), error=function(x)NA)
                                )
		    }))
    
	pop.counts <- as.numeric(unlist(lapply(xpathApply(top,wsNodePath[["sample"]])
                                            ,function(x)xpathApply(x,"count(descendant::Population)")
                                          )
                                      ,use.names=FALSE
                                      )
                                )
	if(grepl("mac", wsType)){
		cid<-as.numeric(paste(xpathApply(top, wsNodePath[["sample"]], function(x)xmlGetAttr(x,"compensationID"))))
		s<-data.frame(s,cid,pop.counts)
		colnames(s)<-c("sampleID","name","count","compID","pop.counts");
	}else{
		##Code for flowJo windows 1.6 xml
		#No compensation ID for windows. Use name
		s<-data.frame(s,pop.counts)
		colnames(s)<-c("sampleID","name","count","pop.counts");
	}
	s[,2]<-as.character(s[,2])
    #update sample name when it is supposed to be fetched from keyword
    if(sampNloc == "keyword"){
      sn <- xpathApply(top, file.path(wsNodePath[["sample"]], "Keywords/Keyword[@name='$FIL']"), function(x){
            xmlAttrs(x)[["value"]]
          })
      
      if(length(sn) == 0)
        stop("sample name is not found in 'keyword' node!Please try 'sampleName' node by setting sampNloc = 'sampleName")
      s[, "name"] <- unlist(sn)   
    }
    
	options("warn"=lastwarn);
	s
}








