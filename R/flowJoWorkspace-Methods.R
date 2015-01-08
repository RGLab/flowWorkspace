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
#'      	\item subset \code{numeric} vector specifying the subset of samples in a group to import. Or a \code{character} specifying the FCS filenames to be imported.
#'      	\item requiregates \code{logical} Should samples that have no gates be included?
#'      	\item includeGates \code{logical} Should gates be imported, or just the data with compensation and transformation?
#'      	\item path either a \code{character} scalar or \code{data.frame}. When \code{character}, it is a path to the fcs files that are to be imported. The code will search recursively, so you can point it to a location above the files. 
#'                                                          When it is a \code{data.frame}, it is expected to contain two columns:'sampleID' and 'file', which is used as the mapping between 'sampleID' and FCS file (absolute) path. When such mapping is provided, the file system searching is avoided.
#'      	\item sampNloc a \code{character} scalar indicating where to get sampleName(or FCS filename) within xml workspace. It is either from "keyword" or "sampleNode". 

#'      	\item compensation=NULL: a \code{matrix} that allow the customized compensation matrix to be used instead of the one specified in flowJo workspace.    
#'      	\item options=0: a \code{integer} option passed to \code{\link{xmlTreeParse}}
#'          \item ignore.case a \code{logical} flag indicates whether the colnames(channel names) matching needs to be case sensitive (e.g. compensation, gating..)
#'          \item extend_val \code{numeric} the threshold that determine wether the gates need to be extended. default is 0. It is triggered when gate coordinates are below this value.
#'          \item extend_to \code{numeric} the value that gate coordinates are extended to. Default is -4000. Usually this value will be automatically detected according to the real data range.
#'                                  But when the gates needs to be extended without loading the raw data (i.e. \code{execute} is set to FALSE), then this hard-coded value is used.
#'          \item leaf.bool a \code{logical} whether to compute the leaf boolean gates. Default is TRUE. It helps to speed up parsing by turning it off when the statistics of these leaf boolean gates are not important for analysis. (e.g. COMPASS package will calculate them by itself.)
#'                                           If needed, they can be calculated by calling \code{recompute} method at later stage.
#'          \item additional.keys \code{character} vector:  By default, FCS filename is used to uniquely identify samples. When filename is not sufficient to serve as guid, parser can optionally combine it with other keywords (parsed from FCS header) to make guid (concatenated with "_").    
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
#' 	ws<-openWorkspace(f)
#' 	G<-parseWorkspace(ws,execute=TRUE,isNcdf=FALSE,path="."); #assume that the fcs files are below the current directory.
#' 	#G is a GatingSet.
#' 	G1<-parseWorkspace(ws)
#' 	#G1 is a GatingSet.
#' }
#'
#' @aliases parseWorkspace
#' @rdname parseWorkspace
#' @export 
#' @importFrom utils menu
setMethod("parseWorkspace",signature("flowJoWorkspace"),function(obj, ...){
      .preprocessor(obj, ...)
    })

.preprocessor <- function(obj, name = NULL
                              , subset = NULL
                              , requiregates = TRUE
                              , sampNloc = "keyword"
                              , additional.keys = NULL
                              , ...)
{	
    sampNloc <- match.arg(sampNloc, c("keyword", "sampleNode"))	
	
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
    sg <- subset(sg, groupName == group.name)
#    browser()    
    #filter by subset (sample name or numeric index)
	if(is.factor(subset)){
		subset<-as.character(subset)
	}
	if(is.character(subset)){
          sg <- subset(sg, name %in% subset)
	}else if(is.numeric(subset))
      sg <- sg[subset, ] 

    #check if there are samples to parse
    
    nSample <- nrow(sg)
    if(nSample == 0)
      stop("No samples in this workspace to parse!")
    
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
    
    
    #check duplicated sample keys
    isDup <- duplicated(sg[["guid"]])
    if(any(isDup))
      stop("Duplicated sample names detected within group: ", paste(sg[["guid"]][isDup], collapse = " "), "\n Please check if the appropriate group is selected.")

    
	message("Parsing ", nSample," samples");
	.parseWorkspace(xmlFileName=file.path(obj@path,obj@file)
                    , samples = sg[,c("name", "sampleID", "guid")]
                    ,xmlParserOption = obj@options
                    ,wsType=wsType
                    ,ws = obj
                    , sampNloc = sampNloc
                    , additional.keys = additional.keys
                    ,...)
		
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
      s[, "name"] <- unlist(sn)   
    }
    
	options("warn"=lastwarn);
	s
}








