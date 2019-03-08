#' @include GatingHierarchy_Methods.R
NULL

#' determine the flow data associated with a Gating Hiearchy is based on `ncdfFlowSet` or `flowSet`
#'
#' @param x \code{GatingHiearchy} object
#' @return \code{logical}
#' @export
isNcdf <- function(x){
#			browser()

  return (class(flowData(x))=="ncdfFlowSet")

}

#' constructors for GatingSet
#'
#' construct object from existing gating hierarchy(gating template) and flow data 
#'
#' @param path \code{character} specifies the path to the flow data (FCS files)
#' @param ... other arguments. see \link{parseWorkspace}
#' @rdname GatingSet-methods
#' @export
setMethod("GatingSet", c("GatingHierarchy", "character"), function(x, y, path="."
																	, swap_cols = FALSE #for diva parsing
																	, ...){
            
			samples <- y
			dataPaths <- vector("character")
			excludefiles <- vector("logical")
			for(file in samples){
#				browser()
				#########################################################
				#get full path for each fcs and store in dataPath slot
				#########################################################
				##escape "illegal" characters
				file<-gsub("\\)","\\\\)",gsub("\\(","\\\\(",file))
				absPath<-list.files(pattern=paste("^",file,"$",sep=""),path=path,recursive=TRUE,full.names=TRUE)

				if(length(absPath)==0){
					warning("Can't find ",file," in directory: ",path,"\n");
					excludefiles<-c(excludefiles,TRUE);

				}else{
					dataPaths<-c(dataPaths,dirname(absPath[1]))
					excludefiles<-c(excludefiles,FALSE);
				}
			}
			#Remove samples where files don't exist.
			if(length(which(excludefiles))>0){
				message("Removing ",length(which(excludefiles))," samples from the analysis since we can't find their FCS files.");
				samples<-samples[!excludefiles];
			}


			files<-file.path(dataPaths,samples)
			
			message("generating new GatingSet from the gating template...")
			
			#load new data
      		cs <- load_cytoset_from_fcs(files, is_h5 = TRUE, ...)
			cols.old <- colnames(cs)
			cols <- swap_data_cols(cols.old, swap_cols)#validity check
			if(!all(cols==cols.old))
			{
				#can't assign cols directly due to the backend implemented as std::unordered_map
				for(c1 in names(swap_cols))
				{
					c2 <- swap_cols[[c1]]
					cs_swap_colnames(cs, c1, c2)					
				}
				
			}
			gs <- new("GatingSet", pointer = .cpp_NewGatingSet(x@pointer,sampleNames(x), cs@pointer))
			
            message("done!")
			return(gs)
		})

#' Swap the colnames
#' Perform some validity checks before returning the updated colnames
#'  
#' @param cols the original colname vector
#' @param swap_cols a named list specifying the pairs to be swapped
#' @return the new colname vector that has some colnames swapped
#' @export 
#' @examples 
#' data(GvHD)
#' fr <- GvHD[[1]]
#' colnames(fr)
#' new <- swap_data_cols(colnames(fr), list(`FSC-H` = "SSC-H", `FL2-H` = "FL2-A"))
#' colnames(fr) <- new
swap_data_cols <- function(cols, swap_cols)
{
	if(!is.null(swap_cols))
		if(!isFALSE(swap_cols) && length(swap_cols) > 0) 
		{
			left <- names(swap_cols)
			right <- as.vector(unlist(swap_cols))
			
			update <- FALSE
			for(left in names(swap_cols))
			{
				right <- swap_cols[[left]]
				
				lidx <- match(left, cols)
				ridx <- match(right, cols)
				
				if(!is.na(lidx) && !is.na(ridx))
				{
					if(length(lidx) > 1) 
						stop("Multiple cols matched to ", left)
					if(length(ridx) > 1)
						stop("Multiple cols matched to ", right)
					message("swap cols: ", left, ":", right)  
					
					cols[c(lidx, ridx)]  <- c(right, left)
				}
			}
		}
	cols
}

#' 1. loads the raw data (when execute == TRUE)
#' 2. compensate and transform the data (when execute == TRUE)
#' 3. transform gates(extend and apply gains when applicable)
#' 4. compute the stats (when execute == TRUE)
#'
#' @param prefix a \code{logical} flag indicates whether the colnames needs to be updated with prefix(e.g. "<>" or "comp") specified by compensations
#' @param channel.ignore.case a \code{logical} flag indicates whether the colnames(channel names) matching needs to be case sensitive (e.g. compensation, gating..)
#' @param extend_val \code{numeric} the threshold that determine wether the gates need to be extended. default is 0. It is triggered when gate coordinates are below this value.
#' @param extend_to \code{numeric} the value that gate coordinates are extended to. Default is -4000. Usually this value will be automatically detected according to the real data range.
#'                                  But when the gates needs to be extended without loading the raw data (i.e. \code{execute} is set to FALSE), then this hard-coded value is used.
#' @param transform \code{logical} to enable/disable transformation of gates and data. Default is TRUE. It is mainly for debug purpose (when the raw gates need to be parsed.
#' @importFrom Biobase AnnotatedDataFrame
#' @noRd 
.addGatingHierarchies <- function(gs, samples, execute,isNcdf = TRUE
                                      ,compensation=NULL
							  		, transformation = NULL
							  			,wsType = ""
                                      , extend_val = 0, extend_to = -4000
                                      , prefix = TRUE, channel.ignore.case = FALSE
                                      , ws = NULL, leaf.bool = TRUE, sampNloc = "keyword"
                                      ,  transform = TRUE, timestep.source = c("TIMESTEP", "BTIM")
									  , swap_cols = FALSE #for diva parsing
							  , ...){
  timestep.source  <- match.arg(timestep.source )
  if(nrow(samples)==0)
    stop("no sample to be added to GatingSet!")
  
  guids <- samples[["guid"]]
  
  if(!is.null(compensation)){
	  if(is(compensation, "matrix"))
		  compensation <- compensation(compensation)
    #replicate the single comp 
    if(is(compensation, "compensation")){
      compensation <- sapply(guids, function(guid)compensation, simplify = FALSE)   
    }else{
      if(is.list(compensation)){
        if(!all(guids %in% names(compensation)))
          stop("names of the compensation list must match the 'guids' of samples!")
      }else
        stop("'compensation' should be either a compensation object of a list of compensation objects!")
    }
     
  }
  if(!is.null(transformation))
  {
  
	  if(is(transformation, "transformerList"))
	  	transformation <- sapply(guids, function(guid)transformation, simplify = FALSE)
	
	  if(!is.list(transformation))
		  stop("'transformation' should be either a transformerList object of a list of transformerList objects!")
	  if(!all(guids %in% names(transformation)))
		  stop("names of the transformation list must match the 'guids' of samples!")
  }
  

  #sample names are supplied explicitly through phenoData to optionally use the names other than the original file names
  pd <- AnnotatedDataFrame(data = data.frame(name = samples[["name"]]
                                             ,row.names = guids
                                             ,stringsAsFactors=FALSE
                                            )
                          ,varMetadata = data.frame(labelDescription="Name",row.names="name")
                          )

  #load the raw data from FCS
	if(execute)
	{
		if(isNcdf){
			stopifnot(length(grep("ncdfFlow",loadedNamespaces()))!=0)
			message("Creating ncdfFlowSet...")

			fs <- read.ncdfFlowSet(samples[["file"]],isWriteSlice=FALSE, phenoData = pd, ...)
		}else{
			message("Creating flowSet...")

      #read.flowSet would ignore given file names and use sampleNames(pd) if phenoData is given
      #so we have to modify pd afterwards
			fs <- read.flowSet(samples[["file"]], ...)
			sampleNames(fs) <- guids
		}
	}else{
      #create dummy flowSet
      frList <- sapply(guids, function(thisSample){
                        mat <- matrix(data = numeric(0))
                        colnames(mat) <- "FSC-A"
                        fr <- suppressWarnings(flowFrame(exprs = mat))

                      })
      fs <- flowSet(frList)
    }

	#global variable storing prefixed colnames
    tempenv<-new.env(parent = emptyenv())
    prefixColNames <- NULL
    assign("prefixColNames",NULL,tempenv)

	axis <- apply(samples,1,function(row,tempenv){

        sampleID <- as.numeric(row[["sampleID"]])
        guid <- row[["guid"]]
        #get global variable
        prefixColNames <- tempenv$prefixColNames
        comp_param_ind <- tempenv$comp_param_ind


        # get comp
        comp <- .cpp_getCompensation( gs@pointer, guid)
        cid <- comp$cid


        ##################################
    #Compensating the data
    ##################################
    if(execute)
		{
      file <- row[["file"]]
      cnd <- colnames(fs)
			message("loading data: ",file);

			if(isNcdf)
				data <- read.FCS(file, ...)[, cnd]
			else
				data <- fs[[guid]]
			
		cols <- swap_data_cols(colnames(data), swap_cols)
		if(!all(cols==colnames(data)))
			colnames(data) <- cols
      #alter colnames(replace "/" with "_") for flowJo X
      #record the locations where '/' character is detected and will be used to restore it accurately
      slash_loc <- sapply(cnd, function(thisCol)as.integer(gregexpr("/", thisCol)[[1]]), simplify = FALSE)
      if(wsType == "vX"){
        new_cnd <- fix_channel_slash(cnd, slash_loc)
          if(!all(new_cnd == cnd)){ #check if needs to update colnames to avoid unneccessary expensive colnames<- call
            cnd <- new_cnd
            colnames(data) <- cnd

          }

      }

      compensation <- compensation[[guid]]
	  transformation <- transformation[[guid]]
	  
	  
			if(cid=="")
				cid <- ifelse(is.null(compensation), "-2", "1")
				

			if(cid!="-1" && cid!="-2"){
				message("Compensating");

				marker <- comp$parameters

				if(is.null(compensation)){
                  
                  ## try to match marker from comp with flow data in case flowJo is not consistent with data
                  if(channel.ignore.case)
                    markerInd <- match(tolower(marker), tolower(cnd))
                  else
                    markerInd <- match(marker, cnd)
                  
                  if(any(is.na(markerInd)))
                    stop("channels mismatched between compensation and flow data!") 

                  marker <- cnd[markerInd]

                  compobj <- compensation(matrix(comp$spillOver,nrow=length(marker),ncol=length(marker),byrow=TRUE,dimnames=list(marker,marker)))
                }else
					compobj <- compensation#TODO: to update compensation information in C part
				#TODO this compensation will fail if the parameters have <> braces (meaning the data is stored compensated).
				#I need to handle this case properly.

                res <- try(compensate(data,compobj),silent=TRUE)
				if(inherits(res,"try-error")){
					message("Data is probably already compensated");
				}else{
					data <- res
					rm(res);
				}

			}
			else if(cid=="-2"){
				#TODO the matrix may be acquisition defined.
				message("No compensation");
			}
			else if(cid=="-1")
			{
				##Acquisition defined compensation.
				nm <- comp$comment


				if(grepl("Acquisition-defined",nm)){
					###Code to compensate the sample using the acquisition defined compensation matrices.
					message("Compensating with Acquisition defined compensation matrix");
					#browser()
					if(is.null(compensation))
					{
						compobj <- compensation(spillover(data)$SPILL)

					}else
					{
						compobj <- compensation

					}

					res <- try(compensate(data,compobj),silent=TRUE)

					if(inherits(res,"try-error")){
						message("Data is probably already compensated");
					}else{
						data<-res
						rm(res);

					}

				}

			}
       }else{
         # get kw from ws (We are not sure yet if this R API will always
         # return keywords from workspace successfully, thus it is currently
         # only used when execute = FALSE
         if(!is.null(ws))
           kw <- getKeywords(ws, sampleID)
         #use $PnB to determine the number of parameters since {N,R,S) could be
         #redundant in some workspaces
         key_names <- unique(names(kw[grep("\\$P[0-9]{1,}B", names(kw))]))
         key_names <- gsub("B", "N", key_names, fixed = TRUE)
         cnd <- as.vector(unlist(kw[key_names]))

       }

       ##################################
       #alter the colnames
       ##################################
  		if(cid!="-2")
  		{

            #get prefix if it is not set yet
            if(is.null(prefixColNames)&&prefix){

              if(is.null(cnd)){
                cnd <- as.vector(parameters(data)@data$name)
              }
              prefixColNames <- cnd
              if(execute)
                comp_param <- parameters(compobj)
              else
                comp_param <- comp$parameters

              comp_param_ind <- match(comp_param, prefixColNames)

              prefixColNames[comp_param_ind] <- paste(comp$prefix,comp_param,comp$suffix,sep="")



            }
        }else{
          prefixColNames <- cnd
          comp_param_ind <- seq_along(cnd)
        }
          ##################################
          #transforming and gating
          ##################################
        gains <- numeric()#gain is no longer relevant
        names(gains) <- character()
          if(execute)
          {

			  #transform with external trans when applicable
			  if(!is.null(transformation))
			  {
				  message(paste("transform the data with supplied transformations ..."))
				  transformation <- transformList(names(transformation), lapply(transformation, function(x)x[["transform"]]))
				  data <- transform(data, transformation)
			  }
				    
            message(paste("gating ..."))
            #stop using gating API of cdf-version because c++ doesn't store the view of ncdfFlowSet anymore
            mat <- data@exprs #using @ is faster than exprs()
            
            #get gains from keywords
            # # for now we still parse it from data
            # # once confirmed that workspace is a reliable source for this info
            # # we can parse it from ws as well
            this_pd <- pData(parameters(data))
            # #skip time channel since the time channel of gates are already stored at gained scale (instead of raw scale)
            time.ind <- grepl("time", this_pd[["name"]], ignore.case = TRUE)
            # this_pd <- subset(this_pd, !time.ind)
            # paramIDs <- rownames(this_pd)
            # key_names <- paste(paramIDs,"G",sep="")
            kw <- keyword(data)
            # if(as.numeric(kw[["FCSversion"]])>=3&&wsType!="vX"){
            #   kw_gains <- kw[key_names]
            # 
            #   # For keywords where the gain is not set, the gain is NULL.
            #   # We replace these instances with the default of 1.
            #   kw_gains[sapply(kw_gains, is.null)] <- 1
            # 
            #   gains <- as.numeric(kw_gains)
            # }else{
            #   gains <- rep(1,length(paramIDs))
            # }
            # 
            # names(gains) <- this_pd$name
            # gains <- gains[gains != 1]#only pass the valid gains to save the unnecessary computing
        
            #update colnames in order for the gating to find right dims
            if(!is.null(prefixColNames)){
              dimnames(mat) <- list(NULL, prefixColNames)
            }

            recompute <- !transform #recompute flag controls whether gates and data need to be transformed
            nodeInd <- 0

            if(any(time.ind)){
              time.range <- range(mat[, time.ind])
              timestep <- compute_timestep(kw, time.range, timestep.source  = timestep.source) #timestep is used to convert time channel to seconds
            }else
              timestep <- 1

			
            .cpp_gating(gs@pointer, mat, guid, gains, nodeInd, recompute, extend_val, channel.ignore.case, leaf.bool, timestep)
#            browser()
            #restore the non-prefixed colnames for updating data in fs with [[<-
            #since colnames(fs) is not udpated yet.
            if(!is.null(prefixColNames)){
              #restore the orig colnames(replace "_" with "/") for flowJo X
              if(wsType == "vX"){
                #use slash locations to avoid tamper the original '_' character in channel names
                old_cnd <- fix_channel_slash(cnd, slash_loc)

                if(!all(old_cnd == cnd)){ #check if needs to update colnames to avoid unneccessary expensive colnames<- call
                  cnd <- old_cnd
                  colnames(data) <- cnd #restore colnames for flowFrame as well for flowJo vX
                }

              }
              dimnames(mat) <- list(NULL, cnd)
            }

            data@exprs <- mat #circumvent the validity check of exprs<- to speed up

            if(isNcdf){
              fs[[guid]] <- data

            }else{
              assign(guid,data,fs@frames)
            }
            #range info within parameter object is not always the same as the real data range
            #it is used to display the data.
            #so we need update this range info by transforming it
            tInd <- grepl("[Tt]ime",cnd)
            if(any(tInd))
              tRg  <- range(mat[,tInd])
            else
              tRg <- NULL
            axis.labels <- .transformRange(gs,guid,wsType,fs@frames,timeRange = tRg, slash_loc, compChnlInd = comp_param_ind)

		}else{
          #extract gains from keyword of ws
          #currently it is only used for extracting gates without gating
          #In future we want to use it for gating as well
          #once we have confirmed that ws is a reliable source of keyword
          #EDIT: Acutally we've already found one workspace from PROVIDE study
          #that does not contain the gain keyword for all channels. So the ws is not reliable source of keyword
          #EDIT: gain is no longer needed #213

          # gains <- rep(1,length(cnd))#init with default 1
          # 
          # #get gains from keywords
          # kw_gains <- grep("P[0-9]{1,}G", names(kw))
          # 
          # if(length(kw_gains) > 0){
          #   key_names <- unique(names(kw[kw_gains]))
          #   kw_gains <- kw[key_names]
          # 
          #   # Sometimes the keywords where the gain is not set, the gain value is NULL.
          #   # We replace these instances with the default of 1.
          #   kw_gains[sapply(kw_gains, is.null)] <- 1
          # 
          #   #update the default gain values
          #   #extract numeric index from channels (Not every channel necessarily has its gain keyword stored in xml)
          #   found_gain_chnl_ind <-  as.numeric(gsub('G$', "", gsub('^\\$P', "", key_names)))
          #   gains[found_gain_chnl_ind] <- as.numeric(kw_gains)
          # }
          # 
          # 
          # 
          # 
          # names(gains) <- prefixColNames
          # gains <- gains[gains != 1]#only pass the valid gains to save the unnecessary computing
          #transform and adjust the gates without gating
          if(transform)
            .cpp_computeGates(gs@pointer, guid, gains, extend_val, extend_to)
          axis.labels <- list()
        }

        #set global variable
        tempenv$prefixColNames <- prefixColNames
        tempenv$comp_param_ind <- comp_param_ind

        #return axis.labels
        axis.labels
	},tempenv)

    names(axis) <- guids
    gs@axis <- axis
    gs@flag <- execute #assume the excution would succeed if the entire G gets returned finally

	if(execute)
	{
#		browser()
        #sync channel info for gates and comps ,trans
        if(channel.ignore.case){
          #get non-prefixed channel names
          raw.cols <- colnames(fs)
          #since updateChannels does case insensitive matching
          #so we simply set both old and new columns with raw.cols
          map <- data.frame(old = raw.cols, new = raw.cols)
          updateChannels(gs, map, all = FALSE)
        }

		#update data with prefixed columns
		#can't do it before fs fully compensated since
		#compensate function check the consistency colnames between input flowFrame and fs
		if(!is.null(tempenv$prefixColNames))
          colnames(fs) <- tempenv$prefixColNames


		#attach filename and colnames to internal stucture for gating
#		browser()
	}

    flowData(gs) <- fs
    if(!is.null(compensation))
      gs@compensation <- compensation[guids] #append the customized compensations provided outside of xml
  	if(!is.null(transformation))
	  gs@transformation <- transformation[guids]
	gs
}

#' compute time step from fcs keyword
#' 
#' @param kw list of keywords
#' @param unit.range the actual measured time unit range
#' @param timestep.source either "TIMESTEP" or "BTIM". prefer to $TIMESTEP keyword when it is non NULL
#' @export  
compute_timestep <- function(kw, unit.range, timestep.source  = c("TIMESTEP", "BTIM")){
  timestep.source  <- match.arg(timestep.source )
  #check if $TIMESTEP is available
  kw.ts <- kw[["$TIMESTEP"]]
  if(is.null(kw.ts))
    timestep.source <- "BTIM"

  if(timestep.source  == "TIMESTEP")
    as.numeric(kw.ts)
  else{

    btime <- kw[["$BTIM"]]
    etime <- kw[["$ETIM"]]
    if(is.null(btime)||is.null(etime))
      return(1)
    else{
      #prefix the 4th section . (replace :) so that strptime recognize the fractional seconds in the input string
      terms <- rep('([0-9]{2})', 4)
      pat <- paste(terms, collapse = ":")
      pat <- paste0("^", pat, "$")
      rep <- "\\1:\\2:\\3\\.\\4"
      etime <- sub(pat, rep, etime)
      btime <- sub(pat, rep, btime)
      format <- "%H:%M:%OS"

      time.total <- difftime(strptime(etime, format), strptime(btime, format), units = "secs")
      as.numeric(time.total)/diff(unit.range)

    }
  }
}
#' toggle the channel names between '/' and '_' character
#'
#' FlowJoX tends to replace '/' in the original channel names with '_' in gates and transformations.
#' We need to do the same to the flow data but also need to change it back during the process since
#' the channel names of the flowSet can't be modified until the data is fully compensated.
#' @param chnls the channel names
#' @param slash_loc a list that records the locations of the original slash character within each channel name
#'                  so that when restoring slash it won't tamper the the original '_' character.
#' @return the toggled channel names
#' @importFrom stringr str_sub str_sub<-
#' @export
fix_channel_slash <- function(chnls, slash_loc = NULL){
  mapply(chnls, slash_loc, FUN = function(thisCol, this_slash){
        toggleTo <- ifelse(grepl("/", thisCol), "_", "/")
        #replace each individual /|_ based on their detected location
        if(any(this_slash > 0))
        {
          for(this_loc in this_slash)
            str_sub(thisCol, this_loc, this_loc) <- toggleTo
        }

        thisCol
      }
      , USE.NAMES = FALSE)

}

#' transform the range slot and construct axis label and pos for the plotting
#' @param G \code{GatingSet} It is an incomplete GatingSet object which does not have data slot assigned yet.
#' @param wsType \code{character} flowJo workspace type
#' @param frmEnv \code{environment} point to the \code{frames} slot of the original \code{flowSet}
#' @param timeRange \code{numeric} vector specifying the range for 'time' channel
#'
#' @return
#' a \code{list} of axis labels and positions. Also, the \code{range} slot of \code{flowFrame} stored in \code{frmEnv} are transformed as an side effect.
#' @noRd 
.transformRange <- function(G,sampleName, wsType,frmEnv, timeRange = NULL, slash_loc = NULL, compChnlInd){


 trans <- getTransformations(G[[sampleName]], channel = "all")
 comp<-.cpp_getCompensation(G@pointer,sampleName)
 prefix <- comp$prefix
 suffix <- comp$suffix
  fr <- frmEnv[[sampleName]]
	rawRange <- range(fr)
    oldnames <- names(rawRange)

    if(wsType == "vX"&&!is.null(slash_loc)){
      names(rawRange) <- fix_channel_slash(oldnames, slash_loc)

    }
	tempenv<-new.env()
	assign("axis.labels",list(),envir=tempenv);

    trans_names <-trimws(names(trans))

	datarange <- sapply(1:dim(rawRange)[2],function(i){
               thisRange <- rawRange[i]
               this_chnl <- names(thisRange)
               if(i%in%compChnlInd)
                 this_chnl <- paste(prefix,this_chnl,suffix,sep="")

               #have to do strict match for vX since trans functions can be defined for both compensated and uncompensated channel
              j <- match(this_chnl, trans_names)
              isMatched <- !is.na(j)

				if(isMatched){

					rw <- thisRange[,1]
                    thisTrans <- trans[[j]]
                    typeAttr <- attr(thisTrans, "type")
					if(is.null(typeAttr)||typeAttr!="gateOnly"){
						r <- thisTrans(rw)
					}else{
						r <- rw
					}
					###An unfortunate hack. If we use the log transformation, then negative values are undefined, so
					##We'll test the transformed range for NaN and convert to zero.
					r[is.nan(r)] <- 0

					###Is this transformed?
					if(!all(rw==r)){


						######################################
						#pretty ticks by equal interval at raw scale
						######################################
						base10raw <- unlist(lapply(2:6,function(e)10^e))
						base10raw <- c(0,base10raw)
						raw <- base10raw[base10raw>=min(rw)&base10raw<max(rw)]
            if(length(raw) == 0)
              raw <- base10raw #hack to prevent it from error in the new line below
						pos <- signif(thisTrans(raw))


						assign("this_chnl",this_chnl,tempenv)
						assign("raw",raw,tempenv);
						assign("pos",pos,tempenv);
						eval(expression(axis.labels[[this_chnl]]<-list(label=as.character(raw),at=pos)),envir=tempenv);
					}
					return(r);
				}else{

                  #update time range with the real data range
                  if(grepl("[Tt]ime",this_chnl))
                  {
                    timeRange
#                    range(exprs(dataenv$data$ncfs[[sampleName]])[,this_chnl])
                  }else{
                    thisRange[,1]
                  }

				}
			})


	datarange <- t(rbind(datarange[2,]-datarange[1,],datarange))
	pData(parameters(fr))[,c("range","minRange","maxRange")] <- datarange
	description(fr) <- flowCore:::updateTransformKeywords(fr)
	frmEnv[[sampleName]] <- fr

    tempenv$axis.labels
}

#' Plot gates and associated cell population contained in a \code{GatingHierarchy} or \code{GatingSet}
#'
#' \strong{Important}: The \code{plotGate} methods are now defunct and gates should instead be plotted using the 
#' \code{\link[ggcyto]{autoplot}} method from the \code{ggcyto} package. The \code{plotGate} documentation has been 
#' left here to ease the transition. \cr\cr
#' When applied to a \code{GatingHierarchy},\code{arrange} is set as TRUE, then all the gates associated with it are plotted as different panel on the same page.
#' If \code{arrange} is FALSE, then it plots one gate at a time.
#' By default ,\code{merge} is set as TRUE, plot multiple gates on the same plot when they share common parent population and axis.
#' When applied to a \code{GatingSet}, if lattice is TRUE,it plots one gate (multiple samples) per page , otherwise, one sample (with multiple gates) per page.

#' @param x \code{\linkS4class{GatingSet}} or \code{\linkS4class{GatingHierarchy}}object
#' @param y \code{character} the node name or full(/partial) gating path
#'          or \code{numeric} representing the node index in the \code{GatingHierarchy}.
#'          or \code{missing} which will plot all gates and one gate per page. It is useful for generating plots in a multi-page pdf.
#'          Nodes can be accessed with \code{\link{getNodes}}.
#' @param ...
#' \itemize{
#'  \item{bool}{ \code{logical} specifying whether to plot boolean gates.}
#'  \item{arrange.main}{ \code{character} The title of the main page of the plot. Default is the sample name. Only valid when \code{x} is GatingHierarchy}
#'  \item{arrange}{ \code{logical} indicating whether to arrange different populations/nodes on the same page via \code{arrangeGrob} call.}
#'  \item{merge}{ \code{logical} indicating whether to draw multiple gates on the same plot if these gates share the same parent population and same x,y dimensions/parameters;}
#' \item{projections}{ \code{list} of character vectors used to customize x,y axis. By default, the x,y axis are determined by the respective gate parameters.
#'                                 The elements of the list are named by the population name or path (see \code{y}). Each element is a pair of named character specifying the channel name(or marker name) for x, y axis.
#'                                 Short form of channel or marker names (e.g. "APC" or "CD3") can be used as long as they can be uniquely matched to the dimentions of flow data.
#'                                 For example, projections = list("lymph" = c(x = "SSC-A", y = "FSC-A"), "CD3" = c(x = "CD3", y = "SSC-A"))
#'                      }
#' \item{par.settings}{ \code{list} of graphical parameters passed to \code{\link{lattice}};}
#'
#'  \item{gpar}{ \code{list} of grid parameters passed to \code{\link{grid.layout}};}
#'
#'  \item{lattice}{ \code{logical} deprecated;}
#'
#'  \item{formula}{ \code{formula} a formula passed to \code{xyplot} function of \code{flowViz}, by default it is NULL, which means the formula is generated according to the x,y parameters associated with gate.}
#'
#'  \item{cond}{ \code{character} the conditioning variable to be passed to lattice plot.}
#'
#'  \item{overlay}{Node names. These populations are plotted on top of the existing gates(defined by \code{y} argument) as the overlaid dots.}
#'  \item{overlay.symbol}{A named (lattice graphic parameter) list that defines the symbol color and size for each overlaid population.
#'                         If not given, we automatically assign the colors.}
#'  \item{key}{Lattice legend paraemter for overlay symbols.}
#'
#'  \item{default.y}{ \code{character} specifiying y channel for xyplot when plotting a 1d gate. Default is "SSC-A" and session-wise setting can be stored by 'flowWorkspace.par.set("plotGate", list(default.y = "FSC-A"))'}
#'
#'  \item{type}{ \code{character} either "xyplot" or "densityplot". Default is "xyplot"  and session-wise setting can be stored by 'flowWorkspace.par.set("plotGate", list(type = "xyplot"))'}
#'
#'  \item{fitGate}{ used to disable behavior of plotting the gate region in 1d densityplot. Default is FALSE and  session-wise setting can be stored by 'flowWorkspace.par.set("plotGate", list(fitGate = FALSE))'}
#'
#'  \item{strip}{ \code{ligcal} specifies whether to show pop name in strip box,only valid when x is \code{GatingHierarchy}}
#' \item{strip.text}{either "parent" (the parent population name) or "gate "(the gate name).}
#'
#'
#'  \item{raw.scale}{ \code{logical} whether to show the axis in raw(untransformed) scale. Default is TRUE and can be stored as session-wise setting by 'flowWorkspace.par.set("plotGate", list(raw.scale = TRUE))'}
#'  \item{xlim, ylim}{ \code{character} can be either "instrument" or "data" which determines the x, y axis scale
#'                                            either by instrument measurement range or the actual data range.
#'                     or \code{numeric} which specifies customized range.
#'                      They can be stored as session-wise setting by 'flowWorkspace.par.set("plotGate", list(xlim = "instrument"))'
#' }
#'
#'  \item{...}{
#'
#'          path A \code{character} or \code{numeric} scalar passed to \link{getNodes} method (used to control how the gating/node path is displayed)
#'
#'          ... The other additional arguments to be passed to \link[flowViz]{xyplot}.
#'          }
#' }
#'
#' @return  a \code{trellis} object if \code{arrange} is \code{FALSE},
#' @references \url{http://www.rglab.org/}
#' @examples \dontrun{
#' 	#G is a GatingHierarchy
#' 	plotGate(G,getNodes(G)[5]);#plot the gate for the  fifth node
#' }
#' @aliases
#' plotGate
#' plotGate-methods
#' plotGate,GatingHierarchy,character-method
#' plotGate,GatingHierarchy,numeric-method
#' plotGate,GatingHierarchy,missing-method
#' plotGate,GatingSet,numeric-method
#' plotGate,GatingSet,character-method
#' plotGate,GatingSet,missing-method
#'
#' @rdname plotGate-methods-defunct
setMethod("plotGate",signature(x="GatingSet",y="missing"),function(x,y,...){
  .Defunct("ggcyto::autoplot", "flowWorkspace")
})


setMethod("plotGate",signature(x="GatingSet",y="numeric"),function(x,y,...){
  .Defunct("ggcyto::autoplot", "flowWorkspace")
})

setMethod("plotGate",signature(x="GatingSet",y="character"),function(x,y,lattice=TRUE,bool=FALSE,merge=TRUE,...){
  .Defunct("ggcyto::autoplot", "flowWorkspace")
})

##recursively parsing conditional variables
#' @noRd 
.parseCond<-function(cond){
#			browser()
  groupBy<-NULL
  if(length(cond)==1)
    groupBy<-as.character(cond)
  else
  {
    for(i in 1:length(cond))
    {
      curCond<-cond[[i]]
#				browser()
      if(length(curCond)==3)
      {
        res<-.parseCond(curCond)
        groupBy<-c(res,groupBy)
      }else
      {
        curCond<-as.character(curCond)
        if(!curCond%in%c(":","*","+"))
          groupBy<-c(groupBy,curCond)
      }

    }
  }

  groupBy
}
# naive formula parser to extract basic compenents (like x,y,groupBy)
.formulaParser <- function(formula)
{
#	browser()

  #parse the b term
  bTerm<-formula[[3]]
  cond<-NULL
  if(length(bTerm)>2)
  {
    xTerm<-bTerm[[2]]
    cond<-bTerm[[3]]
  }else
  {
    xTerm<-bTerm
  }
#	browser()
  ##parse the conditional variable
  if(!is.null(cond))
  {
    groupBy<-.parseCond(cond)

  }else
  {
    groupBy<-NULL
  }

  #parse the xterm
  xfunc<-NULL
  if(length(xTerm)==2)
  {
    xfunc<-xTerm[[1]]
    xTerm<-xTerm[[2]]
  }else
  {
    if(length(xTerm)>=3)
      stop("not supported formula!")
  }


  yTerm<-formula[[2]]
  yfunc<-NULL
  if(length(yTerm)==2)
  {
    yfunc<-yTerm[[1]]
    yTerm<-yTerm[[2]]
  }else
  {
    if(length(yTerm)>=3)
      stop("not supported formula!")
  }

  list(xTerm=xTerm,yTerm=yTerm,xfunc=xfunc,yfunc=yfunc,groupBy=groupBy)
}

fix_y_axis <- function(gs, x, y){
  chnls <- colnames(getData(gs))
  y.candidates <- chnls[-match(x,chnls)]

  if(y%in%y.candidates)
    yParam <- y
  else{
    if(!y %in% chnls)
      stop("default y '", y, "' is not valid channel name!Try to reset it")
    #pick other channel for y axis
    y.candidates <- y.candidates[!grepl("[Tt]ime", y.candidates)]
    yParam <- y.candidates[1]
    warning("Y axis is set to '", yParam, "' because default y '", y, "' can not be used as y axis!\n To eliminate this warning, change the default y channel")
  }
  return(yParam)
}

#' @param x a \code{GatingSet}
#' @param overlay a list of gate indices or event indices (named by sampleNames(x))
#' @param ... other arguments
#'      params channel names for subsetting the result
#' @return either a list of flowFrame for a flowSet/ncdfFlowSet or NULL
#' @noRd 
.getOverlay <- function(x, overlay, ...){
  .Defunct(msg = "getOverlay is defunct!")
  myfunc <- function(x, overlay, params){
    #gate indices
    if(class(overlay)=="logical")
      overlay<-Subset(getData(x),overlay)[,params]
    else{
#      if(length(overlay)>1)
#        stop("only one overlay gate can be added!In order to visualize multiple overlays,try to add a booleanGate first.")
      overlay <- sapply(overlay, function(thisOverlay)getData(x,thisOverlay)[,params])
    }
    overlay
  }


  if(!is.null(overlay))
  {

    if(is.list(overlay)){
      #if overlay is a list, then extract overlay from each element
      samples <- sampleNames(x)
      if(isTRUE(all.equal(names(overlay), samples))){
        overlay <-  sapply(samples, function(sn)myfunc(x[[sn]], overlay[[sn]], ...))
      }else
        stop("names of overlay list does not agree with sampleNames in GatingSet!")
    }else{
      overlay <-  myfunc(x, overlay, ...)
    }


  }
  overlay
}

#' return the range of one channel of flowSet
#'
#' @param fs \code{flowSet} or \code{ncdfFlowSet}
#' @param channel \code{character} channel name
#' @return a numerical range
#' @noRd 
.getRange <- function(fs, channel) {

  thisMin <- .Machine$double.xmax
  thisMax <- .Machine$double.xmin
  for (i in sampleNames(fs)) {
    e <- exprs(fs[[i, channel]])
    thisRange <- range(e)
    thisMin <- min(thisMin,thisRange[1])
    thisMax <- max(thisMax,thisRange[2])
  }

  return(c(thisMin, thisMax))
}

#'  clone a GatingSet
#'
#'   clone a GatingSet
#' @param x A \code{GatingSet}
#' @param ...
#'     h5_dir = tempdir() the directory to store the h5-based flow data matrix
#' @details
#'   Note that the regular R assignment operation on a \code{GatingSet} object does not return the copy as
#'   one would normally expect because the \code{GatingSet} contains environment slots (and external pointer for \code{GatingSet}),
#'   which require deep-copying. So make sure to use this clone method in order to make a copy of existing object.
#' @return A copy of a given \code{GatingSet}.
#' @examples
#'   \dontrun{
#'     #gs is  a GatingSet
#'     gs2 <-gs_clone(gs) #gs2 is independent from gs and have its own copy of both gating trees and flow data
#'     gs3 <- gs_copy_tree_only(gs) #gs3 has its own copy of gating trees but share the same flow data with original gs
#'   }
#' @aliases clone clone-methods clone,GatingSet-method
#' @exportMethod clone
#' @rdname gs_clone
setGeneric("clone", function(x,...)standardGeneric("clone"))
setMethod("clone",c("GatingSet"),function(x, ...){
      .Deprecated("gs_clone")
      gs_clone(x, ...)
    })

#' @rdname gs_clone
#' @export 
gs_clone <- function(x, h5_dir = tempdir()){
  new("GatingSet", pointer = .cpp_CloneGatingSet(x@pointer, h5_dir, is_copy_data = TRUE))
  
}

#' @rdname gs_clone
#' @export 
gs_copy_tree_only <- function(x){
  new("GatingSet", pointer = .cpp_CloneGatingSet(x@pointer, h5_dir = "", is_copy_data = FALSE))
  
}

setGeneric("recompute", function(x,...)standardGeneric("recompute"))
#' Compute the cell events by the gates stored within the gating tree.
#'
#' Compute each cell event to see if it falls into the gate stored within the gating tree
#' and store the result as cell count.
#'
#' It is usually used immediately after \link{add} or \link{setGate} calls.
#'
#' @param x \code{GatingSet}
#' @param y \code{character} node name or node path. Default "root". Optional.
#' @param alwaysLoadData \code{logical}. Specifies whether to load the flow raw data for gating boolean gates. Default 'FALSE'. Optional. Sometime it is more efficient to skip loading the raw data if all the reference nodes and parent are already gated. 'FALSE' will check the parent node and reference to determine whether to load the data.
#' This check may not be sufficient since  the further upstream ancestor nodes may not be gated yet.
#' In that case, we allow the gating to fail and prompt user to recompute those nodes explictily.
#'  When TRUE, then it forces data to be loaded to guarantee the gating process to be uninterrupted at the cost of unnecessary data IO.
#' @param ... other arguments
#'              leaf.bool whether to compute the leaf boolean gate, default is TRUE
#' @aliases recompute
#' @rdname recompute
#' @export
setMethod("recompute",c("GatingSet"),function(x, y="root",alwaysLoadData=FALSE, ...){
			.recompute(x,y=y,alwaysLoadData=alwaysLoadData, ...)

		})

#' @rdname recompute
setMethod("recompute",c("GatingSetList"),function(x, ...){
	invisible(lapply(x, recompute, ..., level = 1))
        })

#' @param x \code{GatingSet}
#' @param y \code{character}
#' @param alwaysLoadData \code{logical} specifies whether to load the flow raw data for gating
#'                  for boolean gates, sometime it is more efficient to skip loading the raw data if all the reference nodes and parent are already gates
#'                  Default 'FALSE' will check the parent node and reference to determine whether to load the data
#'                  but this check may not be sufficient since  the further upstream ancester nodes may not be gated yet
#'                  In that case, we allow the gating to be failed and prompt user to recompute those nodes explictily
#'                  When TRUE, then it forces data to be loaded to guarantee the gating process to be uninterrupted
#'                  , yet may at the cost of unnecessary data IO
#' @noRd 
.recompute <- function(x,y = "root", alwaysLoadData = FALSE, verbose = FALSE, leaf.bool = TRUE){
  cpp_gating(x@pointer, y, alwaysLoadData, verbose, leaf.bool)
  

  message("done!")
  invisible()
}
#' apply \code{FUN} to each sample (i.e. \code{GatingHierarchy})
#'
#' sample names are used for names of the returned list
#'
#' @param X \code{GatingSet}
#' @param FUN \code{function} to be applied to each sample in 'GatingSet'
#' @param ... other arguments to be passed to 'FUN'
#'
#' @rdname lapply-methods
#' @aliases
#' lapply,GatingSet-method
#' @export
setMethod("lapply","GatingSet",function(X,FUN,...){
      sapply(sampleNames(X),function(thisSample,...){
            gh <- X[[thisSample]]
            FUN(gh, ...)
          }, simplify = FALSE, ...)


    })



#' Get/update sample names in a GatingSet
#'
#' Return  a sample names contained in a GatingSet
#'
#' @param object  or a \code{GatingSet}
#'
#' @details
#' The sample names comes from pdata of fs.
#'
#' @return
#' A character vector of sample names
#'
#' @examples
#'       \dontrun{
#'         #G is  a GatingSet
#'         sampleNames(G)
#'       }
#' @aliases sampleNames
#' @rdname sampleNames
#' @export
setMethod("sampleNames","GatingSet",function(object){
      .cpp_getSamples(object@pointer)
    })
#' @name sampleNames
#' @param value \code{character} new sample names
#' @usage \S4method{sampleNames}{GatingSet}(object) <- value
#' @aliases
#' sampleNames<-
#' sampleNames<-,GatingSet-method
#' sampleNames<-,GatingSet,ANY-method
#' @importMethodsFrom Biobase sampleNames<-
#' @rdname sampleNames
#' @export
setReplaceMethod("sampleNames",
    signature=signature(object="GatingSet"),
    definition=function(object, value)
    {
      oldNames <- sampleNames(object)
      #update c++ data structure
      mapply(oldNames,value, FUN = function(oldName, newName){
            .cpp_setSample( object@pointer, oldName, newName)
      })

      
      object
    })

# to speed up reading data from disk later on,
# we can optionally pass j to ncdfFlow::[ to subset on channel
#' @rdname getData-methods
#' @export
setMethod("getData",signature(obj="GatingSet",y="missing"),function(obj,y, ...){
      flowData(obj)[,...]

    })
#' @rdname getData-methods
#' @export
setMethod("getData",signature(obj="GatingSet",y="character"),function(obj,y, ...){

      cs <- new("cytoset", pointer = get_cytoset_from_node(obj@pointer, y))
      cs[,...]
      

		})


#' Fetch or replace the flowData object associated with a GatingSet .
#'
#' Accessor method that gets or replaces the flowset/ncdfFlowSet object in a GatingSet or GatingHierarchy
#'
#' @param x A \code{GatingSet}
#'
#' @details Accessor method that sets or replaces the ncdfFlowSet object in the GatingSet or GatingHierarchy.
#'
#' @return the object with the new flowSet in place.
#'
#' @aliases flowData
#' @rdname flowData
#' @export
setMethod("flowData",signature("GatingSet"),function(x){
      new("cytoset", pointer = get_cytoset(x@pointer))
    })
#' @name flowData
#' @param value The replacement \code{flowSet} or \code{ncdfFlowSet} object
#' @usage \S4method{flowData}{GatingSet}(x) <- value
#' @aliases
#' flowData<-
#' flowData<-,GatingSet-method
#' @rdname flowData
#' @export
setReplaceMethod("flowData",signature(x="GatingSet", value = "cytoset"),function(x,value){
      set_cytoset(x@pointer, value@pointer)
      x
    })


#' read/set pData of flow data associated with \code{GatingSet} or \code{GatingSetList}
#'
#' Accessor method that gets or replaces the pData of the flowset/ncdfFlowSet object in a GatingSet or GatingSetList
#'
#' @param object \code{GatingSet} or \code{GatingSetList}
#'
#' @return a \code{data.frame}
#'
#' @importFrom Biobase pData description exprs sampleNames pData<-
#'
#' @aliases pData
#' @export
#' @rdname pData-methods
setMethod("pData","GatingSet",function(object){
			pData(flowData(object))
		})
#' @name pData
#' @param value \code{data.frame} The replacement of pData for \code{flowSet} or \code{ncdfFlowSet} object
#' @usage \S4method{pData}{GatingSet,data.frame}(object) <- value
#' @aliases
#' pData<-
#' pData<-,GatingSet,data.frame-method
#' @export
#' @rdname pData-methods
setReplaceMethod("pData",c("GatingSet","data.frame"),function(object,value){

			fs <- flowData(object)
            new.rownames <- rownames(value)
            if(is.null(new.rownames))
              new.rownames <- value[["name"]] #use name column when rownames are absent

            rownames(value) <- new.rownames

			pData(fs) <- value

			
			return (object)
		})

#' @description \code{[} subsets a \code{GatingSet} or \code{GatingSetList} using the familiar bracket notation
#'
#' @param x \code{GatingSet} or \code{GatingSetList}
#' @param i \code{numeric} or \code{logical} or \code{character} used as sample index
#' @param j not used
#' @param drop not used
#' @param ... not used
#'
#' @rdname GatingSet-class
#' @export
#' @aliases
#' [,GatingSet,ANY-method
#' [,GatingSetList,ANY-method
setMethod("[",c("GatingSet"),function(x,i,j,...,drop){
#            browser()
      if(extends(class(i), "numeric")||class(i) == "logical"){
        i <- sampleNames(x)[i]
      }
      new("GatingSet", pointer = subset_gs_by_sample(x@pointer, i))
    })

#' subset the GatingSet/GatingSetList based on 'pData'
#'
#' @param x \code{GatingSet} or \code{GatingSetList}
#' @param subset logical expression(within the context of pData) indicating samples to keep. see \code{\link[base:subset]{subset}}
#' @param ... other arguments. (not used)
#' @return a code{GatingSet} or \code{GatingSetList} object
#' @rdname subset
#' @export
subset.GatingSet <- function (x, subset, ...)
{
  pd <- pData(x)
  r <- if (missing(subset))
        rep_len(TRUE, nrow(x))
      else {
        e <- substitute(subset)
        r <- eval(e, pd, parent.frame())
        if (!is.logical(r))
          stop("'subset' must be logical")
        r & !is.na(r)
      }

  x[as.character(rownames(pd[r, ]))]
}
#' @rdname getGate
#' @export
setMethod("getGate",signature(obj="GatingSet",y="character"),function(obj,y){
			lapply(obj,function(x)getGate(x,y))
		})

#' @rdname setNode-methods
#' @export
setMethod("setNode"
    ,signature(x="GatingSet",y="character",value="ANY")
    ,function(x,y,value){
      lapply(x,function(gh){
            setNode(gh,y,value)
          })
    })

#' get/set the log level
#'
#' It is helpful sometime to get more detailed print out for the purpose of trouble shooting
#'
#' @return a character that represents the internal log level
#' @rdname loglevel
#' @export
getLoglevel <- function(){
  level <- .cpp_getLogLevel()
  c("none", "GatingSet", "GatingHierarchy", "Population", "Gate")[level + 1]
}


#' @param level a \code{character} that represents the log level
#'                              , can be value of c("none", "GatingSet", "GatingHierarchy", "Population", "gate")
#'                                 default is "none" , which does not print any information from C parser.
#'
#' @examples
#' getLoglevel()
#' setLoglevel("Population")
#' getLoglevel()
#'
#' @rdname loglevel
#' @export
setLoglevel <- function(level = "none"){
  valid_levels <- c("none", "GatingSet", "GatingHierarchy", "Population", "Gate")
  level <- match.arg(level, valid_levels)
  .cpp_setLogLevel( as.integer(match(level, valid_levels) - 1))
  level
}


#' @description \code{[[} extract a \code{GatingHierarchy} object from a \code{GatingSet} or \code{GatingSetList}
#'
#' @rdname GatingSet-class
#' @export
#' @aliases
#' [[,GatingSet,numeric-method
#' [[,GatingSet,logical-method
#' [[,GatingSet,character-method
setMethod("[[",c(x="GatingSet",i="numeric"),function(x,i,j,...){
      x[[sampleNames(x)[i]]]

    })


setMethod("[[",c(x="GatingSet",i="logical"),function(x,i,j,...){

      x[[sampleNames(x)[i]]]

    })
setMethod("[[",c(x="GatingSet",i="character"),function(x,i,j,...){
      as(x[i], "GatingHierarchy")
      
    })

#' Methods to get the length of a GatingSet
#'
#' Return the length of a \code{GatingSet} or \code{GatingSetList} object (number of samples).
#'
#' @param x \code{GatingSet}
#' @param object \code{object}
#' @aliases length
#' @rdname length
#' @export
setMethod("length","GatingSet",function(x){
      length(flowData(x));
    })

#' @rdname length
#' @export
setMethod("show","GatingSet",function(object){
      cat("A GatingSet with",length(object), "samples\n")
    })


#' Return a table of population statistics for all populations in a GatingHierarchy/GatingSet
#'   or the population proportions or the total number of events of a node (population) in a GatingHierarchy
#'
#' getProp calculates the population proportion (events in the gate / events in the parent population) associated with a node in the \code{GatingHierarchy}.
#' getPopStats is more useful than getPop. Returns a table of population statistics for all populations in a \code{GatingHierarchy}/\code{GatingSet}. Includes the xml counts, openCyto counts and frequencies.
#' getTotal returns the total number of events in the gate defined in the GatingHierarchy object
#' @param x A \code{GatingHierarchy} or \code{GatingSet}
#' @param statistic \code{character} specifies the type of population statistics to extract.(only valid when format is "wide"). Either "freq" or "count" is currently supported.
#' @param xml \code{logical} indicating whether the statistics come from xml (if parsed from xml workspace) or from openCyto.
#' @param path \code{character} see \link{getNodes}
#' @param format \code{character} value of c("wide", "long") specifing whether to origanize the output in long or wide format
#' @param subpopulations \code{character} vector to specify a subset of populations to return. (only valid when format is "long")
#' @param ... Additional arguments passed to \link{getNodes}
#'
#' @details
#' getPopStats returns a table population statistics for all populations in the gating hierarchy. The output is useful for verifying that the import was successful, if the xml and openCyto derived counts don't differ much (i.e. if they have a small coefficient of variation.) for a GatingSet, returns a matrix of proportions for all populations and all samples
#' getProp returns the proportion of cells in the gate, relative to its parent.
#' getTotal returns the total number of events included in this gate. The contents of "thisTot" variable in the "metadata" environment of the \code{nodeData} element associated with the gating tree and gate / population.
#'
#' @return
#' getPopStats returns a \code{data.frame} with columns for the population name, xml derived counts, openCyto derived counts, and the population proportions (relative to their parent pouplation).
#' getProp returns  a population frequency \code{numeric}.
#' getTotal returns a \code{numeric} value of the total number of elements in the population.
#' @seealso \code{\link{getNodes}}
#' @examples
#'         \dontrun{
#'         #gh is a GatingHierarchy
#'         getPopStats(gh);
#'         #proportion for the fifth population
#'         getProp(gh,getNodes(gh)[5])
#'         getTotal(gh,getNodes(gh,tsort=T)[5])
#'
#'         #gs is a GatingSet
#'         getPopStats(gs)
#'         #optionally output in long format as a data.table
#'         getPopStats(gs, format = "long", path = "auto")
#'         #only get stats for a subset of populations
#'         getPopStats(gs, format = "long", subpopulations = getNodes(gs)[4:6])
#'         }
#' @aliases getPopStats
#' @rdname getPopStats
#' @export
#' @import data.table
setMethod("getPopStats", "GatingSet",
    function(x, statistic = c("freq", "count"), xml = FALSE, subpopulations = NULL, format = c("long", "wide"), path = "full", ...) {

      # Based on the choice of statistic, the population statistics are returned for
      # each Gating Hierarchy within the GatingSet.
      statistic <- match.arg(statistic)
      format <- match.arg(format)
      path <- match.arg(path, c("full", "auto"))

      if(format == "long"){

        if(is.null(subpopulations))
          subpopulations <- getNodes(x, path = path, ...)[-1]

        pop_stats <- .getPopCounts(x@pointer, subpopulations, xml, path == "full")
        pop_stats <- data.table(name = pop_stats[["name"]]
                              , Population = pop_stats[["Population"]]
                              , Parent = pop_stats[["Parent"]]
                              , Count = pop_stats[["Count"]]
                              , ParentCount = pop_stats[["ParentCount"]]
                            )

      }else{


        # The 'xml' flag determines whether the 'xml' or 'openCyto' statistics
        # are returned.
        if (xml) {
          statistic <- paste("xml", statistic, sep = ".")
        } else {
          statistic <- paste("openCyto", statistic, sep = ".")
        }

        stats <- lapply(x,function(y){
                d<-getPopStats(y, path = path,...)
                d$key<-rownames(d)
                setkeyv(d,"key")
                d<-d[,list(key,get(statistic))]
                setnames(d,c("key",sampleNames(y)))
                setkeyv(d,"key")
                d
        })
        pop_stats <- Reduce(function(x,y)merge(x,y,all=TRUE),stats)

        rownames(pop_stats) <- pop_stats[,key]
        setkey(pop_stats,NULL)
        pop_stats$key<-NULL
        rn<-rownames(pop_stats)
        pop_stats<-as.matrix(pop_stats)
        rownames(pop_stats)<-rn

    }
    pop_stats
})

#' calculate the coefficient of variation
#' 
#' This builds matrix with all node labels for all GH's
#' so expect many NAs if the GH's don't have matching trees
#' 
#' @noRd 
.computeCV <- function(x, ...){
  #columns are populations
  #rows are samples
  statList <- lapply(x,function(gh){
    thisStat <- getPopStats(gh, ...)
    thisStat
  })
  all_pops <- unique(unlist(lapply(statList, function(x) x$node)))
  cvs <- lapply(statList,function(x){
    res <- apply(x[,list(xml.count,openCyto.count)],1,function(x){
      cv <- IQR(x)/median(x)
      ifelse(is.nan(cv),0,cv)
    })
    names(res) <- rownames(x)
    res
  })
  cvs <- do.call(rbind, lapply(cvs, function(x) x[match(all_pops, names(x))]))
  colnames(cvs) <- all_pops
  cvs
}

#' Plot the coefficient of variation between xml and openCyto population statistics for each population in a gating hierarchy.
#'
#' This function plots the coefficient of variation calculated between the xml population statistics and the openCyto population statistics for each population in a gating hierarchy extracted from a xml Workspace.
#' @param x A \code{GatingHierarchy} from or a \code{GatingSet}.
#' @param m \code{numeric} The number of rows in the panel plot. Now deprecated, uses lattice.
#' @param n \code{numeric} The number of columns in the panel plot. Now deprecated, uses lattice.
#' @param scales \code{list} see \link{barchart}
#' @param path \code{character} see \link{getNodes}
#' @param \dots Additional arguments to the \code{barplot} methods.
#' @details The CVs are plotted as barplots across panels on a grid of size \code{m} by \code{n}.
#' @return Nothing is returned.
#' @seealso \code{\link{getPopStats}}
#' @examples
#'   \dontrun{
#'     #G is a GatingHierarchy
#'     plotPopCV(G,4,4);
#'   }
#' @aliases plotPopCV
#' @export
#' @rdname plotPopCV
#' @import ggplot2
setMethod("plotPopCV","GatingSet",function(x, scales = list(x = list(rot = 90)), path = "auto",...){
      cv <- .computeCV(x, path = path)
      #flatten, generate levels for samples.
      nr<-nrow(cv)
      nc<-ncol(cv)
      populations<-gl(nc,nr,labels=as.character(colnames(cv)), ordered = TRUE)
      samples<-as.vector(t(matrix(gl(nr,nc,labels=basename(as.character(rownames(cv)))),nrow=nc)))
      cv<-data.frame(cv=as.vector(cv),samples=samples,populations=populations)
      return(ggplot2::ggplot(na.omit(cv), aes(x=populations, y=cv)) +
               # free_x to let each plot individually clip out NA bars
               facet_wrap(~samples, scales="free_x") + 
               geom_col() +
               ylab("Coefficient of Variation") +
               xlab(NULL) +
               scale_y_continuous() +
               scale_x_discrete() +
               theme(axis.text.x= element_text(angle = 90, hjust=1, vjust = 0.5)))
      })

#' @rdname keyword
#' @export
setMethod("keyword",c("GatingSet", "missing"),function(object,keyword = "missing", ...){
        lapply(object, flowCore::keyword, ...)

    })
#' @rdname keyword
#' @export
setMethod("keyword",c("GatingSet","character"),function(object,keyword){
      tmp<-data.frame(unlist(lapply(object,function(x)keyword(x,keyword)),use.names=FALSE));
      tmp<-data.frame(matrix(tmp[[1]],ncol=length(keyword),byrow=T))
      colnames(tmp)<-keyword
      tmp
    })


#' tranform the flow data asssociated with the GatingSet
#'
#' The transformation functions are saved in the GatingSet and can be retrieved by \link{getTransformations}.
#' Currently only flowJo-type biexponential transformation(either returned by \link{getTransformations} or constructed by \link{flowJoTrans})
#' is supported.
#'
#' @param _data \code{GatingSet} or \code{GatingSetList}
#' @param translist expect a \code{transformList} object or a list of \code{transformList} objects(with names matched to sample names)
#' @param ... other arguments passed to 'transform' method for 'ncdfFlowSet'.(e.g. 'ncdfFile')
#' @return a \code{GatingSet} or \code{GatingSetList} object with the underling flow data transformed.
#' @examples
#' \dontrun{
#' data(GvHD)
#' fs <- GvHD[1:2]
#' gs <- GatingSet(fs)
#'
#' #construct biexponential transformation function
#' biexpTrans <- flowJo_biexp_trans(channelRange=4096, maxValue=262144, pos=4.5,neg=0, widthBasis=-10)
#'
#' #make a transformList object
#' chnls <- c("FL1-H", "FL2-H")
#' transList <- transformerList(chnls, biexpTrans)
#'
#' #add it to GatingSet
#' gs_trans <- transform(gs, transList)
#'
#' }
#' @export
#' @rdname transform
setMethod("transform",
    signature = signature(`_data` = "GatingSet"),
    definition = function(`_data`, translist, ...)
    {
      
      gs <- `_data`
      # browser()
      if(missing(translist))
        stop("Missing the second argument 'translist'!")
      else if(is(translist, "transformerList"))
      {
        translist <- sapply(sampleNames(gs), function(obj)translist, simplify = FALSE)
      }
      
      if(is(translist, "list"))
      {
        tList <- lapply(translist, function(trans){
          if(!is(trans, "transformerList"))
            stop("All the elements of 'translist' must be 'transformerList' objects!")
          
          res <- lapply(trans, function(obj)obj[["transform"]])
          transformList(names(trans), res)  
        })
      }else
        stop("expect the second argument as a 'transformerList' object or a list of 'transformerList' objects!")
  	
	#check if all trans are supported by Rcpp
  unrecognized <- FALSE
	for(sn in names(translist))
	{
	  
		for(trans in translist[[sn]])
		{
  		  
  		transobj <- parse_transformer(trans)
  		if(length(transobj)==0)
  		{
  			unrecognized <- TRUE
  			break
  		}
  		
		}
	  
	  if(unrecognized)
	    break
	}

	if(unrecognized)#transform in R
	{
		gs@transformation <- translist

		fs <- getData(gs)

		suppressMessages(fs_trans <- transform(fs, tList, ...))
		flowData(gs) <- fs_trans

	}else
	{ #transform data and store trans in c++
		for(sn in names(translist))
		{
			transobjs <- sapply(translist[[sn]], parse_transformer, simplify = FALSE)
			# browser()
			set_transformations(gs@pointer, sn, transobjs)

		}
		gs_transform_data(gs@pointer)
	}
	gs
    })

#' Constructor for transformerList object
#'
#' Similar to \code{transformList} function, it constructs a list of transformer objects generated by \code{trans_new}
#' method from \code{scales} so that the inverse and breaks functions are also included.
#' @param from channel names
#' @param trans a \code{trans} object or a list of \code{trans} objects constructed by \code{trans_new} method.
#' @export
#' @examples
#' library(scales)
#' #create tranformer object from scratch
#' trans <- logicleTransform(w = 0.5, t = 262144, m = 4.5, a = 0)
#' inv <- inverseLogicleTransform(trans = trans)
#' trans.obj <- flow_trans("logicle", trans, inv, n = 5, equal.space = FALSE)
#'
#' #or simply use convenient constructor
#' #trans.obj <- logicle_trans(n = 5, equal.space = FALSE, w = 0.5, t = 262144, m = 4.5, a = 0)
#'
#' transformerList(c("FL1-H", "FL2-H"), trans.obj)
#'
#' #use different transformer for each channel
#' trans.obj2 <- asinhtGml2_trans()
#' transformerList(c("FL1-H", "FL2-H"), list(trans.obj, trans.obj2))
transformerList <- function (from, trans)
{
  from <- unique(from)
  if(is(trans, "trans"))trans <- list(trans)
  if (!is.character(from))
    stop("'from' must be character vectors.", call. = FALSE)
  if (!is.list(trans))
    trans <- list(trans)
  if (!all(sapply(trans, is, "trans")))
    stop("'trans' must be a list of transformer objects (generated by scales::trans_new method)", call. = FALSE)
  trans <- rep(trans, length(from))
  trans <- trans[1:length(from)]
  names(trans) <- from
  attr(trans, "class") <- c("transformerList", "list")

  return(trans)
}



#' compensate the flow data asssociated with the GatingSet
#'
#' The compensation is saved in the GatingSet and can be retrieved by \link{getCompensationMatrices}.
#'
#' @param x \code{GatingSet} or \code{GatingSetList}
#' @param spillover \code{compensation} object or a list of \code{compensation} objects
#'
#' @return a \code{GatingSet} or \code{GatingSetList} object with the underling flow data compensated.
#' @examples
#' \dontrun{
#'
#' cfile <- system.file("extdata","compdata","compmatrix", package="flowCore")
#' comp.mat <- read.table(cfile, header=TRUE, skip=2, check.names = FALSE)
#' ## create a compensation object
#' comp <- compensation(comp.mat,compensationId="comp1")
#' #add it to GatingSet
#' gs <- compensate(gs, comp)
#' }
#' @export
#' @rdname compensate
setMethod("compensate", signature=signature(x="GatingSet", spillover="ANY"),
    definition=function(x, spillover){
      selectMethod("compensate", signature=c(x="cytoset", spillover="ANY"))(x, spillover)
      
    })
#' @export
#' @method getCompensationMatrices GatingSet
#' @rdname getCompensationMatrices
getCompensationMatrices.GatingSet <- function(x){
			lapply(x, getCompensationMatrices)
		}
            
#' @rdname markernames
#' @export
setMethod("markernames",
          signature=signature(object="GatingSet"),
          definition=function(object){

            markernames(flowData(object))

          })

#' @rdname markernames
#' @export
setReplaceMethod("markernames",
                 signature=signature(object="GatingSet", value="ANY"), function(object, value){

                   markernames(flowData(object)) <- value

                   object
                 })

#' @rdname markernames
#' @export
setMethod("colnames",
          signature=signature(x="GatingSet"),
          definition=function(x, do.NULL="missing", prefix="missing"){

            colnames(flowData(x))

          })

#' @rdname markernames
#' @export
setReplaceMethod("colnames",
                 signature=signature(x="GatingSet", value="ANY"), function(x, value){

                   colnames(flowData(x)) <- value

                   x
                 })
