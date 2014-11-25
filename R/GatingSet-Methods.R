#' @include GatingHierarchy-Methods.R
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

#' @title save/load a GatingSet/GatingSetList to/from disk.
#'
#' @description
#' Save/load a GatingSet/GatingSetList which is the gated flow data including gates and populations to/from the disk.
#' The \code{GatingSet} object The internal C data structure (gating tree),\code{ncdfFlowSet} object(if applicable)
#'
#' @param G A \code{GatingSet}
#' @param gslist A \code{GatingSetList}
#' @param path A character scalar giving the path to save/load the GatingSet to/from.
#' @param overwrite A logical scalar specifying whether to overwrite the existing folder.
#' @param cdf a character scalar. The valid options are :"copy","move","skip","symlink","link" specifying what to do with the cdf data file.
#'              Sometime it is more efficient to move or create a link of the existing cdf file to the archived folder.
#'              It is useful to "skip" archiving cdf file if raw data has not been changed.
#' @param type a character scalar. The valid options are :"binary","text","xml" specifying format to store tree structure.(only meaningful when lib == 'BS')
#'                                  default is "binary", which is smaller and faster but machine-dependent.
#'                                  use "text" or "xml" for cross-platform data interchange (deprecated by 'PB' serialization).
#' @param lib a character scalar specifying which serialization library to use. Can be either 'PB' (google protocol buffer) or 'BS'(boost serialization).
#' @param ... other arguments: not used.
#'
#'
#' @return
#' \code{load_gs} returns a GatingSet object
#' \code{load_gslist} returns a GatingSetList object
#'
#' @seealso \code{\link{GatingSet-class}},\code{\link{GatingSetList-class}}
#'
#' @examples
#' \dontrun{
#' 	#G is a GatingSet
#' 	save_gs(G,path="tempFolder")
#' 	G1<-load_gs(path="tempFolder")
#'
#' 	#G is a GatingSet
#'
#' 	save_gslist(gslist1,path="tempFolder")
#' 	gslist2<-load_gslist(path="tempFolder")
#' }
#' @rdname save_gs
#' @export
#' @aliases save_gs load_gs save_gslist load_gslist
save_gs<-function(G,path,overwrite = FALSE
                    , cdf = c("copy","move","skip","symlink","link")
                    , type = c("binary","text", "xml")
                    , lib = c("PB", "BS")
                    , ...){
#  browser()
  cdf <- match.arg(cdf)
  type <- match.arg(type)
  lib <- match.arg(lib)
  
  isPB <- lib == "PB"
  if(isPB)
    fileext <- 'pb'
  else
    fileext <-  switch(type
                      , binary = "dat"
                      , text = "txt"
                      , xml = "xml"
                      , "dat"
                  )
  
  guid <- G@guid
  if(length(guid) == 0){
    G@guid <- .uuid_gen()
    guid <- G@guid
  }
  rds_toSave <- paste(guid,"rds",sep=".")
  dat_toSave <- paste(guid,fileext,sep=".")

  if(file.exists(path)){
    path <- normalizePath(path,mustWork = TRUE)
    if(overwrite){
      this_files <- list.files(path)
      #validity check for non-empty folder
      if(length(this_files)!=0)
      {
        rds_ind <- grep("\\.rds$",this_files)
        dat_ind <- grep(paste0("\\.",fileext,"$"),this_files)

        if(length(rds_ind)!=1||length(dat_ind)!=1){
          stop("Not a valid GatingSet archiving folder!")
        }else{
          this_rds <- this_files[rds_ind]
          this_dat <- this_files[dat_ind]

          if(this_rds!=rds_toSave||this_dat!=dat_toSave){
            stop("The GatingSet doesn't match the archived files in: ", path)
          }
        }
      }

      #validity check for cdf
      if(isNcdf(G)){
        if(length(this_files)!=0){
          cdf_ind <- grep("\\.nc$",this_files)
          if(length(cdf_ind) != 1){
            stop("Not a valid GatingSet archiving folder!")
          }
        }

      }
      if(length(this_files)!=0)
      {
        #start to delete the old files in path
        file.remove(file.path(path,rds_toSave))
        file.remove(file.path(path,dat_toSave))

        if(isNcdf(G)){
          #check if the target path is the same as current cdf path
#            browser()
          this_cdf <- file.path(path,this_files[cdf_ind])
          if(normalizePath(getData(G)@file)==this_cdf){
            cdf <- "skip"
          }
          if(cdf != "skip"){
            file.remove(this_cdf)
          }
        }
      }

    }else{
      stop(path,"' already exists!")
    }

  }else{
    dir.create(path = path)
    #do the dir normalization again after it is created
    path <- normalizePath(path,mustWork = TRUE)

  }
#  browser()
  invisible(.save_gs(G=G,path = path, cdf = cdf, type = type, lib = lib, ...))
  message("Done\nTo reload it, use 'load_gs' function\n")


}


#' @rdname save_gs
#' @export
#' @aliases load_gs load_gslist
load_gs<-function(path){
#  browser()
  path <- normalizePath(path,mustWork = TRUE)
  if(!file.exists(path))
    stop(path,"' not found!")
  files<-list.files(path)
#   browser()
  .load_gs(output = path, files = files)$gs

}




#' serialization functions to be called by wrapper APIs
.save_gs <- function(G,path, cdf = c("copy","move","skip","symlink","link"), type = c("binary","text", "xml"), lib = c("PB", "BS")){

#    browser()
    cdf <- match.arg(cdf)
    type <- match.arg(type)
    lib <- match.arg(lib)
    #only meaning for for BS
    typeID <- switch(type
                      , binary = 0
                      , text = 1
                      , xml = 2
                      , 0
                  )
    isPB <- lib == "PB"
    if(isPB)
      fileext <- 'pb'
    else
      fileext <-  switch(type
          , binary = "dat"
          , text = "txt"
          , xml = "xml"
          , "dat"
      )
    

    if(!file.exists(path))
      stop("Folder '",path, "' does not exist!")
    #generate uuid for the legacy GatingSet Object
    if(length(G@guid)==0){
      G@guid <- .uuid_gen()
    }
    guid <- G@guid

    rds.file<-file.path(path,paste(guid,"rds",sep="."))
    dat.file<-file.path(path,paste(guid,fileext,sep="."))


    filestoSave <- c(rds.file,dat.file)
    #save ncdf file
    if(cdf != "skip" && isNcdf(G))
    {
      from<-flowData(G)@file
#      browser()
      if(cdf == "move"){
        message("moving ncdf...")
        ncFile <- file.path(path,basename(from))
        res <- file.rename(from,ncFile)
        #reset the file path for ncdfFlowSet
        flowData(G)@file <- ncFile
      }else{

        ncFile<-tempfile(tmpdir=path,fileext=".nc")

        if(cdf == "copy"){
          message("saving ncdf...")
          res <- file.copy(from=from,to=ncFile)
        }
        else if(cdf == "symlink"){
          message("creating the symbolic link to ncdf...")
          res <- file.symlink(from=from,to=ncFile)
        }else if(cdf == "link"){
          message("creating the hard link to ncdf...")
          res <- file.link(from=from,to=ncFile)
        }
      }
      if(!res){
        stop("failed to ",cdf," ",from,"!")
      }
      filestoSave<-c(filestoSave,ncFile)
    }

    message("saving tree object...")
    #save external pointer object
    .Call("R_saveGatingSet",G@pointer,dat.file, typeID, isPB)

    message("saving R object...")
    saveRDS(G,rds.file)

    filestoSave

}
#' unserialization functions to be called by wrapper APIs
#' @importFrom tools file_ext
.load_gs <- function(output,files){
      dat.file<-file.path(output,files[grep(".pb$|.dat$|.txt$|.xml$",files)])
      rds.file<-file.path(output,files[grep(".rds$",files)])

      nc.file<-file.path(output,files[grep(".nc$|.nc.trans$",files)])
    #   browser()
      if(length(dat.file)==0)
        stop(".dat file missing in ",output)
      if(length(dat.file)>1)
        stop("multiple .dat or .pb files found in ",output)
      fileext <- file_ext(dat.file)
      isPB <- fileext == "pb"
      #only meaning for for BS
      typeID <- switch(fileext
                      , "dat" = 0
                      , "txt" = 1
                      , "xml" = 2
                      , 0
                      )
      if(length(rds.file)==0)
        stop(".rds file missing in ",output)
      if(length(rds.file)>1)
        stop("multiple .rds files found in ",output)

      message("loading R object...")
      gs <- readRDS(rds.file)

      #deal with legacy archive
      if(class(gs) == "GatingSetInternal")
      {
        thisSet <- attr(gs,"set")
        thisGuid <- attr(gs,"guid")
        if(is.null(thisGuid))
          thisGuid <- .uuid_gen()
#        browser()
        thisGH <- thisSet[[1]]
        thisTree <- attr(thisGH, "tree")
        thisPath <- dirname(attr(thisGH, "dataPath"))
        thisData <- graph::nodeDataDefaults(thisTree)[["data"]]
        fs <- thisData[["data"]][["ncfs"]]

        axis <- sapply(thisSet, function(gh){
                            thisTree <- attr(thisGH, "tree")
                            thisData <- graph::nodeDataDefaults(thisTree)[["data"]]
#                            browser()
                            thisSample <- attr(thisGH, "name")
                            thisChnls <- colnames(fs@frames[[thisSample]])
                            thisAxis <- thisData[["axis.labels"]]
                            if(is.null(thisAxis))
                              list()
                            else{
                              names(thisAxis) <- thisChnls
                              thisAxis
                            }

                          }, simplify = FALSE)


        gs <- new("GatingSet", flag = TRUE, FCSPath = thisPath, guid = thisGuid, axis = axis, data = fs)
      }

      
      guid <- try(slot(gs,"guid"),silent=T)
      if(class(guid)=="try-error"){
        #generate the guid for the legacy archive
        gs@guid <- .uuid_gen()
      }

      message("loading tree object...")
      gs@pointer<-.Call("R_loadGatingSet",dat.file, typeID, isPB)


      if(isNcdf(gs))
      {
        if(length(nc.file)==0)
          stop(".nc file missing in ",output)
        flowData(gs)@file <- nc.file

      }
      message("Done")
      list(gs=gs,files=c(dat.file,rds.file))
}

#' archive/unarchive to/from a tar file
#'
#' Defunct by save_gs/load_gs
#' @param G a \code{GatingSet}
#' @param file a \code{character} target/source archive file name
#' @aliases archive unarchive
#' @rdname archive
#' @export
archive<-function(G,file=tempfile()){
	.Defunct("save_gs")
	filename<-basename(file)
	dirname<-dirname(file)
	filename<-sub(".tar$","",filename)
#	browser()
    toTar<- .save_gs(G,path  = dirname)

	curDir<-getwd()
	setwd(dirname)
	system(paste("tar -cf ",basename(file),paste(basename(toTar),collapse=" ")))
#	tar(tarfile=file,files=toTar) #somehow the R internal tar doesn't work

	#remove intermediate files
	file.remove(basename(toTar))
	setwd(curDir)
	message("Done\nTo reload it, use 'unarchive' function\n")

}


#' @param path a \code{character} target folder that stores cdf file
#' @export
#' @rdname archive
unarchive<-function(file,path=tempdir()){
    .Defunct("load_gs")
	if(!file.exists(file))
		stop(file,"' not found!")
#	browser()
	files<-untar(tarfile=file,list=TRUE)

#	message("extracting files...")

	output<-path
	untar(tarfile=file,exdir=output)

    res <- .load_gs(output = path, files = files)
    toRemove <- res$files
    gs <- res$gs
	#clean up the intermediate files
	file.remove(toRemove)
	message("Done")
	return (gs)

}



.parseWorkspace <- function(xmlFileName,sampleIDs,execute,path,isNcdf,includeGates,sampNloc="keyword",xmlParserOption, wsType, ...){


	message("calling c++ parser...")
#	browser()

	time1<-Sys.time()
	G <- GatingSet(x = xmlFileName
                  , y = sampleIDs
                  , includeGates = includeGates
                  , sampNloc=sampNloc
                  , xmlParserOption = xmlParserOption
                  , wsType = wsType
                  )

	message("c++ parsing done!")
	samples <- .Call("R_getSamples",G@pointer)

#	browser()
	#loading and filtering data
	if(execute)
	{

		dataPaths<-vector("character")
		excludefiles<-vector("logical")
		for(file in samples){

			#########################################################
			#get full path for each fcs and store in FCSPath slot
			#########################################################
			##escape "illegal" characters
			file<-gsub("\\?","\\\\?",gsub("\\]","\\\\]",gsub("\\[","\\\\[",gsub("\\-","\\\\-",gsub("\\+","\\\\+",gsub("\\)","\\\\)",gsub("\\(","\\\\(",file)))))))
			absPath<-list.files(pattern=paste("^",file,"",sep=""),path=path,recursive=TRUE,full.names=TRUE)

			if(length(absPath)==0){
				warning("Can't find ",file," in directory: ",path,"\n");
				excludefiles<-c(excludefiles,TRUE);

			}else{
#				browser()
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
        G@FCSPath <- dataPaths
	}else
	{
		files<-samples
	}
	G <- .addGatingHierarchies(G,files,execute,isNcdf, wsType = wsType, ...)


	message("done!")


    # try to post process the GatingSet to split the GatingSets(based on different the gating trees) if needed                
    gslist <- suppressMessages(.groupByTree(G))
    if(length(gslist) == 1)
      return (gslist[[1]])
    else
    {
      warning("Due to the different gating tree structures, a list of GatingSets is returned instead!")
      return (gslist)
    }
}

#' constructors for GatingSet
#'
#' construct object from existing gating hierarchy(gating template) and flow data
#'
#' @param path \code{character} specifies the path to the flow data (FCS files)
#' @param isNcdf \code{logical} whether to use ncdfFlowSet or flowSet as the underlying flow data storage
#' @param ... other arguments. see \link{parseWorkspace}
#' @rdname GatingSet-methods
#' @export 
setMethod("GatingSet", c("GatingHierarchy", "character"), function(x, y, path=".", isNcdf=FALSE,  ...){

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
				absPath<-list.files(pattern=paste("^",file,"$",sep=""),path=path,recursive=TRUE,full=TRUE)

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
			Object<-new("GatingSet")
			message("generating new GatingSet from the gating template...")
			Object@pointer <- .Call("R_NewGatingSet",x@pointer,sampleNames(x),samples)
            Object@guid <- .uuid_gen()
            Object@FCSPath <- dataPaths
			Object<-.addGatingHierarchies(Object,files,execute=TRUE,isNcdf=isNcdf,...)
            #if the gating template is already gated, it needs to be recompute explicitly
            #in order to update the counts
            #otherwise, the counts should already have been updated during the copying
            #and not need to do this step
            if(x@flag)
              recompute(Object)
            message("done!")
			return(Object)
		})


#' constructing gating set
#' @param prefix a \code{logical} flag indicates whether the colnames needs to be updated with prefix(e.g. "<>" or "comp") specified by compensations
#' @param ignore.case a \code{logical} flag indicates whether the colnames(channel names) matching needs to be case sensitive (e.g. compensation, gating..)
#' @param extend_val \code{numeric} the threshold that determine wether the gates need to be extended. default is 0. It is triggered when gate coordinates are below this value.
#' @param extend_to \code{numeric} the value that gate coordinates are extended to. Default is -4000. Usually this value will be automatically detected according to the real data range.
#'                                  But when the gates needs to be extended without loading the raw data (i.e. \code{execute} is set to FALSE), then this hard-coded value is used.
#' @importMethodsFrom flowCore colnames colnames<- compensate spillover sampleNames
#' @importFrom flowCore compensation read.FCS read.FCSheader read.flowSet
#' @importClassesFrom flowCore flowFrame flowSet
.addGatingHierarchies <- function(G,files,execute,isNcdf,compensation=NULL,wsType = "", extend_val = 0, extend_to = -4000, prefix = TRUE, ignore.case = FALSE, ws = NULL, leaf.bool = TRUE,...){

    if(length(files)==0)
      stop("not sample to be added to GatingSet!")

    #load the raw data from FCS
	if(execute)
	{
		if(isNcdf){
			stopifnot(length(grep("ncdfFlow",loadedNamespaces()))!=0)
			message("Creating ncdfFlowSet...")
			fs<-read.ncdfFlowSet(files,isWriteSlice=FALSE,...)
		}else{
			message("Creating flowSet...")
			fs<-read.flowSet(files,...)
		}
	}else{
      #create dummy flowSet
      frList <- sapply(basename(files), function(thisSample){
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

	axis <- lapply(files,function(file,tempenv){

        #get global variable
        prefixColNames <- tempenv$prefixColNames

		sampleName<-basename(file)

        # get comp
        comp <- .Call("R_getCompensation", G@pointer, sampleName)
        cid <- comp$cid
        

        ##################################
        #Compensating the data
        ##################################
        if(execute)
		{

            cnd <- colnames(fs)
			message("loading data: ",file);
			if(isNcdf)
				data <- read.FCS(file)[, cnd]
			else
				data <- fs[[sampleName]]

            #alter colnames(replace "/" with "_") for flowJo X
            if(wsType == "vX"){
                new_cnd <- gsub("/","_",cnd)
                if(!all(new_cnd == cnd)){ #check if needs to update colnames to avoid unneccessary expensive colnames<- call
                  cnd <- new_cnd
                  colnames(data) <- cnd
                  
                }
                   
            }



			if(cid=="")
				cid=-2

			if(cid!="-1" && cid!="-2"){
				message("Compensating");

				marker <- comp$parameters

				if(is.null(compensation)){
                  ## try to match marker from comp with flow data in case flowJo is not consistent with data
                  markerInd <- sapply(marker, function(thisMarker)grep(thisMarker, cnd, ignore.case = ignore.case))
                  matchedMarker <- cnd[markerInd]
                  if(length(matchedMarker) != length(marker))
                    stop("channels mismatched between compensation and flow data!")
                  marker <- matchedMarker

                  compobj <- compensation(matrix(comp$spillOver,nrow=length(marker),ncol=length(marker),byrow=TRUE,dimnames=list(marker,marker)))
                }else
					compobj <- compensation#TODO: to update compensation information in C part
				#TODO this compensation will fail if the parameters have <> braces (meaning the data is stored compensated).
				#I need to handle this case properly.

                res <- try(compensate(data,compobj),silent=TRUE)
				if(inherits(res,"try-error")){
					message("Data is probably stored already compensated");
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
						message("Data is probably stored already compensated");
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
           kw <- getKeywords(ws, sampleName)
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
              				
              wh <- match(comp_param, prefixColNames)
              
              prefixColNames[wh] <- paste(comp$prefix,comp_param,comp$suffix,sep="")
              
                

            }
        }else{
          prefixColNames <- cnd
        }
          ##################################
          #transforming and gating
          ##################################
          if(execute)
          {

            message(paste("gating ..."))
            #stop using gating API of cdf-version because c++ doesn't store the view of ncdfFlowSet anymore
            mat <- data@exprs #using @ is faster than exprs()
            #get gains from keywords
            # for now we still parse it from data
            # once confirmed that workspace is a reliable source for this info
            # we can parse it from ws as well
            this_pd <- pData(parameters(data))
            paramIDs <- rownames(pData(parameters(data)))
            key_names <- paste(paramIDs,"G",sep="")
            kw <- keyword(data)
            if(as.numeric(kw[["FCSversion"]])>=3){
              kw_gains <- kw[key_names]

              # For keywords where the gain is not set, the gain is NULL.
              # We replace these instances with the default of 1.
              kw_gains[sapply(kw_gains, is.null)] <- 1

              gains <- as.numeric(kw_gains)
            }else{
              gains <- rep(1,length(paramIDs))
            }

            names(gains) <- this_pd$name

            #update colnames in order for the gating to find right dims
            if(!is.null(prefixColNames)){
              dimnames(mat) <- list(NULL, prefixColNames)
            }
            recompute <- FALSE
            nodeInd <- 0
            
            .Call("R_gating",G@pointer, mat, sampleName, gains, nodeInd, recompute, extend_val, ignore.case, leaf.bool)
#            browser()
            #restore the non-prefixed colnames for updating data in fs with [[<-
            #since colnames(fs) is not udpated yet.
            if(!is.null(prefixColNames)){
              #restore the orig colnames(replace "_" with "/") for flowJo X
              if(wsType == "vX"){
                old_cnd <- gsub("_","/",cnd)
                if(!all(old_cnd == cnd)){ #check if needs to update colnames to avoid unneccessary expensive colnames<- call
                  cnd <- old_cnd
                  colnames(data) <- cnd #restore colnames for flowFrame as well for flowJo vX  
                }
                
              }
              dimnames(mat) <- list(NULL, cnd)
            }

            data@exprs <- mat #circumvent the validity check of exprs<- to speed up
            if(isNcdf){
              fs[[sampleName]] <- data

            }else{
              assign(sampleName,data,fs@frames)
            }
            #range info within parameter object is not always the same as the real data range
            #it is used to display the data.
            #so we need update this range info by transforming it
            tInd <- grepl("[Tt]ime",cnd)
            tRg  <- range(mat[,tInd])
            axis.labels <- .transformRange(G,sampleName,wsType,fs@frames,timeRange = tRg)

		}else{
          #extract gains from keyword of ws
          #currently it is only used for extracting gates without gating
          #In future we want to use it for gating as well
          #once we have confirmed that ws is a reliable source of keyword
#          browser()

          #get gains from keywords
          kw_gains <- grep("P[0-9]{1,}G", names(kw))
          
          if(length(kw_gains) > 0){
            key_names <- unique(names(kw[kw_gains]))
            kw_gains <- kw[key_names]

            # For keywords where the gain is not set, the gain is NULL.
            # We replace these instances with the default of 1.
            kw_gains[sapply(kw_gains, is.null)] <- 1
            
            gains <- as.numeric(kw_gains)                      
          }else{
            gains <- rep(1,length(cnd))
          }
          

          names(gains) <- prefixColNames
          #transform and adjust the gates without gating
          .Call("R_computeGates",G@pointer, sampleName, gains, extend_val, extend_to)
          axis.labels <- list()
        }

        #set global variable
        tempenv$prefixColNames <- prefixColNames

        #return axis.labels
        axis.labels
	},tempenv)

    names(axis) <- basename(files)
    G@axis <- axis
    G@flag <- execute #assume the excution would succeed if the entire G gets returned finally

	if(execute)
	{
#		browser()
		#update colnames
		#can't do it before fs fully compensated since
		#compensate function check the consistency colnames between input flowFrame and fs
		if(!is.null(tempenv$prefixColNames))
          colnames(fs) <- tempenv$prefixColNames

		#attach filename and colnames to internal stucture for gating
#		browser()
	}

    flowData(G) <- fs
	G
}


#' transform the range slot and construct axis label and pos for the plotting
#' @param gh \code{GatingHierarchy}
#' @param wsType \code{character} flowJo workspace type
#' @param frmEnv \code{environment} point to the \code{frames} slot of the original \code{flowSet}
#' @param timeRange \code{numeric} vector specifying the range for 'time' channel
#'
#' @return
#' a \code{list} of axis labels and positions. Also, the \code{range} slot of \code{flowFrame} stored in \code{frmEnv} are transformed as an side effect.
.transformRange <- function(G,sampleName, wsType,frmEnv, timeRange = NULL){
#  browser()

     cal<-.getTransformations(G@pointer, sampleName)
     comp<-.Call("R_getCompensation",G@pointer,sampleName)
     prefix <- comp$prefix
     suffix <- comp$suffix
#	frmEnv<-dataenv$data$ncfs@frames
	rawRange<-range(get(sampleName,frmEnv))
	tempenv<-new.env()
	assign("axis.labels",list(),envir=tempenv);

    cal_names <-trimWhiteSpace(names(cal))
	datarange<-sapply(1:dim(rawRange)[2],function(i){

				#added gsub
              if(wsType == "vX"){
                #have to do strict match for vX since trans functions can be defined for both compensated and uncompensated channel
                j <- match(names(rawRange)[i],cal_names)
                isMatched <- !is.na(j)
              }else{
                j<-grep(gsub(suffix,"",gsub(prefix,"",names(rawRange)))[i],cal_names);
                isMatched <- length(j)!=0
              }

              this_chnl <- names(rawRange)[i]
              prefixedChnl <- paste(prefix,this_chnl,suffix,sep="")
				if(isMatched){
#									browser()
					rw<-rawRange[,i];
					if(attr(cal[[j]],"type")!="gateOnly"){
						r<-cal[[j]](c(rw))
					}else{
						r<-rw
					}
					###An unfortunate hack. If we use the log transformation, then negative values are undefined, so
					##We'll test the transformed range for NaN and convert to zero.
					r[is.nan(r)]<-0;

					###Is this transformed?
					if(!all(rw==r)){

#						browser()
						######################################
						#equal interal at raw scale
						######################################
						base10raw<-unlist(lapply(2:6,function(e)10^e))
						base10raw<-c(0,base10raw)
						raw<-base10raw[base10raw>min(rw)&base10raw<max(rw)]
						pos<-signif(cal[[j]](raw))


						assign("prefixedChnl",prefixedChnl,tempenv)
						assign("raw",raw,tempenv);
						assign("pos",pos,tempenv);
						eval(expression(axis.labels[[prefixedChnl]]<-list(label=as.character(raw),at=pos)),envir=tempenv);
					}
					return(r);
				}else{

                  #update time range with the real data range
                  if(grepl("[Tt]ime",this_chnl))
                  {
                    timeRange
#                    range(exprs(dataenv$data$ncfs[[sampleName]])[,this_chnl])
                  }else{
                    rawRange[,i]
                  }

				}
			})

#	browser()
	datarange<-t(rbind(datarange[2,]-datarange[1,],datarange))
	datapar<-parameters(get(sampleName,frmEnv))
	pData(datapar)[,c("range","minRange","maxRange")]<-datarange

	eval(substitute(frmEnv$s@parameters<-datapar,list(s=sampleName)))

    tempenv$axis.labels
}

#' Plot gates and associated cell population contained in a \code{GatingHierarchy} or \code{GatingSet}
#'
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
#'  \item{arrange}{ \code{logical} indicating whether to arrange different populations/nodes on the same page via \code{grid.arrange} call.}
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
#'  \item{default.y}{ \code{character} specifiying y channel for xyplot when plotting a 1d gate. Default is "SSC-A".}
#'
#'  \item{type}{ \code{character} either "xyplot" or "densityplot". Default is "xyplot"}
#'
#'  \item{fitGate}{ used to disable behavior of plotting the gate region in 1d densityplot}
#'
#'  \item{strip}{ \code{ligcal} specifies whether to show pop name in strip box,only valid when x is \code{GatingHierarchy}}
#'
#'
#'  \item{raw.scale}{ \code{logical} whether to show the axis in raw(untransformed) scale}
#'  \item{xlim, ylim}{ \code{character} can be either "instrument" or "data" which determines the x, y axis scale
#'                                            either by instrument measurement range or the actual data range.
#'                     or \code{numeric} which specifies customized range.}
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
#' @rdname plotGate-methods
setMethod("plotGate",signature(x="GatingSet",y="missing"),function(x,y,...){
        stop("y is missing!")
		})


setMethod("plotGate",signature(x="GatingSet",y="numeric"),function(x,y,...){

      stop(" 'numeric` indexing is no longer safe. Please use node name instead!")


		})

setMethod("plotGate",signature(x="GatingSet",y="character"),function(x,y,lattice=TRUE,bool=FALSE,merge=TRUE,...){

      
      plotList <- .mergeGates(x[[1]],y,bool,merge)
      if(length(plotList) > 1)
        stop("Too many populations specified in 'y'!Only one panel per sample is supported!")
#				lapply(plotList,function(y){
      y <- plotList[[1]]
      return(.plotGate(x,y,...))
#						})
      
      
    })

##recursively parsing conditional variables
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
#' preporcess the gating tree to prepare for the plotGate
#'
#' @param x a \code{GatingSet}
#' @param y node name
#' @param type \code{character} either 'xyplot' or 'densityplot'
#' @param stats \code{numeric} proportions of cell population. If \code{missing} then extract it from \code{gatingHiearchy}
#' @return a \code{list} containing 'gates', 'xParam','yParam', and 'stats'
#' @importClassesFrom flowCore filters filtersList
#' @importFrom flowCore filters filtersList
.preplot <- function(x, y, type, stats, formula, default.y = "SSC-A"){
  samples <- sampleNames(x)

  isBool <- FALSE
  if(is.list(y))
  {

    curGates<-sapply(samples,function(curSample){

          filters(lapply(y$popIds,function(y)getGate(x[[curSample]],y)))
        },simplify=F)
    curGates<-as(curGates,"filtersList")

    if(missing(stats)){

      stats <- sapply(samples,function(thisSample){
            lapply(y$popIds,function(thisY){
                  curGh <- x[[thisSample]]
                  getProp(curGh, thisY, flowJo = F)
                })
          },simplify = FALSE)
    }

  }else
  {
    curGates<-getGate(x,y)

    if(suppressWarnings(any(is.na(curGates)))){
      message("Can't plot. There is no gate defined for node ",getNodes(x[[1]],,showHidden=TRUE)[y]);
      invisible();
      return(NULL)
    }
    if(missing(stats)){
      stats <- sapply(samples,function(thisSample){
            curGh <- x[[thisSample]]
            getProp(curGh, y,flowJo = F)
          },simplify = FALSE)
    }else
      stats = stats

  }

  if(class(curGates[[1]])=="booleanFilter")
  {
    params<-rev(parameters(getGate(x[[1]],getParent(x[[1]],y))))
    overlay <- sapply(samples,function(curSample)getIndices(x[[curSample]],y), simplify = FALSE)
    curGates <- NULL
    isBool <- TRUE
  }else
  {
    if(class(curGates[[1]])=="filters")
      params<-rev(parameters(curGates[[1]][[1]]))
    else
      params<-rev(parameters(curGates[[1]]))

  }

  if(type == "xyplot")
  {
    if(length(params) == 1)
    {
      chnls <- colnames(getData(x))
      if(is.null(formula)){
        xParam <- params
        y.candidates <- chnls[-match(xParam,chnls)]

        if(default.y%in%y.candidates)
          yParam <- default.y
        else{
          if(!default.y %in% chnls)
            stop("default.y '", default.y, "' is not valid channel name!Try to reset it")
            #pick other channel for y axis
            y.candidates <- y.candidates[!grepl("[Tt]ime", y.candidates)]
            yParam <- y.candidates[1]
            warning("Y axis is set to '", yParam, "' because default.y '",default.y, "' can not be used as y axis!\n To eliminate this warning, set type = 'densityplot' or change the default y channel through 'default.y' ")
        }

      }else{
        forRes <- .formulaParser(formula)
        yParam <- as.character(forRes[["yTerm"]])
        xParam <- as.character(forRes[["xTerm"]])

      }


    }else
    {
      if(is.null(formula)){
        yParam=params[1]
        xParam=params[2]
      }else{
        forRes <- .formulaParser(formula)
        yParam <- as.character(forRes[["yTerm"]])
        xParam <- as.character(forRes[["xTerm"]])
      }


    }

  }else{
    if(length(params) > 1)
      stop("Can't plot 2d gate on densityplot!")
    xParam <- params
    yParam <- NULL
  }


  list(gates = curGates, xParam = as.vector(xParam), yParam = as.vector(yParam), stats = stats, isBool = isBool)
}

#' @param x a \code{GatingSet}
#' @param overlay a list of gate indices or event indices (named by sampleNames(x))
#' @param ... other arguments
#'      params channel names for subsetting the result
#' @return either a list of flowFrame for a flowSet/ncdfFlowSet or NULL
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
#' the actual plotGate engine
#'
#' @param fitGate used to disable behavior of plotting the gate region in 1d densityplot
#' @param overlay either the gate indice list or event indices list
#' @param strip \code{ligcal} specifies whether to show pop name in strip box,only valid when x is \code{GatingHierarchy}
#' @param marker.only \code{ligcal} specifies whether to show both channel and marker names
#' @param path A \code{character} or \code{numeric} scalar passed to \link{getNodes} method
#' @param xlim, ylim \code{character} can be either "instrument" or "data" which determines the x, y axis scale either by instrument measurement range or the actual data range.
#'          or \code{numeric} which specifies customized range.
#' @param ... other arguments passed to .formAxis and flowViz
#' @importMethodsFrom flowCore nrow parameters parameters<-
#' @importMethodsFrom flowViz xyplot densityplot
#' @importFrom RColorBrewer brewer.pal
.plotGate <- function(x, y, formula=NULL, cond=NULL
                      , smooth=FALSE ,type=c("xyplot","densityplot")
                      , main = NULL
                      , fitGate=FALSE
                      , overlay = NULL
                      , overlay.symbol = NULL
                      , key = NULL
                      , stack = FALSE
                      , xbin = 32
                      , stats , default.y = "SSC-A", scales
                      , strip = TRUE
                      , path = "auto"
                      , xlim = "instrument"
                      , ylim = "instrument"
                      , ...){


	type<- match.arg(type)

    #x is either gs or gh, force it to be gs to be compatible with this plotGate engine
    is.gh <- class(x) == "GatingHierarchy"
    if(is.gh){
      x <- new("GatingSet", pointer = x@pointer 
                          , FCSPath = x@FCSPath
                          , data = x@data
                          , flag = x@flag
                          , axis = x@axis
                          , guid = x@guid
                          )[x@name]       
    }

    gh <- x[[1]]

	if(is.list(y))
      popName<-y$parentId
	else
      popName<-getParent(gh, y, path = path)

    #set default title
#    popName <- getNodes(gh, path = path, showHidden = TRUE)[pid]

    default_main <- list(label = popName)
    if(is.gh && strip)
        default_main <- list()
    #update default main if non-null main are specified
    if(is.list(main))
      main <- lattice:::updateList(default_main, main)


    
	#################################
	# setup axis labels and scales
	################################
    parseRes <- .preplot (x, y, type, stats, formula, default.y)

    curGates <- parseRes$gates
    xParam <- parseRes$xParam
    yParam <- parseRes$yParam
    params <- c(yParam,xParam)
    stats <- parseRes$stats

    if(parseRes$isBool){
      if(is.null(overlay))
        overlay <- y
      else
        overlay <- c(overlay, y)
    }
  

#    browser()
        #get data
    #subset on channels to speed up loading data from disk
    parentData <- getData(x, popName, j = params)
    defaultCond <- "name"
    if(is.gh){
      if(strip){
        #rename sample name with popName in order to display it in strip
        sampleNames(parentData) <- popName
        if(!is.null(curGates))
          names(curGates) <- popName
        if(!is.null(stats))
          names(stats) <- popName
      }else
        defaultCond <- NULL #hide strip
    }
    parentFrame <- parentData[[1]]
    #set the smoothing option
#    smooth <- as.logical(ifelse(nrow(parentFrame)<100,TRUE,smooth))


    axisObject <- .formatAxis(gh,parentFrame, xParam, yParam,...)

    #set the formula
    if(is.null(formula))
    {
      formula<-mkformula(params,isChar=TRUE)
      if(!is.null(cond))
        formula<-paste(formula,cond,sep="|")
      formula<-as.formula(formula)
    }

    #use default scales stored in gs if it is not given
    if(missing(scales)){
      scales <- axisObject$scales
    }else
    {
      if(!is.null(scales)){
        scales <- lattice:::updateList(axisObject$scales, scales) #update default lab if non-null lab are specified
      }
    }




    thisCall<-quote(plot(x=formula
                          ,data=parentData
                          ,filter=curGates
                          ,main=main
                          ,...
                          )
                      )
#                      browser()
    #try to customize the x, y limits if requested
    if(!is.numeric(xlim))
    {
      xlim <- match.arg(xlim, c("instrument", "data"))
      if(xlim == "data")
      {
        xlim <- .getRange(parentData, xParam)
      }
    }
      # instrument range is calculated within xyplot::prepanel.xyplot.flowset
      # so there is no need to do it here
    if(is.numeric(xlim)){
      scales$x <- NULL
      thisCall <- as.call(c(as.list(thisCall),list(xlim = xlim)))
    }



    if(!is.numeric(ylim))
    {
      ylim <- match.arg(ylim, c("instrument", "data"))
      if(ylim == "data")
      {
        ylim <- .getRange(parentData, yParam)
      }
    }
    if(is.numeric(ylim)){
      scales$y <- NULL
      thisCall <- as.call(c(as.list(thisCall),list(ylim = ylim)))
    }


    if(!is.null(stats)){
      thisCall <- as.call(c(as.list(thisCall),list(stats = stats)))
    }

    thisCall <- as.call(c(as.list(thisCall),list(scales = scales)))

	if(type=="xyplot")
	{

		#################################
		# calcuate overlay frames
		################################
        if(!is.null(overlay)){
          if(is.null(overlay.symbol)){
#            browser()
            # set symbol color automatically if not given
            nOverlay <- length(overlay)
            overlay.fill <- brewer.pal(9,"Set1")[1:nOverlay]
            names(overlay.fill) <- overlay
            overlay.symbol <- sapply(overlay.fill, function(col)list(fill = col), simplify = FALSE)
          }
          #set legend automatically if it is not given
          if(is.null(key)){
            
             key = list(text = list(names(overlay.symbol), cex = 0.6)
                                , points = list(col = sapply(overlay.symbol, "[[", "fill") 
                                                , pch = 19
                                                , cex = 0.5) 
                          , columns = length(overlay.symbol)
                          , between = 0.3
                          , space = "bottom"
                          , padding.text = 5)
          }
          overlay <- sapply(overlay, function(thisOverlay)getData(x,thisOverlay)[,params])
          
          if(is.gh){
            if(strip){
#              browser()
              #rename sample name with popName in order to display it in strip
            overlay <- sapply(overlay, function(thisOverlay){
                            sampleNames(thisOverlay) <- popName
                            thisOverlay
                      })
           names(overlay.symbol) <- names(overlay)       
          }
        }
      }


		#################################
		# the actual plotting
		################################
#        browser()
        thisCall <- as.call(c(as.list(thisCall)
                            ,list(smooth = smooth
                                  ,overlay = overlay
                                  ,overlay.symbol = overlay.symbol
                                  , key = key
                                  , defaultCond = defaultCond
                                  , xbin = xbin
                                  )
                            )
                          )
        thisCall[[1]]<-quote(xyplot)

	}else
	{
		if(length(params)==1)
		{

          thisCall<-as.call(c(as.list(thisCall)
                               , fitGate = fitGate
                               , stack = stack
                              )
                           )
          thisCall[[1]]<-quote(densityplot)
		}
	}
	return(eval(thisCall))
}



#'  clone a GatingSet
#'
#'   clone a GatingSet
#' @param x A \code{GatingSet}
#' @param ...
#'     ncdfFile = NULL: see \code{\link{clone.ncdfFlowSet}}
#' @details
#'   Note that the regular R assignment operation on a \code{GatingSet} object does not return the copy as
#'   one would normally expect because the \code{GatingSet} contains environment slots (and external pointer for \code{GatingSet}),
#'   which require deep-copying. So make sure to use this clone method in order to make a copy of existing object.
#' @return A copy of a given \code{GatingSet}.
#' @examples
#'   \dontrun{
#'     #G is  a GatingSet
#'     G1<-clone(G)
#'
#'   }
#' @aliases clone clone-methods clone,GatingSet-method
#' @exportMethod clone
setGeneric("clone", function(x,...)standardGeneric("clone"))
setMethod("clone",c("GatingSet"),function(x,...){

			clone <- x
			#clone c structure
			message("cloning tree structure...")
			clone@pointer <- .Call("R_CloneGatingSet",x@pointer,sampleNames(x))
            clone@guid <- .uuid_gen()

			#deep copying flow Data
			message("cloning flow data...")
			fs <- flowData(x)
			if(isNcdf(x))
				fs_clone<-ncdfFlow::clone.ncdfFlowSet(fs,isEmpty=FALSE,isNew=TRUE,...)
			else
				fs_clone<-flowCore:::copyFlowSet(fs)

			flowData(clone) <- fs_clone

			message("GatingSet cloned!")
			clone
		})


setGeneric("recompute", function(x,...)standardGeneric("recompute"))
#' Compute the cell events by the gates stored within the gating tree
#' 
#' Compute each cell event to see if it falls into the gate stored within the gating tree
#' and store the result as cell count.
#' 
#' It is usually used immediately after \link{add} or \link{setGate} calls. 
#'  
#' @param x \code{GatingSet}
#' @param ... other arguments
#'          y \code{character} node name or node path 
#'          alwaysLoadData \code{logical} 
#'                  specifies whether to load the flow raw data for gating
#'                  for boolean gates, sometime it is more efficient to skip loading the raw data if all the reference nodes and parent are already gates
#'                  Default 'FALSE' will check the parent node and reference to determine whether to load the data
#'                  but this check may not be sufficient since  the further upstream ancestor nodes may not be gated yet
#'                  In that case, we allow the gating to be failed and prompt user to recompute those nodes explictily
#'                  When TRUE, then it forces data to be loaded to guarantee the gating process to be uninterrupted
#'                  , yet may at the cost of unnecessary data IO
#' @aliases recompute
#' @rdname recompute
#' @export
setMethod("recompute",c("GatingSet"),function(x, ...){
			.recompute(x, ...)

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
.recompute <- function(x,y = "root", alwaysLoadData = FALSE, verbose = FALSE, leaf.bool = TRUE){
  
  if(y == "root")
    alwaysLoadData <- TRUE #skip the checking to save time when start from root
  
  extend_val <- 0
  ignore_case <- FALSE
  gains <- NULL
  lapply(x,function(gh){


        sampleName<-sampleNames(gh)
        if(verbose)
          message(paste("gating",sampleName,"..."))
        else
          message(".", appendLF = FALSE)


        # Ideally, we want to track back to all ancesters and references to check if they are already gated
        # in order to determine whether the raw data is needed
        # but for the sake of speed, we only check the parent and reference node
        # of the boolGate at the moment
        # if the further upstream ancester nodes are not gated yet, which will fail the gating
        # since we are passing the empty dummy data, we will simply throw the error and prompt user
        # to recompute these upstream gates explicitly
        if(alwaysLoadData)
          isloadData <- TRUE
        else{
          isAllBoolGate <- all(sapply(y,.isBoolGate, x = gh))
          if(isAllBoolGate){
            isloadData <- all(sapply(y, function(i){

                      pid <- getParent(gh, i)
                      isParentGated <- isGated(gh, pid)
                      bf <- getGate(gh, i)
                      refNodes <- filterObject(bf)$refs
                      isRefGated <- all(sapply(refNodes, isGated, obj = gh))
                      !(isParentGated&&isRefGated)
                    }))


          }else
            isloadData <- TRUE
        }


        if(isloadData){
          data <- getData(gh)
          mat <- exprs(data)
        }else{
          mat <- matrix(nrow = 0, ncol = 1, dimnames = list(NULL, "dummy"))
        }


        lapply(y,function(i){
              nodeID <- .getNodeInd(gh, i)
              nodeInd <- as.integer(nodeID)-1
              recompute <- TRUE
#              browser()
              res <- try(.Call("R_gating",gh@pointer,mat,sampleName,gains,nodeInd,recompute,extend_val, ignore_case, leaf.bool), silent = TRUE)
              if(class(res) == "try-error"){
                if(!isloadData&&grepl("not found in flowData", res))
                  stop("Found ungated upstream population. Set 'alwaysLoadData = TRUE' for 'recompute' method, and try again!")
                else
                  stop(res)
              }

            })



      })
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
      sampleNames(flowData(object))
    })
#' @name sampleNames
#' @param value \code{character} new sample names
#' @usage \S4method{sampleNames}{GatingSet}(object) <- value
#' @aliases 
#' sampleNames<-
#' sampleNames<-,GatingSet-method
#' sampleNames<-,GatingSet,ANY-method
#' @rdname sampleNames
#' @export
setReplaceMethod("sampleNames",
    signature=signature(object="GatingSet"),
    definition=function(object, value)
    {
      oldNames <- sampleNames(object)
      #update c++ data structure
      mapply(oldNames,value, FUN = function(oldName, newName){
            .Call("R_setSample", object@pointer, oldName, newName)
      })

      #update data
      fs <- flowData(object)
      sampleNames(fs) <- value
      flowData(object) <- fs

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
#' @importMethodsFrom flowCore Subset
setMethod("getData",signature(obj="GatingSet",y="character"),function(obj,y, ...){

      this_data <- getData(obj, ...)
      if(y == "root"){
        this_data
      }else{
        #subset by indices
        indices<-lapply(obj,getIndices,y)
        if(class(this_data) == "ncdfFlowSet")
          this_data <- Subset(this_data,indices, validityCheck = FALSE)
        else
          this_data <- Subset(this_data,indices)
        this_data
      }
      

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
        x@data
    })
#' @name flowData
#' @param value The replacement \code{flowSet} or \code{ncdfFlowSet} object
#' @usage \S4method{flowData}{GatingSet}(x) <- value
#' @aliases 
#' flowData<-
#' flowData<-,GatingSet-method
#' @rdname flowData
#' @export
setReplaceMethod("flowData",signature(x="GatingSet"),function(x,value){

      x@data <- value
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
			fs<-flowData(object)
			rownames(value)<-value$name
			pData(fs)<-value
			varM<-varMetadata(phenoData(fs))
			varM[-1,]<-rownames(varM)[-1]
			varMetadata(phenoData(fs))<-varM
			flowData(object)<-fs
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
            #convert non-character indices to character
            if(extends(class(i), "numeric")||class(i) == "logical"){
              i <- sampleNames(x)[i]
            }

            #copy the R structure
            clone <- x
            clone@axis <- clone@axis[i]
            #subsetting data
			fs <- flowData(clone)[i]


            #update the data for clone
            flowData(clone) <- fs
            clone@guid <- .uuid_gen()
			return(clone);
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
  
  x[as.character(pd[r, "name"])]
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
  level <- .Call("R_getLogLevel")
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
  .Call("R_setLogLevel", as.integer(match(level, valid_levels) - 1))
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
#      as(x[i], "GatingHierarchy")
      #new takes less time than as method
      new("GatingHierarchy", pointer = x@pointer 
                            , FCSPath = x@FCSPath
                            , data = x@data
                            , flag = x@flag
                            , axis = x@axis
                            , guid = x@guid
                            , name = i)                             
      
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
#' getPopStats is more useful than getPop. Returns a table of population statistics for all populations in a \code{GatingHierarchy}/\code{GatingSet}. Includes the flowJo counts, flowCore counts and frequencies.
#' getTotal returns the total number of events in the gate defined in the GatingHierarchy object
#' @param x A \code{GatingHierarchy} or \code{GatingSet}
#' @param statistic \code{character} specifies the type of population statistics to extract.(only valid when format is "wide"). Either "freq" or "count" is currently supported.
#' @param flowJo \code{logical} indicating whether the statistics come from FlowJo (if parsed from xml workspace) or from flowCore.
#' @param path \code{character} see \link{getNodes}
#' @param format \code{character} value of c("wide", "long") specifing whether to origanize the output in long or wide format  
#' @param subpopulations \code{character} vector to specify a subset of populations to return. (only valid when format is "long")  
#' @param ... Additional arguments passed to \link{getNodes}
#'
#' @details
#' getPopStats returns a table population statistics for all populations in the gating hierarchy. The output is useful for verifying that the import was successful, if the flowJo and flowCore derived counts don't differ much (i.e. if they have a small coefficient of variation.) for a GatingSet, returns a matrix of proportions for all populations and all samples
#' getProp returns the proportion of cells in the gate, relative to its parent.
#' getTotal returns the total number of events included in this gate. The contents of "thisTot" variable in the "metadata" environment of the \code{nodeData} element associated with the gating tree and gate / population.
#'
#' @return
#' getPopStats returns a \code{data.frame} with columns for the population name, flowJo derived counts, flowCore derived counts, and the population proportions (relative to their parent pouplation).
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
    function(x, statistic = c("freq", "count"), flowJo = FALSE, subpopulations = NULL, format = c("wide", "long"), path = "auto", ...) {

      # Based on the choice of statistic, the population statistics are returned for
      # each Gating Hierarchy within the GatingSet.
      statistic <- match.arg(statistic)
      format <- match.arg(format)
      path <- match.arg(path, c("full", "auto"))
       
      if(format == "long"){
        
        if(is.null(subpopulations))
          subpopulations <- getNodes(x, path = path, ...)[-1]
        
        pop_stats <- .getPopCounts(x@pointer, sampleNames(x), subpopulations, flowJo, path == "full")
        pop_stats <- data.table(name = pop_stats[["name"]]
                              , Population = pop_stats[["Population"]]
                              , Parent = pop_stats[["Parent"]]
                              , Count = pop_stats[["Count"]]
                              , ParentCount = pop_stats[["ParentCount"]]
                            )
        
      }else{
        
      
        # The 'flowJo' flag determines whether the 'flowJo' or 'flowCore' statistics
        # are returned.
        if (flowJo) {
          statistic <- paste("flowJo", statistic, sep = ".")
        } else {
          statistic <- paste("flowCore", statistic, sep = ".")
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
.computeCV <- function(x, ...){

  #columns are populations
  #rows are samples

  statList <- lapply(x,function(gh){
        thisStat <- getPopStats(gh, ...)
        thisStat
      })
  
  cv <- do.call(rbind
              ,lapply(statList,function(x){
                    
                    res <- apply(x[,list(flowJo.count,flowCore.count)],1,function(x){
                          cv <- IQR(x)/median(x)
                          ifelse(is.nan(cv),0,cv)
                        })
                    names(res) <- rownames(x)
                    res
                  })
             )
               
  cv

}
#' Plot the coefficient of variation between flowJo and flowCore population statistics for each population in a gating hierarchy.
#'
#' This function plots the coefficient of variation calculated between the flowJo population statistics and the flowCore population statistics for each population in a gating hierarchy extracted from a flowJoWorkspace.
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
#' @importFrom latticeExtra ggplot2like
setMethod("plotPopCV","GatingSet",function(x, scales = list(x = list(rot = 90)), path = "auto",...){
      cv <- .computeCV(x, path = path)
      #flatten, generate levels for samples.
      nr<-nrow(cv)
      nc<-ncol(cv)
      populations<-gl(nc,nr,labels=as.character(colnames(cv)))
      samples<-as.vector(t(matrix(gl(nr,nc,labels=basename(as.character(rownames(cv)))),nrow=nc)))
      cv<-data.frame(cv=as.vector(cv),samples=samples,populations=populations)

      return(barchart(cv~populations|samples,cv,..., scale = scales, par.settings = ggplot2like));
    })

#' @rdname keyword
#' @export
setMethod("keyword",c("GatingSet", "missing"),function(object,keyword = "missing"){
        lapply(object, flowCore::keyword)

    })
#' @rdname keyword
#' @export
setMethod("keyword",c("GatingSet","character"),function(object,keyword){
      tmp<-data.frame(unlist(lapply(object,function(x)keyword(x,keyword)),use.names=FALSE));
      tmp<-data.frame(matrix(tmp[[1]],ncol=length(keyword),byrow=T))
      colnames(tmp)<-keyword
      tmp
    })

### Extract single-cell data by boooean expansion. For COMPASS

#' routine to return the indices by specify boolean combination of reference nodes:
#'
#' It adds the boolean gates and does the gating on the fly, and
#' return the indices associated with that bool gate, and remove the bool gate
#' the typical use case would be extracting any-cytokine-expressed cells
#' 
#' @param obj \code{GatingSet}
#' @param y a quoted expression.
#' @examples
#' \dontrun{
#'
#'	getIndices(gs,quote(`4+/TNFa+|4+/IL2+`))
#'
#'}
#' @export
setMethod("getIndices",signature=c("GatingSet","name"),function(obj, y){

  bf <- eval(substitute(booleanFilter(v),list(v=y)))
  gh <- obj[[1]]

  suppressMessages({
    suppressWarnings(
      id <- add(obj,bf)
    )

    allNodes <- getNodes(gh, showHidden = TRUE)
    this_node <- allNodes[id]


    res <-try(recompute(obj,this_node),silent=T)
  })


  if(class(res)=="try-error"){
    Rm(this_node,obj)
    stop(res)
  }else{
    this_ind <- lapply(obj,function(this_gh)getIndices(this_gh,this_node))
    Rm(this_node,obj)
    this_ind
  }

})
#' Return the single-cell matrix of 1/0 dichotomized expression
#' @param gh \code{GatingHierarchy} object
#' @param y \code{character} node name
#' @export 
getIndiceMat <- function(gh,y){
  strExpr <- as.character(y)
  nodes <- strsplit(strExpr,split="\\|")[[1]]
  .getIndiceMat(gh, sampleNames(gh), nodes)  
  
}

.getIndiceMat <- function(gs, thisSample, nodes){

  #extract logical indices for each cytokine gate
  indice_list <- sapply(nodes,function(this_node).Call("R_getIndices"
            , gs@pointer
            , thisSample
            , this_node)
      ,simplify = FALSE)
  
  #construct the indice matrix
  do.call(cbind, indice_list)
#  as.data.table(indice_list)
}
#' create mapping between pops and channels
#' 
#' The goal is to match the marker provided by user up to the flowFrame
#' to return the accurate channel info for indexing the flow data 
#' because user might give the short form of either  'name' or 'desc' of flowFrame based on swap argument.
#' 
#' @param this_pd \code{data.frame} extraced from flowFrame object to provide the channel and marker info
#' @param popNames \code{character} node names in gating tree
#' @param popo_marker_list \code{list} contains the node-to-marker provided by user 
#' 
.getPopChnlMapping <- function(this_pd, popNames, pop_marker_list, swap = FALSE){
    
  datSrc <- ifelse(swap, "name", "desc")
  this_pd[, datSrc] <- as.vector(this_pd[, datSrc])
  

  #parse the markers of interest from pop names
  markers_selected <- sapply(popNames,function(this_pop){
    this_pops <- strsplit(split="/", this_pop, fixed=TRUE)[[1]]
    #get the terminal node
    term_pop <- this_pops[length(this_pops)]
    term_pop
  },USE.NAMES=FALSE)

  #match to the pdata of flow frame
  all_markers <- this_pd[,datSrc]

  all_markers[is.na(all_markers)] <- "NA"
  is_matched <- sapply(all_markers,function(this_marker){
    
    ##using the marker name provided by arguments by default
    if(is.null(pop_marker_list))
      matchCount <- 0
    else{
      this_matched <- sapply(pop_marker_list, function(i)grepl(pattern = i, x = this_marker, fixed=TRUE))
      
      matchCount <- length(which(this_matched))  
    }
    

    if(matchCount > 0){
      if(matchCount > 1)
      {
        #more than one matched, then do the exact match
            this_matched <- sapply(pop_marker_list, function(i)match(i, this_marker))
            this_matched <- !is.na(this_matched)
            matchCount <- length(which(this_matched))
        if(matchCount > 1)
          stop(this_marker, " is matched with more than one populations")
      }

      res <- TRUE
      names(res) <- names(pop_marker_list)[this_matched]
    }else{
      #if not given by user, then try to match the name extracted from pop name
      this_matched <- sapply(markers_selected, function(i)grepl(pattern = i, x = this_marker, fixed=TRUE))
#      browser()
      matchCount <- length(which(this_matched))
      if(matchCount > 1){
        stop("multiple populations mached to:", this_marker)
      }else if(matchCount == 0){
        res <- FALSE
      }else{
        res <- TRUE
        names(res) <- popNames[this_matched]
      }
    }

    res
  }, USE.NAMES=FALSE)

  pop_matched <- is_matched[is_matched]
  matched_names <- names(pop_matched)
  sub_match_ind <- match(popNames, matched_names)
  no_match <- is.na(sub_match_ind)
  if(any(no_match)){
    stop("No markers in flow data matches ", "Populations:", paste(popNames[no_match],collapse="; "))

  }

  cbind(pop=as.character(matched_names[sub_match_ind]),this_pd[is_matched,c("name", "desc")][sub_match_ind,])



}

#' Return the cell events data that express in any of the single populations defined in \code{y}
#' 
#' Returns a list of matrix containing the events that expressed in any one of the populations efined in \code{y}
#' 
#' @param x A \code{GatingSet} or \code{GatingSetList} object .
#' @param nodes \code{character} vector specifying different cell populations
#' @param map mapping node names (as specified in the gating hierarchy of the gating set) to channel 
#'                         names (as specified in either the \code{desc} or \code{name} 
#'                          columns of the parameters of the associated \code{flowFrame}s 
#'                          in the \code{GatingSet}).
#' @param swap \code{logical} indicates whether channels and markers of flow data are swapped.
#' @param threshold \code{logical} indicates whether to threshold the flow data by setting intensity value to zero when it is below the gate threshold.
#' @return A \code{list} of \code{numerci matrices}
#' @aliases getSingleCellExpression
#' @author Mike Jiang \email{wjiang2@@fhcrc.org}
#' @seealso \code{\link{getIndices}}  \code{\link{getPopStats}}
#' @examples \dontrun{
#'   #G is a GatingSet
#' 	geData(G,3)
#' 	res <- getSingleCellExpression(gs[1], c("4+/TNFa+", "4+/IL2+"))
#' 	res[[1]]
#' 	res <- getSingleCellExpression(gs[1], c("4+/TNFa+", "4+/IL2+") , list("4+/TNFa+" = "TNFa", "4+/IL2+" = "IL2"))
#' }
#' @rdname getSingleCellExpression
#' @export
setMethod("getSingleCellExpression",signature=c("GatingSet","character"),function(x, nodes, map = NULL, swap = FALSE, threshold = TRUE){
  datSrc <- ifelse(swap, "name", "desc")
  fs <- getData(x)
  sapply(sampleNames(x),function(sample){
      
      message(".", appendLF = FALSE)
      
      fr <- fs[[sample, use.exprs = FALSE]] 
      this_pd <- pData(parameters(fr))  
      #get pop vs channel mapping
      pop_chnl <- .getPopChnlMapping(this_pd, nodes, map, swap = swap)
      chnls <- as.character(pop_chnl[,"name"])
      pops <-  as.character(pop_chnl[,"pop"])
      
      markers <- as.character(pop_chnl[, datSrc])
    
      nodeIds <- sapply(pops, function(pop){
            ind <- .Call("R_getNodeID",x@pointer,sample, pop)
            ind + 1 # convert to R index
          })
      nodeIds <- as.integer(nodeIds) - 1
      data <- fs[[sample, chnls]]
      data <- exprs(data)
      
      data <- .Call("R_getSingleCellExpression", x@pointer, sample, nodeIds, data, markers, threshold)
      data
          

  }, simplify = FALSE)     

})

#' insert a dummy gate to the GatingSet
#' 
#' Is is useful trick to make the tree structure of GatingSet same with other so that
#' they can be combined into a 'GatingSetList' object.
#' 
#' @param gs \code{GatingSet} to work with
#' @param gate \code{filter} a dummy gate to be inserted, its 'filterId' will be used as the population name
#' @param parent \code{character} full path of parent node where the new dummy gate to be added to
#' @param children \code{character} full path of chidlren nodes that the new dummy gate to be parent of
#' @return  a new \code{GatingSet} object with the new gate added but share the same flow data with the input 'GatingSet'
#' @examples 
#' \dontrun{
#' #construct a dummy singlet gate 
#'  dummyGate <- rectangleGate("FSC-A" = c(-Inf, Inf), "FSC-H" = c(-Inf, Inf), filterId = "singlets")
#' #insert it between the 'not debris" node and "lymph" node
#'  gs_clone <- insertGate(gs, dummyGate, "not debris", "lymph") 
#' }
insertGate <- function(gs, gate, parent, children){
  dummyNode <- gate@filterId
  nodes <- getNodes(gs)
  dummyPath <- file.path(parent, dummyNode)
  if(any(grepl(dummyPath, nodes)))
    stop(dummyPath, " already exists!")
  
  #copy the entire tree structure
  message("cloning tree structure...")
  clone <- gs
  clone@pointer <- .Call("R_CloneGatingSet",gs@pointer,sampleNames(gs))
  #remove the old children
  lapply(children, function(child)Rm(child, clone))
  
  # add the new node
  add(clone, gate, parent = parent)
  #copy children
  ancester <- getNodes(clone)
  nodesToadd <- nodes[!nodes%in%ancester]
  
  lapply(nodesToadd, function(node){
        
        if(node%in%children)
          thisParent <- dummyNode #add the old direct children to the new dummy node
        else{
          #copy the other nodes to its parent
          oldParent <- getParent(gs, node)
          #match to the new parent
          thisNodes <- getNodes(clone)
          thisParent <- thisNodes[match(oldParent, gsub(paste0("/", dummyNode), "", thisNodes))] 
        }
        popName <- basename(node)
        lapply(sampleNames(gs),function(sn){
              gh <- gs[[sn]]
              gate <- getGate(gh, node)
              negated <- flowWorkspace:::isNegated(gh, node)
              add(clone[[sn]], gate, name = popName, parent = thisParent, negated = negated)      
            })  
        
      })
  recompute(clone)
  clone
}

#' tranform the flow data asssociated with the GatingSet
#' 
#' The transformation functions are saved in the GatingSet and can be retrieved by \link{getTransformations}.
#' Currently only flowJo-type biexponential transformation(either returned by \link{getTransformations} or constructed by \link{flowJoTrans})
#' is supported. 
#' 
#' @param _data \code{GatingSet} or \code{GatingSetList}
#' @param ... expect a \code{transformList} object
#' 
#' @return a \code{GatingSet} or \code{GatingSetList} object with the underling flow data transformed.
#' @examples
#' \dontrun{
#' #construct biexponential transformation function
#' biexpTrans <- flowJoTrans(channelRange=4096, maxValue=262144, pos=4.5,neg=0, widthBasis=-10)
#' #make a transformList object
#' transFuns <- transformList(chnls, biexpTrans)
#' #add it to GatingSet
#' gs_trans <- transform(gs, transFuns)

#' }   
#' @export
#' @rdname transform 
#' @importMethodsFrom ncdfFlow transform
setMethod("transform",
    signature = signature(`_data` = "GatingSet"),
    definition = function(`_data`, ...)
    {
      dots <- list(...)
      if(length(dots) == 0)
        stop("expect the second argument as a 'transformList' object!")
      transList <- dots[[1]]
      gs <- `_data`
      if(class(transList) != "transformList")
        stop("expect the second argument as a 'transformList' object!")
      flowWorkspace:::.addTrans(gs@pointer, transList)  
      message("transforming the flow data...")
      fs <- getData(gs)
     
      suppressMessages(fs_trans <- transform(fs, transList))
      flowData(gs) <- fs_trans
      gs
    })

