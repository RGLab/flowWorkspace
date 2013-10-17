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
save_gs<-function(G,path,overwrite = FALSE, cdf = c("copy","move","skip","symlink","link"), ...){
#  browser()
  cdf <- match.arg(cdf)
  guid <- G@guid
  if(length(guid) == 0){
    G@guid <- .uuid_gen()
    guid <- G@guid
  }
  rds_toSave <- paste(guid,"rds",sep=".")
  dat_toSave <- paste(guid,"dat",sep=".")
  
  if(file.exists(path)){
    path <- normalizePath(path,mustWork = TRUE)
    if(overwrite){
      this_files <- list.files(path)
      #validity check for non-empty folder
      if(length(this_files)!=0)
      {
        rds_ind <- grep("\\.rds$",this_files)
        dat_ind <- grep("\\.dat$",this_files)
        
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
  invisible(.save_gs(G=G,path = path, cdf = cdf, ...))
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
.save_gs <- function(G,path, cdf = c("copy","move","skip","symlink","link")){
    
#    browser()
    cdf <- match.arg(cdf)
    
    if(!file.exists(path))
      stop("Folder '",path, "' does not exist!")
    #generate uuid for the legacy GatingSet Object
    if(length(G@guid)==0){
      G@guid <- .uuid_gen()
    }
    guid <- G@guid
    
    rds.file<-file.path(path,paste(guid,"rds",sep="."))
    dat.file<-file.path(path,paste(guid,"dat",sep="."))
  
    
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
    .Call("R_saveGatingSet",G@pointer,dat.file)
    
    message("saving R object...")
    saveRDS(G,rds.file)
    
    filestoSave
  
}
#' unserialization functions to be called by wrapper APIs
.load_gs <- function(output,files){
      dat.file<-file.path(output,files[grep(".dat$",files)])
      rds.file<-file.path(output,files[grep(".rds$",files)])
      
      nc.file<-file.path(output,files[grep(".nc$|.nc.trans$",files)])
    #   browser()
      if(length(dat.file)==0)
        stop(".dat file missing in ",file)
      if(length(dat.file)>1)
        stop("multiple .dat files found in ",file)
      if(length(rds.file)==0)
        stop(".rds file missing in ",file)
      if(length(rds.file)>1)
        stop("multiple .rds files found in ",file)
      
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
      
#      browser()
      guid <- try(slot(gs,"guid"),silent=T)
      if(class(guid)=="try-error"){
        #generate the guid for the legacy archive
        gs@guid <- .uuid_gen()
      }
      
      message("loading tree object...")
      gs@pointer<-.Call("R_loadGatingSet",dat.file)
      
      
      if(isNcdf(gs))
      {
        if(length(nc.file)==0)
          stop(".nc file missing in ",file)
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



.parseWorkspace<-function(xmlFileName,sampleIDs,execute,path,dMode,isNcdf,includeGates,sampNloc="keyword",xmlParserOption, ...){


	message("calling c++ parser...")
#	browser()
	time1<-Sys.time()
	G <- GatingSet(x = xmlFileName
                  , y = sampleIDs
                  , includeGates = includeGates
                  , sampNloc=sampNloc
                  , xmlParserOption = xmlParserOption
                  , dMode=dMode
                  )

	message("c++ parsing done!")
	samples<-.Call("R_getSamples",G@pointer)
	
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
		
	}else
	{
		files<-samples
	}
    G@FCSPath <- dataPaths
    
	G<-.addGatingHierarchies(G,files,execute,isNcdf,...)


	message("done!")


	G	
}

#' constructors for GatingSet
#' 
#' construct object from existing gating hierarchy(gating template) and flow data
#' 
#' @rdname GatingSet-methods
#' @aliases
#' GatingSet,GatingHierarchy,character-method
setMethod("GatingSet", c("GatingHierarchy", "character"), function(x, y, path=".", isNcdf=FALSE, dMode=1, ...){
			
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
			Object@pointer <- .Call("R_NewGatingSet",x@pointer,getSample(x),samples,as.integer(dMode))
            Object@guid <- .uuid_gen()
            Object@FCSPath <- dataPaths
			Object<-.addGatingHierarchies(Object,files,execute=TRUE,isNcdf=isNcdf,...)
            message("done!")
			return(Object)
		})
   

#' constructing gating set
#' @param prefix a \code{logical} flag indicates whether the colnames needs to be updated with prefix(e.g. "<>" or "comp") specified by compensations
#' @param ignore.case a \code{logical} flag indicates whether the colnames(channel names) matching needs to be case sensitive (e.g. compensation, gating..) 
#' @importMethodsFrom flowCore colnames colnames<- compensate spillover sampleNames
#' @importFrom flowCore compensation read.FCS read.FCSheader read.flowSet
#' @importClassesFrom flowCore flowFrame flowSet
.addGatingHierarchies <- function(G,files,execute,isNcdf,compensation=NULL,wsversion = -1,extend_val = 0, prefix = TRUE, ignore.case = FALSE, ...){
	
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
	}
	
	#global variable storing prefixed colnames
    tempenv<-new.env(parent = emptyenv())	
    prefixColNames <- NULL
    assign("prefixColNames",NULL,tempenv)
    
	axis <- lapply(files,function(file,tempenv){
		
        #get global variable
        prefixColNames <- tempenv$prefixColNames
        
		sampleName<-basename(file)
        
		
		#gating (including loading data,compensating,transforming and the actual gating)
		if(execute)
		{
			
			message("loading data: ",file);
			if(isNcdf)
				data <- read.FCS(file)[,colnames(fs)]
			else
				data <- fs[[sampleName]]
             
            cnd <- colnames(data)
#            browser()
            #alter colnames(replace "/" with "_") for flowJo X
            if(wsversion == "1.8"){
                colnames(data) <- gsub("/","_",cnd)
                cnd<-colnames(data)
            
            }
              
			##################################
			#Compensating the data
			##################################
			comp <- .Call("R_getCompensation", G@pointer, sampleName)
			cid <- comp$cid
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
				nm<-comp$comment
				
				if(grepl("Acquisition-defined",nm)){
					###Code to compensate the sample using the acquisition defined compensation matrices.
					message("Compensating with Acquisition defined compensation matrix");
					#browser()
					if(is.null(compensation))
					{
						compobj<-compensation(spillover(data)$SPILL)
						
					}else
					{
						compobj<-compensation
						
					}
					
					res<-try(compensate(data,compobj),silent=TRUE)
					if(inherits(res,"try-error")){
						message("Data is probably stored already compensated");
					}else{
						data<-res
						rm(res);
						
					}
					
				}
				
			}
			if(cid!="-2")
			{
#				browser()
				#get prefix if it is not set yet
                if(is.null(prefixColNames)&&prefix){
                  
                  if(is.null(cnd)){
                    cnd <- as.vector(parameters(data)@data$name)
                  }
                  prefixColNames <- cnd
                  
                  wh <- match(parameters(compobj), prefixColNames)
                  
                  prefixColNames[wh] <- paste(comp$prefix,parameters(compobj),comp$suffix,sep="")
                  
                    
                }
              }
            ##################################
            #transforming and gating
            ##################################
            message(paste("gating ..."))
            #stop using gating API of cdf-version because c++ doesn't store the view of ncdfFlowSet anymore
            mat <- data@exprs #using @ is faster than exprs()
            #get gains from keywords
            this_pd <- pData(parameters(data))
            paramIDs <- rownames(pData(parameters(data)))
            key_names <- paste(paramIDs,"G",sep="")
            kw <- keyword(data)
            if(as.numeric(kw[["FCSversion"]])>=3){
              kw_gains <- kw[key_names]
              gains <- as.numeric(kw_gains)                      
            }else{
              gains <- rep(1,length(paramIDs))
            }
            
            names(gains) <- this_pd$name
            
            #update colnames in order for the gating to find right dims
            if(!is.null(prefixColNames)){
              colnames(mat) <- prefixColNames  
            }
            recompute <- FALSE
            nodeInd <- 0
            .Call("R_gating",G@pointer, mat, sampleName, gains, nodeInd, recompute, extend_val, ignore.case)
#            browser()
            #restore the non-prefixed colnames for updating data in fs with [[<- 
            #since colnames(fs) is not udpated yet.
            if(!is.null(prefixColNames)){
              #restore the orig colnames(replace "_" with "/") for flowJo X
              if(wsversion == "1.8"){
                cnd <- gsub("_","/",cnd)
                colnames(data) <- cnd #restore colnames for flowFrame as well for flowJo vX 
              }
              colnames(mat) <- cnd
            }
            
            data@exprs<-mat #circumvent the validity check of exprs<- to speed up
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
            axis.labels <- .transformRange(G,sampleName,wsversion,fs@frames,timeRange = tRg)

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
		flowData(G) <- fs
		
		
	}
	G
}


#' transform the range slot and construct axis label and pos for the plotting
#' @param gh \code{GatingHierarchy}
#' @param wsversion \code{character} flowJo workspace version
#' @param frmEnv \code{environment} point to the \code{frames} slot of the original \code{flowSet}
#' @param timeRange \code{numeric} vector specifying the range for 'time' channel
#' 
#' @return 
#' a \code{list} of axis labels and positions. Also, the \code{range} slot of \code{flowFrame} stored in \code{frmEnv} are transformed as an side effect.
.transformRange <- function(G,sampleName, wsversion,frmEnv, timeRange = NULL){
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
              if(wsversion == "1.8"){
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
#'  \item{bool}{\code{logical} specifying whether to plot boolean gates.}
#'  \item{arrange.main}{\code{character} The title of the main page of the plot. Default is the sample name. Only valid when \code{x} is GatingHierarchy}
#'  \item{arrange}{\code{logical} indicating whether to arrange different populations/nodes on the same page via \code{grid.arrange} call.}
#'  \item{merge}{\code{logical} indicating whether to draw multiple gates on the same plot if these gates share the same parent population and same x,y dimensions/parameters;}
#'  \item{gpar}{\code{list} of grid parameters passed to \code{\link{grid.layout}};}
#'  \item{lattice}{\code{logical} indicating whether to draw one node/gate on multiple samples on the same page through lattice plot;} 
#'  \item{formula}{\code{formula} a formula passed to \code{xyplot} function of \code{flowViz}, by default it is NULL, which means the formula is generated according to the x,y parameters associated with gate.}
#'  \item{cond}{\code{character} the conditioning variable to be passed to lattice plot.}
#'  \item{overlay}{\code{numeric} scalar indicating the index of a gate/populationwithin the \code{GatingHierarchy} or a \code{logical} vector that indicates the cell event indices representing a sub-cell population. This cell population is going to be plotted on top of the existing gates(defined by \code{y} argument) as an overlay.}
#'  \item{default.y}{\code{character} specifiying y channel for xyplot when plotting a 1d gate. Default is "SSC-A".}
#'  \item{type}{\code{character} either "xyplot" or "densityplot". Default is "xyplot"}
#'  \item{...}{The other additional arguments to be passed to \link[flowViz]{xyplot}.}
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

			
            y <- getNodes(x[[1]])
            y <- setdiff(y,"root")
            
			plotGate(x,y,...)
			
		})

    
setMethod("plotGate",signature(x="GatingSet",y="numeric"),function(x,y,lattice=TRUE,bool=FALSE,merge=TRUE,...){
			if(lattice)
			{
				plotList<-.mergeGates(x[[1]],y,bool,merge)
				
				lapply(plotList,function(y){
							
							return(.plotGate(x,y,...))
						})
				
				
				
			}else
			{
				for(i in 1:length(x))
					plotGate(x[[i]],y,bool=bool,merge=merge,...)
			}
			
		})
    
setMethod("plotGate",signature(x="GatingSet",y="character"),function(x,y,...){

      ind <- sapply(y, function(i).getNodeInd(x[[1]],i))
      plotGate(x,ind,...)
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
#' @param y gate index
#' @param type \code{character} either 'xyplot' or 'densityplot'
#' @param stats \code{numeric} proportions of cell population. If \code{missing} then extract it from \code{gatingHiearchy}
#' @return a \code{list} containing 'gates', 'xParam','yParam', and 'stats'
#' @importClassesFrom flowCore filters filtersList
#' @importFrom flowCore filters filtersList
.preplot <- function(x, y, type, stats, formula, default.y = "SSC-A"){
  samples <- sampleNames(x)
#  browser()
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
                  getProp(curGh,getNodes(curGh,showHidden=TRUE)[thisY],flowJo = F)
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
            getProp(curGh,getNodes(curGh,showHidden=TRUE)[y],flowJo = F)
          },simplify = FALSE) 
    }else
      stats = stats
    
  }
  
  if(class(curGates[[1]])=="booleanFilter")
  {
    params<-rev(parameters(getGate(x[[1]],getParent(x[[1]],y))))
    overlay<-sapply(samples,function(curSample)getIndices(x[[curSample]],y))
    curGates<-NULL
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
      chnls <- colnames(flowData(x))
      if(is.null(formula)){
        xParam <- params
        y.candidates <- chnls[-match(xParam,chnls)]
        
        if(default.y%in%y.candidates)
          yParam <- default.y
        else{
          if(!default.y %in% chnls)
            #pick other channel for y axis
            yParam <- y.candidates[1]
            warning("Y axis is set to '", yParam, "' because '",default.y, "' is not found in flow data!\n To eliminate this warning, set type = 'densityplot' or change the default y channel through 'default.y' ")
        }
        
      }else{
        forRes <- .formulaParser(formula)
        yParam <- as.character(forRes[["yTerm"]])
        xParam <- as.character(forRes[["xTerm"]])
        
      }
      

    }else
    {
      yParam=params[1]
      xParam=params[2]
      
    }
    
  }else{
    xParam <- params
    yParam <- NULL
  }
  
  
  list(gates = curGates, xParam = xParam, yParam = yParam, stats = stats)
}

.getOverlay <- function(x, overlay, params){
  if(!is.null(overlay))
  {
    #gate indices
    if(class(overlay)=="logical")
      overlay<-Subset(getData(x),overlay)[,params]
    else{
      if(length(overlay)>1)
        stop("only one overlay gate can be added!In order to visualize multiple overlays,try to add a booleanGate first.")
      overlay<-getData(x,overlay)[,params]
    }

  }
  overlay
}
#' the actual plotGate engine
#' 
#' @param fitGate used to disable behavior of plotting the gate region in 1d densityplot
#' @param overlay either the gate indice list or event indices list
#' @importMethodsFrom flowCore nrow parameters parameters<-
#' @importMethodsFrom flowViz xyplot densityplot
.plotGate <- function(x,y,formula=NULL,cond=NULL,main=NULL,smooth=FALSE,type=c("xyplot","densityplot"),xlab=NULL,ylab=NULL,fitGate=FALSE,overlay=NULL, stack = FALSE, stats , default.y = "SSC-A", scales , ...){

	
	type<- match.arg(type)
    
    #x is either gs or gh, force it to be gs to be compatible with this plotGate engine
    if(class(x) == "GatingHierarchy")  
      x <- as(x, "GatingSet")
    gh <- x[[1]] 
    
	if(is.list(y))
		pid<-y$parentId
	else
		pid<-getParent(gh,y)
	
    #set the title  
    default_main <- list(label = getNodes(gh,isPath=T,showHidden = TRUE)[pid])
    if(is.null(main)){
      main <- default_main
    }else
    {
      if(is.list(main))
        main <- lattice:::updateList(default_main, main) #update default main if non-null main are specified
    }
	
	    
#    browser()
	#################################
	# setup axis labels and scales
	################################
    parseRes <- .preplot (x, y, type, stats, formula, default.y)
    
    curGates <- parseRes$gates
    xParam <- parseRes$xParam
    yParam <- parseRes$yParam
    params <- c(yParam,xParam)
    stats <- parseRes$stats
    
        #get data 
    #subset on channels to speed up loading data from disk
    parentData <- getData(x,pid,j = params)
    parentFrame <- parentData[[1]]      
    #set the smoothing option
    smooth <- as.logical(ifelse(nrow(parentFrame)<100,TRUE,smooth))
    
    
    axisObject<-.formatAxis(gh,parentFrame, xParam, yParam,...)
    
#    browser()
    default_xlab <- list(label = axisObject$xlab)
    if(is.null(xlab)){
      xlab <- default_xlab
    }else
    {
      if(is.list(xlab))
        xlab <- lattice:::updateList(default_xlab, xlab) #update default lab if non-null lab are specified
    }
    
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
                          ,xlab=xlab
                          ,scales=scales
                          ,main=main
                          ,...
                          )
                      )
#    browser()                      
    if(!is.null(stats)){
      thisCall <- as.call(c(as.list(thisCall),list(stats = stats)))
    }                      
	if(type=="xyplot")
	{
		
		#################################
		# calcuate overlay frames
		################################
        overlay <- .getOverlay(x, overlay, params)
		
#        browser()
        default_ylab <- list(label = axisObject$ylab)
        if(is.null(ylab)){
          ylab <- default_ylab
        }else
        {
          if(is.list(ylab))
            ylab <- lattice:::updateList(default_ylab, ylab) #update default scales if non-null scales are specified
        }
        
		#################################
		# the actual plotting
		################################
#        browser()
        thisCall <- as.call(c(as.list(thisCall)
                            ,list(ylab = ylab
                                  ,smooth = smooth
                                  ,overlay = overlay
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
        
#' @exportMethod recompute
setGeneric("recompute", function(x,...)standardGeneric("recompute"))
setMethod("recompute",c("GatingSet"),function(x, y){
			if(missing(y))
				y<-1
			if(is.character(y))
                y <- .getNodeInd(x[[1]],y)
				
            extend_val <- 0
            ignore_case <- FALSE
            gains <- NULL
			lapply(x,function(gh){
						
						
						sampleName<-getSample(gh)
						
						message(paste("gating",sampleName,"..."))
#					browser()
#					time1<-Sys.time()
						
						data <- getData(gh)
						mat <- exprs(data)
						lapply(y,function(nodeID){
                             nodeInd <- as.integer(nodeID)-1
                             recompute <- TRUE
							.Call("R_gating",gh@pointer,mat,sampleName,gains,nodeInd,recompute,extend_val, ignore_case)			
						})
						
						
						
					})
			message("done!")
			invisible()
						
		})

#' apply \code{FUN} to each sample (i.e. \code{GatingHierarchy})
#' 
#' sample names are used for names of the returned list
#'  
#' @rdname lapply-methods
#' @importFrom BiocGenerics lapply
#' @aliases 
#' lapply,GatingSet-method
setMethod("lapply","GatingSet",function(X,FUN,...){
      sapply(sampleNames(X),function(thisSample,...){
            gh <- X[[thisSample]]
            FUN(gh, ...)
          }, simplify = FALSE, ...)
            
      
    })    

   
    
setMethod("getSamples","GatingSet",function(x){
      stop("'getSamples' is defunct.\nUse 'sampleNames' instead.")
      sampleNames(x)
    })
#' Get/update sample names in a GatingSet
#' 
#' Return  a sample names contained in a GatingSet
#' 
#' @param object  or a \code{GatingSet}
#' @param value \code{character} new sample names
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
#' @aliases
#' sampleNames
#' sampleNames,GatingSet-method
#' sampleNames<-,GatingSet-method
#' @export
setMethod("sampleNames","GatingSet",function(object){
      sampleNames(flowData(object))
    })

setReplaceMethod("sampleNames",
    signature=signature(object="GatingSet"),
    definition=function(object, value)
    {
      oldNames <- sampleNames(object)
      #update c++ data structure
      mapply(oldNames,object, FUN = function(oldName, newName){
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
setMethod("getData",signature(obj="GatingSet",y="missing"),function(obj,y,tsort=FALSE, ...){
      flowData(obj)[,...]
  
    })
setMethod("getData",signature(obj="GatingSet",y="numeric"),function(obj,y,tsort=FALSE, ...){
      
      this_data <- getData(obj, ...)                        
      if(y == 0){
        this_data  
      }else{
        #subset by indices
        indices<-lapply(obj,getIndices,y)
        this_data <- Subset(this_data,indices)
        
        this_data	
      }
      
    })

#' @importMethodsFrom flowCore Subset
setMethod("getData",signature(obj="GatingSet",y="character"),function(obj,y,tsort=FALSE, ...){
			
      getData(obj,.getNodeInd(obj[[1]],y), ...)
			
		})


#' @export
setGeneric("ncFlowSet", function(x) standardGeneric("ncFlowSet"))
#' Fetch the flowData object associated with a GatingSet .
#' 
#' Deprecated by \code{flowData} method
#' @aliases 
#' ncFlowSet,GatingSet-method
#' @rdname ncFlowSet-methods
#' @export
setMethod("ncFlowSet",signature(x="GatingSet"),function(x){
      .Defunct("flowData")
      
    })

#' @export
setGeneric("ncFlowSet<-", function(x,value) standardGeneric("ncFlowSet<-"))

#' replace the flowData object associated with a GatingSet .
#' 
#' Deprecated by \code{flowData} method
#' 
#' @name ncFlowSet<-
#' @aliases 
#' ncFlowSet<-,GatingSet-method
#' @rdname ncFlowSet-methods 
#' @export
setReplaceMethod("ncFlowSet",signature(x="GatingSet"),function(x,value){
      .Defunct("flowData<-")
    })

#' Fetch or replace the flowData object associated with a GatingSet .
#' 
#' Accessor method that gets or replaces the flowset/ncdfFlowSet object in a GatingSet or GatingHierarchy
#' 
#' @param x A \code{GatingSet}
#' @param value The replacement \code{flowSet} or \code{ncdfFlowSet} object
#' 
#' @details Accessor method that sets or replaces the ncdfFlowSet object in the GatingSet or GatingHierarchy.
#' 
#' @return the object with the new flowSet in place.
#' 
#' @aliases 
#' flowData<-
#' flowData
#' flowData-methods
#' flowData,GatingSet-method
#' flowData<-,GatingSet-method
#' @export 
#' @rdname flowData-methods
setMethod("flowData",signature("GatingSet"),function(x,...){
        x@data
    })

setReplaceMethod("flowData",signature(x="GatingSet"),function(x,value){
      
      x@data <- value
      x
    })


#' read/set pData of flow data associated with \code{GatingSet} or \code{GatingSetList}
#' 
#' Accessor method that gets or replaces the pData of the flowset/ncdfFlowSet object in a GatingSet or GatingSetList
#'
#' @param object \code{GatingSet} or \code{GatingSetList}
#' @param value \code{data.frame} The replacement of pData for \code{flowSet} or \code{ncdfFlowSet} object
#' 
#' @return a \code{data.frame}
#' 
#' @importFrom Biobase pData description exprs sampleNames pData<-
#' 
#' @aliases 
#' pData,GatingSet-method
#' pData<-,GatingSet,data.frame-method
#' @exportMethod pData
#' @rdname pData-methods
setMethod("pData","GatingSet",function(object){
			pData(flowData(object))
		})
#' @exportMethod pData<-
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
            #deep copying flowData(but still pointing to the same cdf)
#            if(isNcdf(clone))
#              fs<-ncdfFlow::clone.ncdfFlowSet(fs,isEmpty=FALSE,isNew=FALSE)
#            else
#              fs<-flowCore:::copyFlowSet(fs)
            
            
            #update the data for clone            
            flowData(clone) <- fs
            clone@guid <- .uuid_gen()
			return(clone);
		})
		
		

setMethod("getGate",signature(obj="GatingSet",y="character"),function(obj,y){
			lapply(obj,function(x)getGate(x,y))
		})
setMethod("getGate",signature(obj="GatingSet",y="numeric"),function(obj,y){
      lapply(obj,function(x)getGate(x,y))
    })
    
#' @aliases 
#' setNode,GatingSet,numeric,ANY-method
#' setNode,GatingSet,character,ANY-method
#' @rdname setNode-methods
setMethod("setNode"
    ,signature(x="GatingSet",y="numeric",value="ANY")
    ,function(x,y,value,...){
      lapply(x,function(gh){
            setNode(gh,y,value,...)
          })
    })
setMethod("setNode"
    ,signature(x="GatingSet",y="character",value="ANY")
    ,function(x,y,value,...){
      setNode(x,.getNodeInd(x[[1]],y),value)
    })




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
      as(x[i], "GatingHierarchy");
    })

#' Methods to get the length of a GatingSet
#' 
#' Return the length of a \code{GatingSet} or \code{GatingSetList} object (number of samples).
#' 
#' @aliases 
#' length-methods 
#' length,GatingSet-method
#' @rdname length-methods
#' @export  
setMethod("length","GatingSet",function(x){
      length(flowData(x));
    })

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
#' @param ... Additional arguments
#' y \code{character} The name of the node. A list of nodes is accessible via \code{getNodes(x)}.
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
#'         #If gh is a GatingHierarchy
#'         getPopStats(gh);
#'         #proportion for the fifth population
#'         getProp(G,getNodes(gh)[5])
#'         getTotal(G,getNodes(G,tsort=T)[5])
#'         
#'         }
#' @aliases
#' getPopStats
#' getPopStats-methods
#' getPopStats,GatingHierarchy-method
#' getPopStats,GatingSet-method
#' getProp
#' getProp-methods
#' getProp,GatingHierarchy,character-method
#' getTotal
#' getTotal-methods
#' getTotal,GatingHierarchy,character-method
setMethod("getPopStats", "GatingSet",
    function(x, statistic = c("freq", "count"), flowJo = FALSE, ...) {
      
      # Based on the choice of statistic, the population statistics are returned for
      # each Gating Hierarchy within the GatingSet.
      statistic <- match.arg(statistic)
      
      # The 'flowJo' flag determines whether the 'flowJo' or 'flowCore' statistics
      # are returned.
      if (flowJo) {
        statistic <- paste("flowJo", statistic, sep = ".")
      } else {
        statistic <- paste("flowCore", statistic, sep = ".")
      }
      
      
      pop_stats <- do.call(cbind, lapply(x, function(y) {
                getPopStats(y)[[statistic]]
              }))
      rownames(pop_stats) <- rownames(getPopStats(x[[1]]))
      pop_stats
    })

#' Plot the coefficient of variation between flowJo and flowCore population statistics for each population in a gating hierarchy.
#' 
#' This function plots the coefficient of variation calculated between the flowJo population statistics and the flowCore population statistics for each population in a gating hierarchy extracted from a flowJoWorkspace. 
#' @param x A \code{GatingHierarchy} from a \code{flowJoWorkspace}, or a \code{GatingSet}.
#' @param m \code{numeric} The number of rows in the panel plot. Now deprecated, uses lattice.
#' @param n \code{numeric} The number of columns in the panel plot. Now deprecated, uses lattice.
#' @param \dots Additional arguments to the \code{barplot} methods.
#' @details The CVs are plotted as barplots across panels on a grid of size \code{m} by \code{n}.
#' @return Nothing is returned.
#' @seealso \code{\link{getPopStats}}
#' @examples
#'   \dontrun{
#'     #G is a GatingHierarchy
#'     plotPopCV(G,4,4);
#'   }
#' @aliases plotPopCV plotPopCV-methods plotPopCV,GatingHierarchy-method plotPopCV,GatingSet-method
#' @rdname plotPopCV-methods
setMethod("plotPopCV","GatingSet",function(x,...){
#columns are populations
#rows are samples
      cv<-do.call(rbind,lapply(lapply(x,getPopStats),function(x)apply(x[,2:3],1,function(x){cv<-IQR(x)/median(x);ifelse(is.nan(cv),0,cv)})))
      rownames(cv)<-sampleNames(x);#Name the rows
#flatten, generate levels for samples.
      nr<-nrow(cv)
      nc<-ncol(cv)
      populations<-gl(nc,nr,labels=basename(as.character(colnames(cv))))
      samples<-as.vector(t(matrix(gl(nr,nc,labels=basename(as.character(rownames(cv)))),nrow=nc)))
      cv<-data.frame(cv=as.vector(cv),samples=samples,populations=populations)
      return(barchart(cv~populations|samples,cv,...,scale=list(x=list(...))));
    })


setMethod("getKeywords",c("GatingSet","character"),function(obj,y){
      stop("'getKeywords' is defunct. use 'keyword' instead! ")
      ind <- which(sampleNames(obj)%in%y)
      if(length(ind)>0){
        getKeywords(obj,ind);
      }else{
        stop(paste("Sample ",y," not in GatingSet",sep=""));
      }
    })
setMethod("getKeywords",c("GatingSet","numeric"),function(obj,y){
      stop("'getKeywords' is defunct. use 'keyword' instead! ")
      if(length(obj) < y){
        stop("index out of range");
      }else{
        lapply(obj, getKeywords);
      }
    })
setMethod("keyword",c("GatingSet", "missing"),function(object,keyword = "missing"){
        lapply(object, flowCore::keyword)
      
    })

setMethod("keyword",c("GatingSet","character"),function(object,keyword){
      tmp<-data.frame(unlist(lapply(object,function(x)keyword(x,keyword)),use.names=FALSE));
      tmp<-data.frame(matrix(tmp[[1]],ncol=length(keyword),byrow=T))
      colnames(tmp)<-keyword
      tmp
    })
