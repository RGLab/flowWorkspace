#' @include GatingHierarchy-Methods.R

# TODO: currently archive save the entire cdf file instead of the current view of cdf
# it could cause data redudancy if only a subset(view of ncdfFlowSet) is desired to be saved
###############################################################################

######################################
##archive/unarchive to/from a folder 
##it is faster than tar-version,but require
##a new dest folder to avoid overwriting
##the old data by mistake
##currently not exposed to end user
######################################
save_gs<-function(G,path,overwrite = FALSE, cdf = "copy", ...){
#  browser()
  cdf <- match.arg(cdf,c("copy","move","skip","symlink","link"))
  guid <- G@guid
  if(length(guid)==0){
    G@guid <- flowWorkspace:::.uuid_gen()
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
      if(flowWorkspace:::isNcdf(G[[1]])){
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
        
        if(flowWorkspace:::isNcdf(G[[1]])){
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
  invisible(flowWorkspace:::.save_gs(G=G,path = path, cdf = cdf, ...))
  message("Done\nTo reload it, use 'load_gs' function\n")
  
  
}


load_gs<-function(path){
#  browser()
  path <- normalizePath(path,mustWork = TRUE)
  if(!file.exists(path))
    stop(path,"' not found!")
  files<-list.files(path)
#   browser()
  flowWorkspace:::.load_gs(output = path, files = files)$gs
  
}




###serialization functions to be called by wrapper APIs
### when save.cdf == FALSE, skip saving cdf file
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
    if(cdf != "skip" && flowWorkspace:::isNcdf(G[[1]]))
    {
      from<-ncFlowSet(G)@file
#      browser()
      if(cdf == "move"){
        message("moving ncdf...")
        ncFile <- file.path(path,basename(from))
        res <- file.rename(from,ncFile)
        #reset the file path for ncdfFlowSet
        ncFlowSet(G)@file <- ncFile
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
###unserialization functions to be called by wrapper APIs
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
      gs<-readRDS(rds.file)
      
      guid <- try(slot(gs,"guid"),silent=T)
      if(class(guid)=="try-error"){
        #generate the guid for the legacy archive
        gs@guid <- .uuid_gen()
      }
      
      message("loading tree object...")
      gs@pointer<-.Call("R_loadGatingSet",dat.file)
      
      #update the pointer in each gating hierarchy
      for(i in 1:length(gs@set))
      {
        gs@set[[i]]@pointer<-gs@pointer
      }
      if(flowWorkspace:::isNcdf(gs[[1]]))
      {
        if(length(nc.file)==0)
          stop(".nc file missing in ",file)
        ncFlowSet(gs)@file<-nc.file
        
      }
      message("Done")
      list(gs=gs,files=c(dat.file,rds.file))
}
######################################
##archive/unarchive to/from a tar file
######################################
archive<-function(G,file=tempfile()){
	
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
	
#path specifies the temporary folder to store the unzipped dat (and nc) files
unarchive<-function(file,path=tempdir()){
	
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



.parseWorkspace<-function(xmlFileName,sampleIDs,execute,path,dMode,isNcdf,includeGates,flowSetId=NULL,sampNloc="keyword",xmlParserOption, ...){


	message("calling c++ parser...")
#	browser()
	time1<-Sys.time()
	G<-GatingSet(x=xmlFileName,y=sampleIDs,includeGates=includeGates,sampNloc=sampNloc,xmlParserOption = xmlParserOption, dMode=dMode)
#	time_cpp<<-time_cpp+(Sys.time()-time1)
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
			#get full path for each fcs and store in dataPath slot
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
		
#		time1<-Sys.time()
		
		
#		time_sum<<-time_sum+(Sys.time()-time1)
	}else
	{
		files<-samples
	}
#	browser()
#	print(Sys.time()-time1)
	G<-.addGatingHierarchies(G,files,execute,isNcdf,...)
#	time1<-Sys.time()

	message("done!")
#	print(Sys.time()-time1)

	G	
}

#' constructors for GatingSet
#' 
#' construct object from existing gating hierarchy(gating template) and flow data
#' 
#' @rdname GatingSet-methods
#' @aliases
#' GatingSet,GatingHierarchyInternal,character-method
setMethod("GatingSet",c("GatingHierarchy","character"),function(x,y,path=".",isNcdf=FALSE,dMode=1,...){
			
			samples<-y
			dataPaths<-vector("character")
			excludefiles<-vector("logical")
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
			Object@pointer<-.Call("R_NewGatingSet",x@pointer,getSample(x),samples,as.integer(dMode))
            Object@guid <- .uuid_gen()
			Object<-.addGatingHierarchies(Object,files,execute=TRUE,isNcdf=isNcdf,...)
            message("done!")
			return(Object)
		})
   

#' constructing gating set
#' @param prefix a \code{logical} flag indicates whether the colnames needs to be updated with prefix(e.g. "<>" or "comp") specified by compensations
.addGatingHierarchies<-function(G,files,execute,isNcdf,compensation=NULL,wsversion = -1,extend_val = 0, prefix = TRUE,...){
	
    if(length(files)==0)
      stop("not sample to be added to GatingSet!")
	#environment for holding fs data,each gh has the same copy of this environment
	globalDataEnv<-new.env(parent=emptyenv())
	
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
	
	nFiles<-length(files)
	set<-vector(mode="list",nFiles)	
    prefixColNames <- NULL
	for(i in 1:nFiles)
	{
		file<-files[i]		
		sampleName<-basename(file)
		gh<-new("GatingHierarchy",pointer=G@pointer,name=sampleName)
#			browser()
		localDataEnv<-nodeDataDefaults(gh@tree,"data")
		localDataEnv$data<-globalDataEnv
		
		#gating (including loading data,compensating,transforming and the actual gating)
		if(execute)
		{
			
			gh@dataPath<-dirname(file)
			
			
			message("loading data: ",file);
			if(isNcdf)
				data<-read.FCS(file)[,colnames(fs)]
			else
				data<-fs[[sampleName]]
             
            cnd<-colnames(data)
#            browser()
            #alter colnames(replace "/" with "_") for flowJo X
            if(wsversion == "1.8"){
                colnames(data) <- gsub("/","_",cnd)
                cnd<-colnames(data)
            
            }
              
			##################################
			#Compensating the data
			##################################
			comp<-.Call("R_getCompensation",G@pointer,sampleName)
			cid<-comp$cid
			if(cid=="")
				cid=-2

			if(cid!="-1" && cid!="-2"){
				message("Compensating");
				
				marker<-comp$parameters
				
				if(is.null(compensation))
					compobj<-compensation(matrix(comp$spillOver,nrow=length(marker),ncol=length(marker),byrow=TRUE,dimnames=list(marker,marker)))
				else
					compobj<-compensation#TODO: to update compensation information in C part
				#TODO this compensation will fail if the parameters have <> braces (meaning the data is stored compensated).
				#I need to handle this case properly.
				res<-try(compensate(data,compobj),silent=TRUE)
				if(inherits(res,"try-error")){
					message("Data is probably stored already compensated");
				}else{
					data<-res
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
						gh@compensation<-spillover(data)$SPILL
					}else
					{
						compobj<-compensation
						gh@compensation<-compensation@spillover
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
                    cnd<-as.vector(parameters(data)@data$name)
                  }
                  prefixColNames <- cnd
                  
                  wh<-match(parameters(compobj),prefixColNames)
                  
                  prefixColNames[wh]<-paste(comp$prefix,parameters(compobj),comp$suffix,sep="")
                  
                    
                }
              }
            ##################################
            #transforming and gating
            ##################################
            message(paste("gating ..."))
            #stop using gating API of cdf-version because c++ doesn't store the view of ncdfFlowSet anymore
            mat<-data@exprs #using @ is faster than exprs()
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
            
            .Call("R_gating",gh@pointer,mat,sampleName,gains,nodeInd=0,recompute=FALSE, extend_val = extend_val)
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
            tempenv <- .transformRange(gh,wsversion,fs@frames,timeRange = tRg)
            dataenv <- nodeDataDefaults(gh@tree,"data")
            assign("axis.labels",tempenv$axis.labels,dataenv)
            

		}
		
		gh@flag<-execute #assume the excution would succeed if the entire G gets returned finally
		set[[i]]<-gh
	}
	names(set)<-basename(files)
	G@set<-set
	
	if(execute)
	{
#		browser()
		#update colnames 
		#can't do it before fs fully compensated since
		#compensate function check the consistency colnames between input flowFrame and fs
		if(!is.null(prefixColNames))
          colnames(fs) <- prefixColNames 
		
		#attach filename and colnames to internal stucture for gating
#		browser()
		assign("ncfs",fs,globalDataEnv)
		
		
	}
	G
}

.transformRange_vX<-function(gh){
        sampleName <- getSample(gh)
        dataenv <- nodeDataDefaults(gh@tree,"data")
        frmEnv<-dataenv$data$ncfs@frames
        rawRange<-range(get(sampleName,frmEnv))
        tempenv<-new.env()
        assign("axis.labels",vector(mode="list",ncol(rawRange)),envir=tempenv);
        cal<-getTransformations(gh)

        cal_names <-trimWhiteSpace(names(cal))
#         browser()
        datarange<-sapply(1:dim(rawRange)[2],function(i){
#              message(i)
          j <- match(names(rawRange)[i],cal_names)
          if(!is.na(j)){
  #                                   browser()
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
              
  #                       browser()
              ######################################
              #equal interal at raw scale
              ######################################                      
              base10raw<-unlist(lapply(2:6,function(e)10^e))
              base10raw<-c(0,base10raw)
              raw<-base10raw[base10raw>min(rw)&base10raw<max(rw)]
              pos<-signif(cal[[j]](raw))
              
              
              assign("i",i,tempenv)
              assign("raw",raw,tempenv);
              assign("pos",pos,tempenv);
              eval(expression(axis.labels[[i]]<-list(label=as.character(raw),at=pos)),envir=tempenv);
            }
            return(r);
          }else{
            this_chnl <- names(rawRange)[i]
            #update time range with the real data range
            if(grepl("[Tt]ime",this_chnl))
            {
              range(dataenv$data$ncfs[[sampleName]]@exprs[,this_chnl])
            }else{
              rawRange[,i]
            }
            
          }
      })
  copyEnv(tempenv,dataenv);
  
#   browser()       
  datarange<-t(rbind(datarange[2,]-datarange[1,],datarange))
  datapar<-parameters(get(sampleName,frmEnv))
  pData(datapar)[,c("range","minRange","maxRange")]<-datarange
  
  #gc(reset=TRUE)
#   assign("datapar",datapar,dataenv)
  eval(substitute(frmEnv$s@parameters<-datapar,list(s=sampleName)))
}

#' transform the range slot and construct axis label and pos for the plotting
.transformRange<-function(gh,wsversion,frmEnv, timeRange = NULL){
#  browser()
    sampleName <- getSample(gh)
#    dataenv <- nodeDataDefaults(gh@tree,"data")
     cal<-getTransformations(gh)
     comp<-.Call("R_getCompensation",gh@pointer,sampleName)
     prefix <- comp$prefix
     suffix <- comp$suffix
#	frmEnv<-dataenv$data$ncfs@frames
	rawRange<-range(get(sampleName,frmEnv))
	tempenv<-new.env()
	assign("axis.labels",vector(mode="list",ncol(rawRange)),envir=tempenv);
    
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
						
							
						assign("i",i,tempenv)
						assign("raw",raw,tempenv);
						assign("pos",pos,tempenv);
						eval(expression(axis.labels[[i]]<-list(label=as.character(raw),at=pos)),envir=tempenv);
					}
					return(r);
				}else{
                  this_chnl <- names(rawRange)[i]
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
#	copyEnv(tempenv,dataenv);
	
#	browser()		
	datarange<-t(rbind(datarange[2,]-datarange[1,],datarange))
	datapar<-parameters(get(sampleName,frmEnv))
	pData(datapar)[,c("range","minRange","maxRange")]<-datarange
	
	eval(substitute(frmEnv$s@parameters<-datapar,list(s=sampleName)))

    tempenv
}
setMethod("haveSameGatingHierarchy",signature=c("GatingSet","missing"),function(object1,object2=NULL){
#			em<-edgeMatrix(object1)
#			if(length(em)>=2){
#				return(all(sapply(2:length(em),function(i)
#											identical(em[[1]],em[[i]])
#								)
#							)
#						& all(apply(do.call(cbind,lapply(object1,function(x)
#															gsub("^.*\\.","",RBGL:::bfs(x@tree))
#														)
#											)
#										,1,function(x)
#												x%in%x[1]
#									)
#								)
#						)
#			}else{
#				return(TRUE)
#			}
			return(TRUE)
		})

    
#' Plot gates and associated cell population contained in a \code{GatingHierarchy} or \code{GatingSet}
#' 
#' When applied to a \code{GatingHierarchy},\code{arrange} is set as TRUE, then all the gates associated with it are plotted as different panel on the same page.
#' If \code{arrange} is FALSE, then it plots one gate at a time.
#' By default ,\code{merge} is set as TRUE, plot multiple gates on the same plot when they share common parent population and axis.
#' When applied to a \code{GatingSet}, if lattice is TRUE,it plots one gate (multiple samples) per page , otherwise, one sample (with multiple gates) per page.   

#' @param x \code{\linkS4class{GatingSet}} object
#' @param y \code{character} the node name or full(/partial) gating path 
#'          or \code{numeric} representing the node index in the \code{GatingHierarchy}.
#'          or \code{missing} which will plot all gates and one gate per page. It is useful for generating plots in a multi-page pdf.
#'          Nodes can be accessed with \code{\link{getNodes}}.
#' @param bool \code{logical} specifying whether to plot boolean gates.

#' @param main \code{character}, The main title of the plot. Default is the sample name.
#' @param arrange \code{logical} indicating whether to arrange different populations/nodes on the same page via \code{grid.arrange} call.
#' @param merge \code{logical} indicating whether to draw multiple gates on the same plot if these gates share the same parent population and same x,y dimensions/parameters;
#' @param lattice \code{logical} indicating whether to draw one node/gate on multiple samples on the same page through lattice plot;
#' @param ...
#' @param formula \code{formula} a formula passed to \code{xyplot} function of \code{flowViz}, by default it is NULL, which means the formula is generated according to the x,y parameters associated with gate.
#' @param cond \code{character} the conditioning variable to be passed to lattice plot.
#' @param overlay either a \code{numeric} scalar indicating the index of a gate/populationwithin the \code{GatingHierarchy} or a \code{logical} vector that indicates the cell event indices representing a sub-cell population. This cell population is going to be plotted on top of the existing gates(defined by \code{y} argument) as an overlay. 
#' @param ... The other additional arguments to be passed to \link[flowViz]{xyplot}.
#' @return  a \code{trellis} object if \code{arrange} is \code{FALSE}, 
#' @references \url{http://www.rglab.org/}
#' @examples \dontrun{
#' 	#G is a GatingHierarchy
#' 	plotGate(G,getNodes(G)[5]);#plot the gate for the  fifth node
#' }
 
setMethod("plotGate",signature(x="GatingSet",y="missing"),function(x,y,...){
#			browser()
			y<-2:length(getNodes(x[[1]]))
			plotGate(x,y,...)
			
		})

    
setMethod("plotGate",signature(x="GatingSet",y="numeric"),function(x,y,lattice=TRUE,bool=FALSE,merge=TRUE,...){
			if(lattice)
			{
				plotList<-.mergeGates(x[[1]],y,bool,merge)
				
				lapply(plotList,function(y){
							
							return(.plotGateGS(x,y,...))
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

#' preporcess the gating tree to prepare for the plotGate
#' 
#' @param x a \code{GatingSet}
#' @param y gate index
#' @param type \code{character} either 'xyplot' or 'densityplot'
#' @param stats \code{numeric} proportions of cell population. If \code{missing} then extract it from \code{gatingHiearchy}
#' @return a \code{list} containing 'gates', 'xParam','yParam', and 'stats'
.preplot <- function(x, y, type, stats, formula, ...){
  samples <- getSamples(x)
#  browser()
  if(is.list(y))
  {
#       browser()
    curGates<-sapply(samples,function(curSample){
          
          filters(lapply(y$popIds,function(y)getGate(x[[curSample]],y)))
        },simplify=F)
    curGates<-as(curGates,"filtersList")
    
    if(missing(stats)){
      stats <- sapply(samples,function(thisSample){
            lapply(y$popIds,function(y){
                  curGh <- x[[thisSample]]
                  getProp(curGh,getNodes(curGh,y),flowJo = F)
                })
          },simplify = FALSE)  
    }    
    
  }else
  {
    curGates<-getGate(x,y)
    
    if(suppressWarnings(any(is.na(curGates)))){
      message("Can't plot. There is no gate defined for node ",getNodes(gh,y));
      invisible();            
      return(NULL)
    }
    if(missing(stats)){
      stats <- sapply(samples,function(thisSample){
            curGh <- x[[thisSample]]
            getProp(curGh,getNodes(curGh,y),flowJo = F)
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
  if(type=="xyplot")
  {
    if(length(params)==1)
    {
      if(is.null(formula)){
        yParam <- "SSC-A"
      }else{
        yParam <- flowViz:::expr2char(formula[[2]])
      }
      
      
      if(params=="SSC-A" && yParam == "SSC-A")
        xParam<-"FSC-A"
      else
        xParam<-params

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
#' the actual plotGate engine
#' 
#' @param fitGate used to disable behavior of plotting the gate region in 1d densityplot
#' @overlay either the gate indice list or event indices list
.plotGateGS<-function(x,y,formula=NULL,cond=NULL,main=NULL,smooth=FALSE,type=c("xyplot","densityplot"),xlab=NULL,ylab=NULL,fitGate=FALSE,overlay=NULL, stats , ...){

	
	type<- match.arg(type)
	
	gh<-x[[1]]
	if(is.list(y))
		pid<-y$parentId
	else
		pid<-getParent(gh,y)
	
    #set the title  
	if(is.null(main)){
		fjName<-getNodes(gh,pid,isPath=T)
		main<-fjName
	}
	
	    
#    browser()
	#################################
	# setup axis labels and scales
	################################
    parseRes <- .preplot (x, y, type, stats, formula,...)
    
    curGates <- parseRes$gates
    xParam <- parseRes$xParam
    yParam <- parseRes$yParam
    params <- c(yParam,xParam)
    stats <- parseRes$stats
    
    #get data 
    #subset on channels to speed up loading data from disk
    parentdata<-getData(x,pid,j = params)
    parentFrame<-parentdata[[1]]
    
    #set the smoothing option
    smooth<-ifelse(nrow(parentFrame)<100,TRUE,smooth)
    
    
    axisObject<-.formatAxis(gh,parentFrame, xParam, yParam,...)
    
    if(is.null(xlab)){
      xlab <- axisObject$xlab
    }
    
    #set the formula
    if(is.null(formula))
    {
      formula<-mkformula(params,isChar=TRUE)
      if(!is.null(cond))
        formula<-paste(formula,cond,sep="|")
      formula<-as.formula(formula)
    }

    thisCall<-quote(plot(x=formula
                          ,data=parentdata
                          ,filter=curGates
                          ,xlab=xlab
                          ,scales=axisObject$scales
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
		if(!is.null(overlay))
		{
			#gate indices
			if(class(overlay)=="numeric")
			{
				if(length(overlay)>1)
					stop("only one overlay gate can be added!In order to visualize multiple overlays,try to add a booleanGate first.")
				overlay<-getData(x,overlay)[,params]
			}else
				overlay<-Subset(getData(x),overlay)[,params]
		}
		
        
        if(is.null(ylab)){
          ylab <- axisObject$ylab
        }
		#################################
		# the actual plotting
		################################
#        browser()
        thisCall<-as.call(c(as.list(thisCall)
                            ,ylab=ylab
                            ,smooth=smooth
                            ,overlay=overlay
                            )
                          )
        thisCall[[1]]<-quote(xyplot)                          
		
	}else
	{
		if(length(params)==1)
		{
            
          thisCall<-as.call(c(as.list(thisCall)
                              ,fitGate=fitGate
                              )
                           )
          thisCall[[1]]<-quote(densityplot) 	
		}
	}
	return(eval(thisCall))	
}




setGeneric("clone", function(x,...)standardGeneric("clone"))
setMethod("clone",c("GatingSet"),function(x,...){

			clone<-x
			#clone c structure
			message("cloning tree structure...")
			clone@pointer<-.Call("R_CloneGatingSet",x@pointer,getSamples(x))
            clone@guid <- .uuid_gen()
              
            #create new global data environment
            gdata<-new.env(parent=emptyenv());
#            browser()
			#update the pointer in each gating hierarchy
			for(i in 1:length(clone@set))
			{
				this_gh <- clone@set[[i]]
                this_gh@pointer<-clone@pointer
                this_gh@transformations<-list()#update trans slot since it contains environment and does not get deep copied automatically
            
                #update data environment for each gh
                nd<-this_gh@tree@nodeData
                
                nd@defaults$metadata<-new.env(hash=TRUE, parent=emptyenv())
                nd@defaults$data<-new.env(hash=TRUE, parent=emptyenv())
                
                source_gh <- x@set[[i]] 
                copyEnv(source_gh@tree@nodeData@defaults$data,nd@defaults$data)
                
                nd@defaults$data[["data"]]<-gdata
                copyEnv(source_gh@tree@nodeData@defaults$metadata,nd@defaults$metadata)
                
                this_gh@tree@nodeData<-nd
                
                clone@set[[i]] <- this_gh
			}
			
          
			
			

			#deep copying flowSet/ncdfFlowSet
			message("cloning flow data...")
			fs<-ncFlowSet(x)
			if(flowWorkspace:::isNcdf(x[[1]]))
				fs_clone<-ncdfFlow::clone.ncdfFlowSet(fs,isEmpty=FALSE,isNew=TRUE,...)
			else
				fs_clone<-flowCore:::copyFlowSet(fs)
		
#			ncFlowSet(clone)<-fs_clone
			
			assign("ncfs",fs_clone,gdata)
			message("GatingSet cloned!")
			clone
		})

setGeneric("recompute", function(x,...)standardGeneric("recompute"))
setMethod("recompute",c("GatingSet"),function(x,y){
			if(missing(y))
				y<-1
			if(is.character(y))
                y <- .getNodeInd(x[[1]],y)
				
			
			lapply(x,function(gh){
						
						
						sampleName<-getSample(gh)
						
						message(paste("gating",sampleName,"..."))
#					browser()
#					time1<-Sys.time()
						
						data<-getData(gh)
						mat<-exprs(data)
						lapply(y,function(nodeID){
									.Call("R_gating",gh@pointer,mat,sampleName,gains=NULL,nodeInd=as.integer(nodeID)-1,recompute=TRUE)			
								})
						
						
						
					})
			message("done!")
			invisible()
						
		})
setMethod("recompute",c("GatingSet"),function(x,y){
			recomputeGate(x=x,gate=y)
		})


setReplaceMethod("ncFlowSet",signature(x="GatingSet"),function(x,value){
			
			callNextMethod(x,value)
			#associate the internal structure with the new cdf

			x
			
		})


setMethod("show","GatingSet",function(object){
			
			callNextMethod(object)
			
			for(i in 1:length(object@set))
			{
#				browser()
				if(!identical(object@set[[i]]@pointer,object@pointer))
					stop("GatingHierarchy ",names(object@set)[i]," has a differnent pointer than GatingSet!")
			}
			
		})
#overload the getSamples for GatingSet to ensure the sample names comes from pdata of fs    
setMethod("getSamples","GatingSet",function(x){
      sampleNames(getData(x))
    })
#' to speed up reading data from disk later on, 
#' we can optionally pass j to ncdfFlow::[ to subset on channel
setMethod("getData",signature(obj="GatingSet",y="missing"),function(obj,y,tsort=FALSE, ...){
      ncFlowSet(obj)[,...]
  
    })
setMethod("getData",signature(obj="GatingSet",y="numeric"),function(obj,y,tsort=FALSE, ...){
      
      this_node <- getNodes(obj[[1]])[y]
      getData(obj,this_node, ...)
      
    })

setMethod("getData",signature(obj="GatingSet",y="character"),function(obj,y,tsort=FALSE, ...){
			
            this_data <- getData(obj, ...)                        
            if(y == "root"){
              this_data  
            }else{
				#subset by indices
				indices<-lapply(obj,getIndices,y)
                this_data <- Subset(this_data,indices)
               
                this_data	
			}
			
		})
######################################################
#read/update ncdfFlowSet object stored in GatingSet
######################################################
setMethod("ncFlowSet",signature(x="GatingSet"),function(x){
      first_sample <- names(x@set)[[1]]
      ncFlowSet(x[[first_sample]])
    })

setReplaceMethod("ncFlowSet",signature(x="GatingSet"),function(x,value){
      first_sample <- names(x@set)[[1]]
      ncFlowSet(x[[first_sample]])<-value
      x
    })
#to be replaced by method "ncdfFlowSet"
setMethod("getNcdf",signature(obj="GatingSet"),function(obj){
      .Deprecated("ncFlowSet")
      getNcdf(obj[[1]])
    })
    
#note:it doesn't use metadata slot of GatingSet, instead it directly access the pData of flowSet/ncdfFlowSet
setMethod("pData","GatingSet",function(object){
			pData(ncFlowSet(object))
		})
setReplaceMethod("pData",c("GatingSet","data.frame"),function(object,value){
#			env<-nodeDataDefaults(object[[1]]@tree,"data")$data
#		
#			assign("value",value,env)
#			expr1<-expression({value<-value[match(sampleNames(ncfs),value$name),];rownames(value)<-value$name;pData(ncfs)<-value;varM<-varMetadata(phenoData(ncfs));varM[-1,]<-rownames(varM)[-1];varMetadata(phenoData(ncfs))<-varM})
#			eval(expr1,envir=env)
#			rm("value",envir=env)
			fs<-ncFlowSet(object)
			rownames(value)<-value$name
			pData(fs)<-value
			varM<-varMetadata(phenoData(fs))
			varM[-1,]<-rownames(varM)[-1]
			varMetadata(phenoData(fs))<-varM
			ncFlowSet(object)<-fs
			return (object)
		})


##overload the original method to add subetting on flowSet/ncdfFlowSet
setMethod("[",c("GatingSet"),function(x,i,j,...,drop){
            
            #convert non-character indices to character
            if(extends(class(i), "numeric")||class(i) == "logical"){
              i <- getSamples(x)[i]
            }
            
            #copy the R structure          
            clone <-x
            
            #subsetting flowSet
			fs<-ncFlowSet(clone)[i]
            #deep copying flowSet/ncdfFlowSet R object(but still pointing to the same cdf)
            if(flowWorkspace:::isNcdf(clone[[1]]))
              fs<-ncdfFlow::clone.ncdfFlowSet(fs,isEmpty=FALSE,isNew=FALSE)
            else
              fs<-flowCore:::copyFlowSet(fs)
            
#            browser()
            
            #subsetting R object
            clone@set <- clone@set[i]
			
			#update data environment for each gh
			gdata<-new.env(parent=emptyenv());
			for(sample in names(clone@set)){
#				browser()
				nd<-clone[[sample]]@tree@nodeData
				nd@defaults$metadata<-new.env(parent=emptyenv())
				nd@defaults$data<-new.env(parent=emptyenv())
				copyEnv(clone[[sample]]@tree@nodeData@defaults$data,nd@defaults$data)
				nd@defaults$data[["data"]]<-gdata
				copyEnv(clone[[sample]]@tree@nodeData@defaults$metadata,nd@defaults$metadata)
				clone[[sample]]@tree@nodeData<-nd
			}
			
            #update the data for clone            
            ncFlowSet(clone)<-fs
            clone@guid <- .uuid_gen()
			return(clone);
		})
		
		

setMethod("getGate",signature(obj="GatingSet",y="character"),function(obj,y,tsort=FALSE){
			lapply(obj,function(x)getGate(x,y))
		})
setMethod("getGate",signature(obj="GatingSet",y="numeric"),function(obj,y,tsort=FALSE){
      lapply(obj,function(x)getGate(x,y,tsort=tsort))
    })
    
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
setMethod("[[",c("GatingSet"),function(x,i,j,...){
      #convert non-character indices to character
      if(extends(class(i), "numeric")||class(i) == "logical"){
        i <- getSamples(x)[i]
      }
      
      return(x@set[[i]]);
    })
setMethod("length","GatingSet",function(x){
      length(x@set);
    })

setMethod("show","GatingSet",function(object){
      cat("A GatingSet with ",length(object), " samples\n")
      for(i in 1:length(object)){
        cat(i,". ");
        show(object[[i]])
      }
    })
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
      
      if (!haveSameGatingHierarchy(x)) {
        message("Can't retrieve population statistics table for GatingSet. The samples don't all have the same gating schemes.")
      }
      
      pop_stats <- do.call(cbind, lapply(x, function(y) {
                getPopStats(y)[[statistic]]
              }))
      rownames(pop_stats) <- rownames(getPopStats(x[[1]]))
      pop_stats
    })
setMethod("plotPopCV","GatingSet",function(x,...){
#columns are populations
#rows are samples
      cv<-do.call(rbind,lapply(lapply(x,getPopStats),function(x)apply(x[,2:3],1,function(x){cv<-IQR(x)/median(x);ifelse(is.nan(cv),0,cv)})))
      rownames(cv)<-getSamples(x);#Name the rows
#flatten, generate levels for samples.
      nr<-nrow(cv)
      nc<-ncol(cv)
      populations<-gl(nc,nr,labels=basename(as.character(colnames(cv))))
      samples<-as.vector(t(matrix(gl(nr,nc,labels=basename(as.character(rownames(cv)))),nrow=nc)))
      cv<-data.frame(cv=as.vector(cv),samples=samples,populations=populations)
      return(barchart(cv~populations|samples,cv,...,scale=list(x=list(...))));
    })

setMethod("getAxisLabels",signature(obj="GatingHierarchy",y="missing"),function(obj,y=NULL,...){
      get("axis.labels",envir=nodeData(obj@tree)[[1]]$data)
    })
#Return the list of keywords given a GatingSet and a sample name
setMethod("getKeywords",c("GatingSet","character"),function(obj,y){
      ind<-which(getSamples(obj)%in%y)
      if(length(ind)>0){
        getKeywords(obj,ind);
      }else{
        stop(paste("Sample ",y," not in GatingSet",sep=""));
      }
    })
setMethod("getKeywords",c("GatingSet","numeric"),function(obj,y){
      if(length(obj)<y){
        stop("index out of range");
      }else{
        getKeywords(obj[[y]]);
      }
    })
setMethod("keyword",c("GatingSet","character"),function(object,keyword){
      tmp<-data.frame(unlist(lapply(object,function(x)keyword(x,keyword)),use.names=FALSE));
      tmp<-data.frame(matrix(tmp[[1]],ncol=length(keyword),byrow=T))
      colnames(tmp)<-keyword
      tmp
    })