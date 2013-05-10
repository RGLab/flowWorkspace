# TODO: currently archive save the entire cdf file instead of the current view of cdf
# it could cause data redudancy if only a subset(view of ncdfFlowSet) is desired to be saved
###############################################################################

###serialization functions to be called by wrapper APIs
### when save.cdf == FALSE, skip saving cdf file
.save_gs <- function(G,path, save.cdf = TRUE, move.cdf = FALSE){
    
#    browser()
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
    if(save.cdf&&flowWorkspace:::isNcdf(G[[1]]))
    {   
      message("saving ncdf...")
      from<-ncFlowSet(G)@file
      
      if(move.cdf){
        system(paste("mv",from,path))  
      }else{
        ncFile<-tempfile(tmpdir=path,fileext=".nc")
        file.copy(from=from,to=ncFile)
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



.parseWorkspace<-function(xmlFileName,sampleIDs,execute,path,dMode,isNcdf,includeGates,flowSetId=NULL,sampNloc="keyword",xmlParserOption,...){


	message("calling c++ parser...")
#	browser()
	time1<-Sys.time()
	G<-GatingSet(x=xmlFileName,y=sampleIDs,includeGates=includeGates,sampNloc=sampNloc,xmlParserOption = xmlParserOption,dMode=dMode)
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
	G<-.addGatingHierarchy(G,files,execute,isNcdf,...)
#	time1<-Sys.time()

	message("done!")
#	print(Sys.time()-time1)

	G	
}
##construct object from existing gating hierarchy(gating template) and flow data
setMethod("GatingSet",c("GatingHierarchyInternal","character"),function(x,y,path=".",isNcdf=FALSE,dMode=1,...){
			
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
			Object<-new("GatingSetInternal")
			message("generating new GatingSet from the gating template...")
			Object@pointer<-.Call("R_NewGatingSet",x@pointer,getSample(x),samples,as.integer(dMode))
            Object@guid <- .uuid_gen()
			Object<-.addGatingHierarchy(Object,files,execute=TRUE,isNcdf=isNcdf,...)
			return(Object)
		})
   
############################################################################
#constructing gating set
############################################################################
.addGatingHierarchy<-function(G,files,execute,isNcdf,compensation=NULL,wsversion,...){
#	browser()
    if(length(files)==0)
      stop("not sample to be added to GatingSet!")
	#environment for holding fs data,each gh has the same copy of this environment
	globalDataEnv<-new.env(parent=emptyenv())
	
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

	for(i in 1:nFiles)
	{
		file<-files[i]		
		sampleName<-basename(file)
		gh<-new("GatingHierarchyInternal",pointer=G@pointer,name=sampleName)
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
              
            #alter colnames(replace "/" with "_") so that it is consistent with flowJO convention
            colnames(data) <- gsub("/","_",colnames(data))
			
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
				
				##add prefix to parameter names
				cnd<-colnames(data)
				if(is.null(cnd)){cnd<-as.vector(parameters(data)@data$name)}
				wh<-match(parameters(compobj),cnd)
				
				cnd[wh]<-paste(comp$prefix,parameters(compobj),comp$suffix,sep="")
				
				#colnames(data)<-cnd;
				e<-exprs(data)
				d<-description(data);
				p<-parameters(data);
				p@data$name<-cnd
				colnames(e)<-cnd;
				data<-new("flowFrame",exprs=e,description=d,parameters=p)
	#			browser()
				#save raw or compensated data 
				message("saving compensated data");
				if(isNcdf)
					addFrame(fs,data,sampleName)#once comp is moved to c++,this step can be skipped
				else
					assign(sampleName,data,fs@frames)#can't use [[<- directly since the colnames are inconsistent at this point
			}else{
				#if cid is -2 add to ncdf flow set. Fixes bug where a missing compensation matrix with ncdfFlow does not save the uncompensated data.
				if(isNcdf)
					addFrame(fs,data,sampleName)
			}	
		}
		
		gh@flag<-execute #assume the excution would succeed if the entire G gets returned finally
		set[[i]]<-gh
	}
	names(set)<-basename(files)
	G@set<-set
#	browser()
#	print(Sys.time()-time1)
#	
#	time1<-Sys.time()
	
	if(execute)
	{
#		browser()
		#update colnames slot for flowSet
		#can't do it before fs fully compensated since
		#compensate function check the consistency colnames between input flowFrame and fs
		colnames(fs)<-colnames(data)
		
		#attach filename and colnames to internal stucture for gating

#		browser()
		assign("ncfs",fs,globalDataEnv)
		
		
		lapply(G,function(gh){
					
					
					sampleName<-getSample(gh)
					
					message(paste("gating",sampleName,"..."))
					#stop using gating API of cdf-version because c++ doesn't store the view of ncdfFlowSet anymore

				
					
					data<-fs[[sampleName]]
					mat<-exprs(data)
                    #get gains from keywords
                    this_pd <- pData(parameters(data))
                    paramIDs <- rownames(pData(parameters(data)))
                    key_names <- paste(paramIDs,"G",sep="")
                    kw <- keyword(data)
                    kw_gains <- kw[key_names]
                    gains <- as.numeric(kw_gains)
                    names(gains) <- this_pd$name
                    #select the gain that is not 1
#                    gains <- gains[as.integer(gains)!=1]
                    #check gates that needs to be updated by gains
#                    allNodes<-getNodes(gh)[-1]
#                    lapply(allNodes,function(this_node){
#                          browser()
#                          this_gate <- getGate(gh,this_node)
#                          this_params <- parameters(this_gate)
#                          lapply(this_params,function(this_param){
#                                browser()
#                                matchInd <- match(this_param,names(gains))
#                                if(!is.na(matchInd)){
#                                  this_gain <- gains[matchInd]
#                                  
#                                }
#                              })
#                        })
                    
                    
                                        
					.Call("R_gating",gh@pointer,mat,sampleName,gains,nodeInd=0,recompute=FALSE)
					#update data with transformed data
					exprs(data)<-mat
					fs[[sampleName]]<-data#update original flowSet/ncdfFlowSet
						
#					time_cpp<<-time_cpp+(Sys.time()-time1)
#				browser()
					#range info within parameter object is not always the same as the real data range
					#it is used to display the data.
					#so we need update this range info by transforming it
			
#					localDataEnv<-nodeDataDefaults(gh@tree,"data")
#					comp<-.Call("R_getCompensation",G@pointer,sampleName)	

#					browser()
#					cal<-getTransformations(gh)
                    
                    .transformRange(gh,wsversion)  
                    
					
                    
#					browser()
					
				})
		
		
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
              range(exprs(dataenv$data$ncfs[[sampleName]])[,this_chnl])
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
.transformRange<-function(gh,wsversion){

    sampleName <- getSample(gh)
    dataenv <- nodeDataDefaults(gh@tree,"data")
     cal<-getTransformations(gh)
     comp<-.Call("R_getCompensation",gh@pointer,sampleName)
     prefix <- comp$prefix
     suffix <- comp$suffix
	frmEnv<-dataenv$data$ncfs@frames
	rawRange<-range(get(sampleName,frmEnv))
	tempenv<-new.env()
	assign("axis.labels",vector(mode="list",ncol(rawRange)),envir=tempenv);
#    browser()
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
                    range(exprs(dataenv$data$ncfs[[sampleName]])[,this_chnl])
                  }else{
                    rawRange[,i]
                  }
					
				}
			})
	copyEnv(tempenv,dataenv);
	
#	browser()		
	datarange<-t(rbind(datarange[2,]-datarange[1,],datarange))
	datapar<-parameters(get(sampleName,frmEnv))
	pData(datapar)[,c("range","minRange","maxRange")]<-datarange
	
	#gc(reset=TRUE)
#	assign("datapar",datapar,dataenv)
	eval(substitute(frmEnv$s@parameters<-datapar,list(s=sampleName)))
#	eval(expression(data@parameters<-datapar),envir=dataenv)
	#gc(reset=TRUE)
}
setMethod("haveSameGatingHierarchy",signature=c("GatingSetInternal","missing"),function(object1,object2=NULL){
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

#plot by children index
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
     
      ind <- .getNodeInd(x[[1]],y)
      plotGate(x,ind,...)
    })

#TODO:merge this to .plotGate routine
#fitGate is used to disable behavior of plotting the gate region in 1d densityplot
#overlay is either the gate indice list or event indices list
.plotGateGS<-function(x,y,formula=NULL,cond=NULL,main=NULL,margin=FALSE,smooth=FALSE,type=c("xyplot","densityplot"),xlab=NULL,ylab=NULL,xlim=NULL,ylim=NULL,stat=TRUE,fitGate=FALSE,overlay=NULL,...){

	samples<-getSamples(x)
	type<- match.arg(type)
	
	gh<-x[[1]]
	if(is.list(y))
		pid<-y$parentId
	else
		pid<-getParent(gh,y)
	
	if(is.null(main)){
		fjName<-getNodes(gh,pid,isPath=T)
		main<-fjName
	}
	
	if(is.list(y))
	{
#		browser()
		curGates<-sapply(samples,function(curSample){
					
					filters(lapply(y$popIds,function(y)getGate(x[[curSample]],y)))
				},simplify=F)
		curGates<-as(curGates,"filtersList")
	}else
	{
		curGates<-getGate(x,y)
		
		if(suppressWarnings(any(is.na(curGates)))){
			message("Can't plot. There is no gate defined for node ",getNodes(gh,y));
			invisible();			
			return(NULL)
		}
		
	}			
	
	parentdata<-getData(x,pid)
	parentFrame<-parentdata[[1]]
			
	smooth<-ifelse(nrow(parentFrame)<100,TRUE,smooth)
	#################################
	# setup axis labels and scales
	################################
	if(class(curGates[[1]])=="BooleanGate")
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
	panelFunc<-panel.xyplot.flowset
		
	
	if(type=="xyplot")
	{
		if(length(params)==1)
		{
			yParam<-"SSC-A"
			
			if(params=="SSC-A")
				xParam<-"FSC-A"
			else
				xParam<-params
			params<-c(yParam,xParam)
		}else
		{
			yParam=params[1]
			xParam=params[2]
			
		}
	
		axisObject<-.formatAxis(gh,parentFrame,xParam,yParam,...)
        if(is.null(xlab)){
          xlab <- axisObject$xlab
        }
        if(is.null(ylab)){
          ylab <- axisObject$ylab
        }
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
		
		#################################
		# the actual plotting
		################################
		if(is.null(formula))
		{
			formula<-mkformula(params,isChar=TRUE)
			if(!is.null(cond))
				formula<-paste(formula,cond,sep="|")
			formula<-as.formula(formula)
		}
		
		res<-xyplot(x=formula
				,data=parentdata[,params]
				,filter=curGates
				,xlab=xlab
				,ylab=ylab
				,margin=margin
				,smooth=smooth
				,scales=axisObject$scales
				,main=main
				,stat=stat
				,panel=panelFunc
				,overlay=overlay
				,...
		)
	}else
	{
		if(length(params)==1)
		{
			
#			browser()
			axisObject<-.formatAxis(gh,parentFrame,xParam=params,yParam=NULL,...)
            if(is.null(xlab)){
              xlab <- axisObject$xlab
            }
			if(is.null(formula))
			{
				formula<-mkformula(params,isChar=TRUE)
				if(!is.null(cond))
					formula<-paste(formula,cond,sep="|")
				formula<-as.formula(formula)
			}
			res<-densityplot(x=formula
								,data=parentdata[,params]
								,filter=curGates
								,xlab=xlab
#								,ylab=axisObject$ylab
								,margin=margin
#								,smooth=smooth
#								,scales=axisObject$scales
								,main=main
								,stat=stat
								,fitGate=fitGate
#								,panel=panelFunc
								,...
								)
		}
	}
	return(res)	
}
##plot by prarent index
plotGate_labkey<-function(G,parentID,x,y,smooth=FALSE,cond=NULL,xlab=NULL,ylab=NULL,...){
	#get all childrens
	cids<-getChildren(G[[1]],parentID)
	if(length(cids)>0)
	{
		#try to match to projections
#		browser()
		isMatched<-lapply(cids,function(cid){
					g<-getGate(G[[1]],cid)
					if(class(g)!="BooleanGate") 
					{
						prj<-parameters(g)
						if(length(prj)==1)
						{
							return (prj%in%c(x,y))
							
						}else
						{
							revPrj<-rev(prj)
							if((x==prj[1]&&y==prj[2])||(x==revPrj[1]&&y==revPrj[2]))
								return (TRUE)
							else
								return (FALSE)	
						}
					}else
					return (FALSE)
				})
		
		ind<-which(unlist(isMatched))
		if(length(ind)>0)
			isPlotGate<-TRUE
		else
			isPlotGate<-FALSE
	}else
		isPlotGate<-FALSE
	formula1<-paste("`",y,"`~`",x,"`",sep="")
	if(!is.null(cond))
		formula1<-paste(formula1,cond,sep="|")
	formula1<-as.formula(formula1)
#	browser()
	if(isPlotGate)
		plotGate(G,cids[ind],formula=formula1,smooth=smooth,xlab=xlab,ylab=ylab,...)
	else
	{
		fs<-getData(G,parentID)
		axisObject<-.formatAxis(x=G[[1]],data=fs[[1]],xParam=x,yParam=y,...)
        if(is.null(xlab)){
          xlab <- axisObject$xlab
        }
        if(is.null(ylab)){
          ylab <- axisObject$ylab
        }
		xyplot(formula1
				,fs
				,smooth=smooth
				,xlab=xlab
				,ylab=ylab
				,scales=axisObject$scales
				,...)
	}
	
}



setGeneric("clone", function(x,...){standardGeneric("clone")})
setMethod("clone",c("GatingSetInternal"),function(x,...){

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
setMethod("clone",c("GatingSet"),function(x,...){
#			browser()
			.cloneGatingSet(x,...)
		})

setGeneric("recompute", function(x,...){standardGeneric("recompute")})
setMethod("recompute",c("GatingSetInternal"),function(x,y){
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


setReplaceMethod("ncFlowSet",signature(x="GatingSetInternal"),function(x,value){
			
			callNextMethod(x,value)
			#associate the internal structure with the new cdf

			x
			
		})


setMethod("show","GatingSetInternal",function(object){
			
			callNextMethod(object)
			
			for(i in 1:length(object@set))
			{
#				browser()
				if(!identical(object@set[[i]]@pointer,object@pointer))
					stop("GatingHierarchy ",names(object@set)[i]," has a differnent pointer than GatingSet!")
			}
			
		})
#overload the getSamples for GatingSet to ensure the sample names comes from pdata of fs    
setMethod("getSamples","GatingSetInternal",function(x){
      sampleNames(getData(x))
    })
setMethod("getData",signature(obj="GatingSetInternal",y="missing"),function(obj,y,tsort=FALSE){
      
        ncFlowSet(obj)
    })
setMethod("getData",signature(obj="GatingSetInternal",y="numeric"),function(obj,y,tsort=FALSE){
      
      this_node <- getNodes(obj[[1]])[y]
      getData(obj,this_node)
      
    })

setMethod("getData",signature(obj="GatingSetInternal",y="character"),function(obj,y,tsort=FALSE){
			
            this_data <- getData(obj)                        
            if(y == "root"){
              this_data  
            }else{
				#subset by indices
				indices<-lapply(obj,getIndices,y)
                this_data <- Subset(this_data,indices)
							
                this_data	
			}
			
		})
#note:it doesn't use metadata slot of GatingSet, instead it directly access the pData of flowSet/ncdfFlowSet
setMethod("pData","GatingSetInternal",function(object){
			pData(ncFlowSet(object))
		})
setReplaceMethod("pData",c("GatingSetInternal","data.frame"),function(object,value){
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
setMethod("[",c("GatingSetInternal"),function(x,i,j,...,drop){
            
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
		
		
#setMethod("rbind2",c("GatingSetInternal","GatingSetInternal"),function(x,y,...){
#			#up to user to make sure x,y have the same gating hierarchy
##			if(!haveSameGatingHierarchy(x,y)){
##				stop("x and y must have the same gating hierarchy for each sample")
##			}
#			
#			rbind2(GatingSetList(list(x,y)),...)		
#		})

setMethod("getGate",signature(obj="GatingSet",y="character"),function(obj,y,tsort=FALSE){
			lapply(obj,function(x)getGate(x,y))
		})

setMethod("setNode"
    ,signature(x="GatingSet",y="numeric",value="character")
    ,function(x,y,value,...){
      lapply(x,function(gh){
            setNode(gh,y,value,...)
          })
    })
setMethod("setNode"
    ,signature(x="GatingSet",y="character",value="character")
    ,function(x,y,value,...){
      setNode(x,.getNodeInd(x[[1]],y),value)
    })
