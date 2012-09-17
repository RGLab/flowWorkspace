# TODO: Add comment
# 
# Author: wjiang2
###############################################################################
archive<-function(G,file=tempfile()){
	
	filename<-basename(file)
	dirname<-dirname(file)
	filename<-sub(".tar$","",filename)
#	browser()
	if(!file.exists(dirname))
		stop("Folder '",dirname, "' does not exist!")
	rds.file<-tempfile(tmpdir=dirname,fileext=".rds")
	dat.file<-tempfile(tmpdir=dirname,fileext=".dat")
	toTar<-c(rds.file,dat.file)
	#save ncdf file
	if(flowWorkspace:::isNcdf(G[[1]]))
	{	
		message("saving ncdf...")
		from<-ncFlowSet(G)@file
		ncFile<-file.path(dirname,basename(from))
		file.copy(from=from,to=ncFile)
		toTar<-c(toTar,ncFile)
	}
		
	message("saving tree object...")
	#save external pointer object
	.Call("R_saveGatingSet",G@pointer,dat.file)

	message("saving R object...")
	saveRDS(G,rds.file)

#	browser()
#	message("Archive the GatingSet...")
	curDir<-getwd()
	setwd(dirname)
	system(paste("tar -cf ",file,paste(basename(toTar),collapse=" ")))
#	tar(tarfile=file,files=toTar) #somehow the R internal tar doesn't work
	
	#remove intermediate files
	file.remove(toTar)
	setwd(curDir)
	message("Done\nTo reload it, use 'unarchive' function\n")
	
}
	

unarchive<-function(file){
	
	if(!file.exists(file))
		stop(file,"' not found!")
#	browser()
	files<-untar(tarfile=file,list=TRUE)
	
#	message("extracting files...")
	output<-tempdir()
#	system(paste("tar -xf ",file))
	untar(tarfile=file,exdir=output)
	
	dat.file<-file.path(output,files[grep(".dat$",files)])
	rds.file<-file.path(output,files[grep(".rds$",files)])
	
	nc.file<-file.path(output,files[grep(".nc$",files)])
#	browser()
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
	
	#clean up the intermediate files
	file.remove(c(dat.file,rds.file))
	message("Done")
	return (gs)
	
}


setMethod("setData",c("GatingSetInternal","flowSet"),function(this,value){
			#pass the filename and channels to c structure
			if(inherits(value,"ncdfFlowSet"))
			{
				
				.Call("R_setData",this@pointer,value@file,sampleNames(value),colnames(value))
			}
		})

.parseWorkspace<-function(xmlFileName,sampleIDs,execute,path,dMode,isNcdf,includeGates,flowSetId=NULL,sampNloc="keyword",...){


	message("calling c++ parser...")
	
	time1<-Sys.time()
	G<-GatingSet(x=xmlFileName,y=sampleIDs,includeGates=includeGates,sampNloc=sampNloc,dMode=dMode)
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
			file<-gsub("\\)","\\\\)",gsub("\\(","\\\\(",file))
			absPath<-list.files(pattern=paste("^",file,"",sep=""),path=path,recursive=TRUE,full=TRUE)
			
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
	G<-.addGatingHierachy(G,files,execute,isNcdf,...)
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
			
			Object<-.addGatingHierachy(Object,files,execute=TRUE,isNcdf)
			return(Object)
		})
############################################################################
#constructing gating set
############################################################################
.addGatingHierachy<-function(G,files,execute,isNcdf,compensation=NULL,...){
#	browser()
	#environment for holding fs data,each gh has the same copy of this environment
	globalDataEnv<-new.env()
	
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
#	isColUpdated<-FALSE
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
				data<-read.FCS(file)
			else
				data<-fs[[sampleName]]
			
#			browser()
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
#				cnd<-colnames(data)
#				if(is.null(cnd)){cnd<-as.vector(parameters(data)@data$name)}
#				wh<-cnd%in%parameters(compobj)
#				cnd[wh]<-paste(comp$prefix,parameters(compobj),comp$suffix,sep="")
#				
#				#colnames(data)<-cnd;
#				e<-exprs(data)
#				d<-description(data);
#				p<-parameters(data);
#				p@data$name<-cnd
#				colnames(e)<-cnd;
#				data<-new("flowFrame",exprs=e,description=d,parameters=p)						
				
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
#						browser()
#					cnd<-colnames(data)
#					wh<-cnd%in%parameters(compobj)
#					cnd[wh]<-paste(comp$prefix,parameters(compobj),comp$suffix,sep="")
#					e<-exprs(data)
#					d<-description(data);
#					p<-parameters(data);
#					p@data$name<-cnd
#					colnames(e)<-cnd;
#					data<-new("flowFrame",exprs=e,description=d,parameters=p)
					
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
		setData(G,fs)
#		browser()
		assign("ncfs",fs,globalDataEnv)
		
		
		lapply(G,function(gh){
					
					
					sampleName<-getSample(gh)
					
					message(paste("gating",sampleName,"..."))
#					browser()
#					time1<-Sys.time()
					if(isNcdf)
						.Call("R_gating_cdf",gh@pointer,sampleName,nodeInd=0,recompute=FALSE)
					else
					{
						data<-fs[[sampleName]]
						mat<-exprs(data)
						.Call("R_gating",gh@pointer,mat,sampleName,nodeInd=0,recompute=FALSE)
						#update fs with transformed data
						exprs(data)<-mat
						assign(sampleName,data,fs@frames)##suppose to be faster than [[<-
						
					}
					
#					time_cpp<<-time_cpp+(Sys.time()-time1)
#				browser()
					#range info within parameter object is not always the same as the real data range
					#it is used to display the data.
					#so we need update this range info by transforming it
					
					localDataEnv<-nodeDataDefaults(gh@tree,"data")
					comp<-.Call("R_getCompensation",G@pointer,sampleName)	

					
					cal<-getTransformations(gh)					
					.transformRange(localDataEnv,cal,sampleName,prefix=comp$prefix,suffix=comp$suffix)
					
#					browser()
					
				})
		
		
	}
	G
}


.transformRange<-function(dataenv,cal,sampleName,prefix,suffix){

	frmEnv<-dataenv$data$ncfs@frames
	rawRange<-range(get(sampleName,frmEnv))
	assign("axis.labels",vector(mode="list",ncol(rawRange)),envir=dataenv);
#	browser()
	datarange<-sapply(1:dim(rawRange)[2],function(i){
				#added gsub

				j<-grep(gsub(suffix,"",gsub(prefix,"",names(rawRange)))[i],names(cal));
				if(length(j)!=0){
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
#						#No transformation
#						raw<-seq(r[1],r[2],by=(r[2]-r[1])/10)
#						signif(raw,2)
#						pos<-raw;
#					}else{
						#based on the range
						#Inverse transform;
#						browser()
						if(attr(cal[[j]],"type")=="log")
							f<-function(x){10^x}
						else
						{
							toScale<-seq(rw[[1]],rw[[2]],l=100000)
							fromScale<-cal[[j]](toScale)
							f<-splinefun(fromScale,toScale,method="natural")
						}
						pos<-seq(r[1],r[2],l=20)
						raw<-signif(f(pos),2);
#						browser()
#						pos<-signif(cal[[j]](raw),2)
						
					
						assign("i",i,dataenv)
						assign("raw",raw,dataenv);
						assign("pos",pos,dataenv);
						eval(expression(axis.labels[[i]]<-list(label=as.character(raw),at=pos)),envir=dataenv);
					}
					return(r);
				}else{
					rawRange[,i]
				}
			})
	
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
#TODO:merge this to .plotGate routine
.plotGateGS<-function(x,y,formula=NULL,main=NULL,margin=FALSE,smooth=FALSE,xlab=NULL,ylab=NULL,xlim=NULL,ylim=NULL,stat=TRUE,scales=list(),...){			
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
		curGates<-sapply(getSamples(x),function(curSample){
					
					filters(lapply(y$popIds,function(y)getGate(x[[curSample]],y)))
				},simplify=F)
		curGates<-as(curGates,"filtersList")
	}else
	{
		curGates<-sapply(getSamples(x),function(curSample){
					
					getGate(x[[curSample]],y)
				},simplify=F)
		
		if(suppressWarnings(is.na(curGates))){
			message("Can't plot. There is no gate defined for node ",getNode(gh,y));
			invisible();			
			return(NULL)
		}
		
	}			
	
	parentdata<-getData(x,pid)
	parentFrame<-parentdata[[1]]
#			browser()
	smooth<-ifelse(nrow(parentFrame)<100,TRUE,smooth)
	#################################
	# setup axis labels and scales
	################################
	if(class(curGates)=="BooleanGate")
	{
		stop("bool gate plot is not supported for gatingSet yet! ")
#		params<-rev(parameters(getGate(gh,getParent(gh,y))))
#		ind<-getIndices(gh,y)
#		curGate<-getData(x)[ind,params]##get gated pop from indexing the root pop because ind here is global
#		
#		panelFunc<-panel.xyplot.flowFrame.booleanGate
	}else
	{
		if(class(curGates[[1]])=="filters")
			params<-rev(parameters(curGates[[1]][[1]]))
		else
			params<-rev(parameters(curGates[[1]]))
		panelFunc<-panel.xyplot.flowset
	}
	
	
	if(length(params)==1)
	{
		xParam=params
		yParam="SSC-A"
		params<-c(yParam,xParam)
	}else
	{
		yParam=params[1]
		xParam=params[2]
		
	}
	pd<-pData(parameters(parentFrame))
	xObj<-.getChannelMarker(pd,xParam)
	yObj<-.getChannelMarker(pd,yParam)
	
	xlab<-sub("NA","",paste(unlist(xObj),collapse=" "))
	ylab<-sub("NA","",paste(unlist(yObj),collapse=" "))
#			browser()
	if(length(params)==2){
		xParam.ind<-match(xParam,pd$name)
		yParam.ind<-match(yParam,pd$name)
		x.labels<-getAxisLabels(gh)[[xParam.ind]]
		y.labels<-getAxisLabels(gh)[[yParam.ind]]
		
		#init the scales and x,y lim
		
		xlim=range(parentFrame)[,xParam]
		ylim=range(parentFrame)[,yParam]
		
		#update axis when applicable
		if(!is.null(x.labels))
		{
			xscales<-list(x=list(at=x.labels$at,labels=x.labels$label,rot=45))
			scales<-lattice:::updateList(xscales,scales)
			xlim=range(x.labels$at)
		}
		if(!is.null(y.labels))
		{	
			yscales<-list(y=list(at=y.labels$at,labels=y.labels$label))
			scales<-lattice:::updateList(scales,yscales)
			ylim=range(y.labels$at)
		}
		
	}
	
	
	
	
	#################################
	# the actual plotting
	################################
	if(is.null(formula))
		formula<-mkformula(params)
	res<-xyplot(x=formula
			,data=parentdata[,params]
			,filter=curGates
			,xlab=xlab
			,ylab=ylab
			,margin=margin
			,smooth=smooth
			,scales=scales
			,main=main
			,stat=stat
			,panel=panelFunc
			,...
	)	
	return(res)	
}

plotGate_labkey<-function(G,parentID,x,y,smooth=FALSE,...){
#	res<-QUALIFIER:::.queryStats(db,statsType="proportion",pop=pop,gsid=1,isTerminal=T)
#	res<-as.data.frame(cast(res,...~stats))
#	QUALIFIER:::qa.GroupPlot(db=db,df=res,statsType="proportion",par=.db$lattice)
	#get all childrens
	cids<-getChildren(G[[1]],parentID)
	if(length(cids)>0)
	{
		#try to match to projections
#		browser()
		isMatched<-lapply(cids,function(cid){
					g<-getGate(G[[1]],cid)
					prj<-parameters(g)
					revPrj<-rev(prj)
					if((x==prj[1]&&y==prj[2])||(x==revPrj[1]&&y==revPrj[2]))
						return (TRUE)
					else
						return (FALSE)
				})
		
		ind<-which(unlist(isMatched))
		if(length(ind)>0)
			isPlotGate<-TRUE
		else
			isPlotGate<-FALSE
	}else
		isPlotGate<-FALSE
#	formula1<-as.formula(paste("`",y,"`~`",x,"`",sep=""))
	formula1<-mkformula(c(y,x))
	if(isPlotGate)
		plotGate(G,cids[ind],formula=formula1,smooth=smooth,...)
	else
	{
		fs<-getData(G,parentID)
		xyplot(formula1,fs,smooth=smooth,...)
	}
		
	
	
}


setGeneric("clone", function(x,...){standardGeneric("clone")})
setMethod("clone",c("GatingSetInternal"),function(x,...){
#			browser()
			tmpfile<-tempfile()
			archive(x,file=tmpfile)
			gs<-unarchive(tmpfile)
			gs
		})
setMethod("clone",c("GatingSet"),function(x,...){
#			browser()
			cloneGatingSet(x,...)
		})

setGeneric("recompute", function(x,...){standardGeneric("recompute")})
setMethod("recompute",c("GatingSetInternal"),function(x,y){
			if(missing(y))
				y<-0
			if(is.character(y))
				y<-match(y,getNodes(x[[1]]))
			
			lapply(x,function(gh){
						
						
						sampleName<-getSample(gh)
						
						message(paste("gating",sampleName,"..."))
#					browser()
#					time1<-Sys.time()
						if(flowWorkspace:::isNcdf(gh))
							.Call("R_gating_cdf",gh@pointer,sampleName,nodeInd=as.integer(y),recompute=TRUE)
						else
						{
							data<-getData(gh)
							mat<-exprs(data)
							.Call("R_gating",gh@pointer,mat,sampleName,nodeInd=as.integer(y),recompute=TRUE)
							
						}
					})
			message("done!")
			invisible()
						
		})
setMethod("recompute",c("GatingSet"),function(x,y){
			recomputeGate(x=x,gate=y)
		})
