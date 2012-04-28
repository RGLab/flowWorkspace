# TODO: Add comment
# 
# Author: wjiang2
###############################################################################

setMethod("setData",c("GatingSetInternal","flowSet"),function(this,value){
			#pass the filename and channels to c structure
			if(inherits(value,"ncdfFlowSet"))
			{
				
				.Call("R_setData",this@pointer,value@file,colnames(value))
			}
		})

.parseWorkspace<-function(xmlFileName,sampleIDs,execute,path,dMode,isNcdf,flowSetId=NULL){
#	browser()

	print("calling c++ parser...")
#		browser()
	G<-new("GatingSetInternal",xmlFileName,sampleIDs,execute,dMode)
#	browser()
	print("c++ parsing done!")
	samples<-.Call("R_getSamples",G@pointer)
	#environment for holding fs data,each gh has the same copy of this environment
	dataEnv<-new.env()
	
	#loading and filtering data
	if(execute)
	{
#		files<-file.path(path,samples)
		dataPaths<-vector("character")
		excludefiles<-vector("logical")
		for(file in samples){
#			browser()
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
		
		print("Creating ncdfFlowSet...")
		files<-file.path(dataPaths,samples)
		if(isNcdf){
			stopifnot(length(grep("ncdfFlow",loadedNamespaces()))!=0)
			fs<-read.ncdfFlowSet(files,flowSetId=ifelse(is.null(flowSetId),"New FlowSet",flowSetId),isWriteSlice=FALSE)
		}else{
			fs<-read.flowSet(files)
		}
		
			
	}
	
	############################################################################
	#constructing gating set
	############################################################################
	G@set<-	sapply(files,function(file){
			
			sampleName<-basename(file)
			gh<-new("GatingHierarchyInternal",pointer=G@pointer,name=sampleName)
			gh@dataEnv<-dataEnv
			
			#gating (including loading data,compensating,transforming and the actual gating)
			if(execute)
			{
				
				gh@dataPath<-dirname(file)
				
#					file<-getSample(gh,isFullPath=TRUE)
				
				message("Loading data file: ",file);
				data<-read.FCS(file);
				
				##################################
				#Compensating the data
				##################################
				comp<-.Call("R_getCompensation",G@pointer,sampleName)
				cid<-comp$cid
				
#				browser()
				if(cid!="-1" && cid!="-2"){
					message("Compensating");
					#compobj<-compensation(.getCompensationMatrices(doc)[[as.numeric(cid)]])
					marker<-comp$parameters
					compobj<-compensation(matrix(comp$spillOver,nrow=length(marker),ncol=length(marker),byrow=TRUE,dimnames=list(marker,marker)))
					
					#TODO this compensation will fail if the parameters have <> braces (meaning the data is stored compensated).
					#I need to handle this case properly.
					res<-try(compensate(data,compobj),silent=TRUE)
					if(inherits(res,"try-error")){
						message("Data is probably stored already compensated");
					}else{
						data<-res
						rm(res);
					}
					cnd<-colnames(data)
					if(is.null(cnd)){cnd<-as.vector(parameters(data)@data$name)}
					wh<-cnd%in%parameters(compobj)
					cnd[wh]<-paste(comp$prefix,parameters(compobj),comp$suffix,sep="")
					
					#colnames(data)<-cnd;
					e<-exprs(data)
					d<-description(data);
					p<-parameters(data);
					p@data$name<-cnd
					colnames(e)<-cnd;
					data<-new("flowFrame",exprs=e,description=d,parameters=p)						
					
				}
				else if(cid=="-2"){
					#TODO the matrix may be acquisition defined.
					message("No compensation");
				}
				else if(cid=="-1")
				{
					##Acquisition defined compensation.
					nm<-comp$comment
					
					if(grepl("Acquisition-defined",cid)){
						###Code to compensate the sample using the acquisition defined compensation matrices.
						message("Compensating with Acquisition defined compensation matrix");
						#browser()
						comp<-compensation(spillover(data)$SPILL)
						gh@compensation<-spillover(data)$SPILL
						res<-try(compensate(data,comp),silent=TRUE)
						if(inherits(res,"try-error")){
							message("Data is probably stored already compensated");
						}else{
							data<-res
							rm(res);
					
						}
						cnd<-colnames(data)
						wh<-cnd%in%parameters(comp)
						cnd[wh]<-paste(comp$prefix,parameters(compobj),comp$suffix,sep="")
						e<-exprs(data)
						d<-description(data);
						p<-parameters(data);
						p@data$name<-cnd
						colnames(e)<-cnd;
						data<-new("flowFrame",exprs=e,description=d,parameters=p)
					
					}
					
				}
				
				##################################
				#Transforming the data
				##################################
#				cal<-.Call("R_getTransformation",G@pointer,sampleName)				
#				message("Transforming");
				
#				axis.labels<-list();
				.flowJoTransform(environment(),cal);
#				#gc(reset=TRUE);
#				#wh<-which(data@parameters@data$name=="Time")
#				wh<-grep("^Time$",data@parameters@data$name)
#				if(length(wh!=0)){
#					#gc(reset=TRUE);
#					parameters(data)@data[wh,4:5]<-range(exprs(data[,wh]));
#					#gc(reset=TRUE);
#					parameters(data)@data[wh,3]<-diff(range(exprs(data[,wh])));
#				}
#
#
#					e<-new.env(parent=.GlobalEnv);

				#########################################
				#if use ncdfFlowSet,then add the transformed
				#matrix to the ncdf file
				addFrame(fs,data,sampleName)
			}
			
			gh@flag<-execute #assume the excution would succeed if the entire G gets returned finally
	
			gh
		}
		,USE.NAMES=TRUE)
#	browser()	
	colnames(fs)<-colnames(fs[[1]])#update colnames slot for ncdfFlowSet

	#attach filename and colnames to internal stucture for gating
	setData(G,fs)
	assign("fs",fs,dataEnv)
	
	if(execute)
		lapply(G,function(gh){
#					setData(gh,fs)
					sampleName<-getSample(gh)
					message(paste("gating",sampleName,"..."))
					.Call("R_gating",gh@pointer,sampleName)	
				})
	
	
	G	
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

