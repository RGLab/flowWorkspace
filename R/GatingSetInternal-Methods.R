# TODO: Add comment
# 
# Author: wjiang2
###############################################################################

setMethod("setData",c("GatingSetInternal","flowSet"),function(this,value){
			#pass the filename and channels to c structure
			if(inherits(value,"ncdfFlowSet"))
			{
				
				.Call("R_setData",this@pointer,value@file,sampleNames(value),colnames(value))
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
		
			
	}else
	{
		files<-samples
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
				message("Compensating");
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
					
					if(grepl("Acquisition-defined",nm)){
						###Code to compensate the sample using the acquisition defined compensation matrices.
						message("Compensating with Acquisition defined compensation matrix");
						#browser()
						compobj<-compensation(spillover(data)$SPILL)
						gh@compensation<-spillover(data)$SPILL
						res<-try(compensate(data,compobj),silent=TRUE)
						if(inherits(res,"try-error")){
							message("Data is probably stored already compensated");
						}else{
							data<-res
							rm(res);
					
						}
#						browser()
						cnd<-colnames(data)
						wh<-cnd%in%parameters(compobj)
						cnd[wh]<-paste(comp$prefix,parameters(compobj),comp$suffix,sep="")
						e<-exprs(data)
						d<-description(data);
						p<-parameters(data);
						p@data$name<-cnd
						colnames(e)<-cnd;
						data<-new("flowFrame",exprs=e,description=d,parameters=p)
					
					}
					
				}
				
				#save compensated data to cdf
				message("saving compensated data");

				addFrame(fs,data,sampleName)#once comp is moved to c++,this step can be skipped
				
				#transformaton is done and cdf data is updated within R_gating call
			}
			
			gh@flag<-execute #assume the excution would succeed if the entire G gets returned finally
	
			gh
		}
		,USE.NAMES=TRUE)
	
	if(execute)
	{
		colnames(fs)<-colnames(fs[[1]])#update colnames slot for ncdfFlowSet
		
		#attach filename and colnames to internal stucture for gating
		setData(G,fs)
		assign("fs",fs,dataEnv)
			

		lapply(G,function(gh){
#					setData(gh,fs)
					sampleName<-getSample(gh)
					message(paste("gating",sampleName,"..."))
					.Call("R_gating",gh@pointer,sampleName)

					#update range info for flowFrames due to the transformation
	#				newRange<-apply(exprs(fs@frames$"004_A1_A01.fcs"),2,range)
	#				pData(parameters(fs@frames$"004_A1_A01.fcs"))[,c("minRange","maxRange")]<-t(newRange)
				
					newRange<-apply(exprs(fs[[sampleName]]),2,range)
					pData(parameters(fs[[sampleName]]))[,c("minRange","maxRange")]<-t(newRange)
				
					#udpate the data range after tranformation
#					fr<-fs[[sampleName]]
#					datarange<-sapply(1:dim(range(fr))[2],function(i){
#								browser()
#							#added gsub
#							j<-grep(gsub(">","",gsub("<","",names(range(fr))))[i],names(cal));
#							if(length(j)!=0){
#								rw<-range(fr)[,i];
#								if(attr(cal[[j]],"type")!="gateOnly"){
#									r<-cal[[j]](c(rw))
#								}else{
#									r<-rw
#								}
#								###An unfortunate hack. If we use the log transformation, then negative values are undefined, so
#								##We'll test the transformed range for NaN and convert to zero.
#								r[is.nan(r)]<-0;
#								###Is this transformed?
#								if(all(rw==r)){
#									#No transformation
#									raw<-seq(r[1],r[2],by=(r[2]-r[1])/10)
#									signif(raw,2)
#									pos<-raw;
#								}else{
#									#based on the range
#									#Inverse transform;
#									f<-splinefun(cal[[j]](seq(rw[1],rw[2],l=100000)),seq(rw[[1]],rw[[2]],l=100000),method="natural")
#									raw<-signif(f(seq(r[1],r[2],l=20)),2);
#									pos<-signif(cal[[j]](raw),2)
#								}
#								assign("i",i,dataenv)
#								assign("raw",raw,dataenv);
#								assign("pos",pos,dataenv);
#								eval(expression(axis.labels[[i]]<-list(label=as.character(raw),at=pos)),envir=dataenv);
#								return(r);
#							}else{
#								range(get("data",dataenv))[,i]
#							}
#						})
#				datarange<-t(rbind(datarange[2,]-datarange[1,],datarange))
#				datapar<-parameters(get("data",dataenv));
#				datapar@data[,c("range","minRange","maxRange")]<-datarange
#				#gc(reset=TRUE)
#				assign("datapar",datapar,dataenv)
#				eval(expression(data@parameters<-datapar),envir=dataenv)
#					wh<-grep("^Time$",data@parameters@data$name)
#					if(length(wh!=0)){
#						#gc(reset=TRUE);
#						parameters(data)@data[wh,4:5]<-range(exprs(data[,wh]));
#						#gc(reset=TRUE);
#						parameters(data)@data[wh,3]<-diff(range(exprs(data[,wh])));
#					}
				
				})
		#update data environment
		assign("fs",fs,dataEnv)
	
	}	
	
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

