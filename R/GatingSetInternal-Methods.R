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
	print("calling c++ parser...")
#		browser()
	G<-new("GatingSetInternal",xmlFileName,sampleIDs,execute,dMode)
	print("c++ parsing is done!")
	samples<-.Call("R_getSamples",G@pointer)

	if(execute)
	{
		files<-file.path(path,samples)
		print("loading FCS files...")
		if(isNcdf){
			stopifnot(length(grep("ncdfFlow",loadedNamespaces()))!=0)
			fs<-read.ncdfFlowSet(files,flowSetId=ifelse(is.null(flowSetId),"New FlowSet",flowSetId))
		}else{
			fs<-read.flowSet(files)
		}
		setData(G,fs)
		#environment for holding fs data,each gh has the same copy of this environment
		dataEnv<-new.env()
		assign("fs",fs,dataEnv)	
	}
	
	G@set<-sapply(samples,function(x){
				
				
				gh<-new("GatingHierarchyInternal",pointer=G@pointer,name=x)
				
				if(execute)
				{
					setData(gh,fs)
					gh@dataEnv<-dataEnv	
				}
				
				gh@flag<-execute #assume the excution would succeed if the entire G gets returned finally
		
				gh
			}
			,USE.NAMES=TRUE)
	if(execute)
		lapply(G,function(hierarchy){
					sampleName<-getSample(hierarchy)
					print(paste("gating",sampleName,"..."))
					.Call("R_gating",hierarchy@pointer,sampleName)	
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

