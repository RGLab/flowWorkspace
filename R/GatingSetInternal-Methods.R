# TODO: Add comment
# 
# Author: wjiang2
###############################################################################

setMethod("setData",c("GatingSetInternal","flowSet"),function(this,value){
			if(inherits(value,"ncdfFlowSet"))
			{
				
				.Call("R_setData",this@pointer,value@file,colnames(value))
			}
		})

.parseWorkspace<-function(xmlFileName,sampleIDs,execute,path,dMode,isNcdf,flowSetId=NULL){
	G<-new("GatingSetInternal",xmlFileName,sampleIDs,execute,dMode)
	samples<-.Call("R_getSamples",G@pointer)
	if(execute)
	{
		files<-file.path(path,samples)
		if(isNcdf){
			stopifnot(length(grep("ncdfFlow",loadedNamespaces()))!=0)
			fs<-read.ncdfFlowSet(files,flowSetId=ifelse(is.null(flowSetId),"New FlowSet",flowSetId))
		}else{
			fs<-read.flowSet(files)
		}
		setData(G,fs)	
	}
	G@set<-sapply(samples,function(x){
				new("GatingHierarchyInternal",pointer=G@pointer,name=x)
			}
			,USE.NAMES=TRUE)
	if(execute)
		lapply(G,function(hierarchy).Call("R_gating",hierarchy@pointer,getSample(hierarchy)))
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

