# TODO: Add comment
# 
# Author: wjiang2
###############################################################################

#parseWorkspace_cpp<-function(obj,groupIDs,dMode=0,execute=FALSE,...){
#			
#			
#			res<-lapply(groupIDs,function(groupID)
#								.parseWorkspace(obj,groupID,execute,dMode)
#						)
#			if(length(res)==1)
#				res<-unlist(res)
#			
#			res
#			
#		}
.parseWorkspace<-function(xmlFileName,sampleIDs,execute,dMode){
	G<-new("GatingSetInternal",xmlFileName,sampleIDs,execute,dMode)
	samples<-.Call("R_getSamples",G@pointer)
	G@set<-sapply(samples,function(x){
#													ghPtr<-.Call("R_getGatingHierarchyS",G@pointer,i)
#													new("GatingHierarchyInternal",pointer=ghPtr)
				new("GatingHierarchyInternal",pointer=G@pointer,name=x)
			}
			,USE.NAMES=TRUE)
	
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
#made change to the original method to add flowJo option
setMethod("getPopStats","GatingSet",function(x,flowJo=FALSE,...){
			if(!haveSameGatingHierarchy(x)){
				message("Can't retrieve population statistics table for GatingSet. The samples don't all have the same gating schemes.")
			}
			r<-do.call(cbind,lapply(x,function(y){
#						browser()
								curStats<-getPopStats(y)
								if(flowJo)
								{
									curStats$flowJo.count/curStats$parent.total
								}
								
								else
									curStats$flowCore.freq
							}))
			rownames(r)<-rownames(getPopStats(x[[1]]));
			r
		})
