# TODO: Add comment
# 
# Author: wjiang2
###############################################################################
###add support for win version
setMethod("getSampleGroups","flowJoWorkspace",function(x){
#			browser()
			win<-!x@version=="2.0"
			.getSampleGroups(x@doc,win)
		})

###add support for win version
.getSampleGroups<-function(x,win=FALSE){
	if(!win){
		do.call(rbind,xpathApply(x,"/Workspace/Groups/GroupNode",function(x){
							gid<-c(xmlGetAttr(x,"name"),xmlGetAttr(x,"groupID"));
							sid<-do.call(c,xpathApply(x,".//SampleRef",function(x){
												as.numeric(xmlGetAttr(x,"sampleID"))
											}))
							if(is.null(sid)){
								sid<-NA;
							}
							groups<-na.omit(data.frame(groupName=gid[[1]],groupID=as.numeric(gid[2]),sampleID=as.numeric(sid)));
						}))
	}else{
		#Note that groupID is from order of groupNode instead of from xml attribute 
		counter<-1
		do.call(rbind,xpathApply(x,"/Workspace/Groups/GroupNode",function(x){
#							browser()
							gid<-c(xmlGetAttr(x,"name"),xmlGetAttr(x,"groupID"));
							sid<-do.call(c,xpathApply(x,".//SampleRef",function(x){
												as.numeric(xmlGetAttr(x,"sampleID",default=NA))
											}))
							if(is.null(sid)){
								sid<-NA;
							}
#			browser()
							groups<-na.omit(data.frame(groupName=gid[[1]],groupID=counter,sampleID=as.numeric(sid)));
							counter<-counter+1
							groups
						}))
	}
}

splitGatingSetByNgates<-function(x){
	flowCore:::checkClass(x,"GatingSet");
#	browser()
	pData(x)$ngates<-unlist(lapply(x,function(x)length(getNodes(x))))
	#cluster by number of gates.. set up the groups
	groups<-by(pData(x),pData(x)$ngates,function(x)as.numeric(rownames(x)))
	#split the data
	x<-lapply(groups,function(y){
				x[y]
			})
	x
}

#made change to the original method to add flowJo option
setMethod("getPopStats","GatingSet",function(x,flowJo=FALSE,...){
#			browser()
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
setMethod("parseWorkspace",signature("character"),function(obj,groupID,dMode=0,execute=FALSE,...){
			
#			browser()
			if(missing(groupID))
			{
				groups<-""
			}else
			{
				groups<-list(groupID)
			}	
			
			res<-lapply(groups,.parseWorkspace,obj,groupID,execute,dMode)
			if(length(res)==1)
				res<-unlist(res)
			
			res
			
		})
.parseWorkspace<-function(xmlFileName,groupID,execute,dMode){
	G<-new("GatingSetInternal",xmlFileName,groupID,execute,dMode)
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
