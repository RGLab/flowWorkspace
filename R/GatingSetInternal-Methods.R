# TODO: Add comment
# 
# Author: wjiang2
###############################################################################

setMethod("getSamples","GatingSetInternal",function(x,isFullPath=FALSE){
			
			.Call("R_getSamples",x@pointer)
		})

setMethod("length","GatingSetInternal",function(x){
			length(getSamples(x));
		})


setMethod("parseWorkspace",signature("GatingSetInternal"),function(obj,groupID,...){
			
			.Call("R_parseWorkspace",obj@pointer,groupID)
			
		})
setMethod("[[",signature("GatingSetInternal"),function(x,i,j,...){
			
			
			if(class(i)=="character")
				ghPtr<-.Call("R_getGatingHierarchyS",x@pointer,i)
			else
				if(class(i)=="integer"||class(i)=="numeric")
				{
					if(i>=1&&i<=length(x))
						ghPtr<-.Call("R_getGatingHierarchyI",x@pointer,as.integer(i)-1)
					else
						stop("index out of range:",i)
				}
					
				else
					stop("Not valid index type(character or integer):",i)
			return(new("GatingHierarchyInternal",pointer=ghPtr));
		})


setMethod("show","GatingSetInternal",function(object){
			cat("A GatingSet(internal) with ",length(object), " samples\n")
			for(i in 1:length(object)){
				cat(i,". ");
				show(object[[i]])
			}
		})

setMethod("getPopStats","GatingSet",function(x,...){
			if(!haveSameGatingHierarchy(x)){
				message("Can't retrieve population statistics table for GatingSet. The samples don't all have the same gating schemes.")
			}
			r<-do.call(cbind,lapply(x,function(y)getPopStats(y)$flowCore.freq))
			rownames(r)<-rownames(getPopStats(x[[1]]));
			r
		})
