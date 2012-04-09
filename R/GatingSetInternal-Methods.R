# TODO: Add comment
# 
# Author: wjiang2
###############################################################################

#setMethod("getSamples","GatingSetInternal",function(x,isFullPath=FALSE){
#			
#			.Call("R_getSamples",x@pointer)
#		})
#
#setMethod("length","GatingSetInternal",function(x){
#			length(getSamples(x));
#		})


#setMethod("[[",signature("GatingSetInternal"),function(x,i,j,...){
#			
#			
#			if(class(i)=="character")
#			else
#				if(class(i)=="integer"||class(i)=="numeric")
#				{
#					if(i>=1&&i<=length(x))
#						ghPtr<-.Call("R_getGatingHierarchyI",x@pointer,as.integer(i)-1)
#					else
#						stop("index out of range:",i)
#				}
#					
#				else
#					stop("Not valid index type(character or integer):",i)
#			return(new("GatingHierarchyInternal",pointer=ghPtr));
#		})

#TODO:right now the this GatingSetInternal contructor depends on "[[" methods, it should be another way around
#which requires moving some of the contruction to cpp code
setMethod("parseWorkspace",signature("character"),function(obj,groupID,dMode=0,...){
			
#			browser()
#			G<-.Call("R_parseWorkspace",obj,groupID,dMode)
			G<-new("GatingSetInternal",xmlFileName=obj,groupID,dMode)
			samples<-.Call("R_getSamples",G@pointer)
			G@set<-sapply(samples,function(x){
#													ghPtr<-.Call("R_getGatingHierarchyS",G@pointer,i)
#													new("GatingHierarchyInternal",pointer=ghPtr)
													new("GatingHierarchyInternal",pointer=G@pointer,name=x)
													}
							,USE.NAMES=TRUE)
								
			G
			
		})
setMethod("haveSameGatingHierarchy",signature=c("GatingSetInternal","missing"),function(object1,object2=NULL){
#			em<-edgeMatrix(object1)
#			if(length(em)>=2){
#				return(all(sapply(2:length(em),function(i)identical(em[[1]],em[[i]])))& all(apply(do.call(cbind,lapply(object1,function(x)gsub("^.*\\.","",RBGL:::bfs(x@tree)))),1,function(x)x%in%x[1])))
#			}else{
#				return(TRUE)
#			}
			return(TRUE)
		})
#setMethod("show","GatingSetInternal",function(object){
#			cat("A GatingSet(internal) with ",length(object)," samples\n")
#			print(object@pointer)
#			cat("\n")
#			nGh<-length(object)
#			if(nGh>0)
#			{
#				for(i in 1:nGh)
#				{
#					cat(i,". ");
#					show(object[[i]])
#				}
#			}
#		})
#setMethod("lapply","GatingSetInternal",function(X,FUN,...){
#			##return a GatingSet unless the function FUN is not suitable, in which case return the list.
#			r<-lapply(X@set,FUN,...)
#			result<-try(new("GatingSet",set=r),silent=TRUE);
#			if(inherits(result,"try-error")){
#				result<-r;
#			}else{
#				result@metadata<-X@metadata;
#			}
#			result;
##		})
#setMethod("getPopStats","GatingSet",function(x,...){
#			if(!haveSameGatingHierarchy(x)){
#				message("Can't retrieve population statistics table for GatingSet. The samples don't all have the same gating schemes.")
#			}
#			r<-do.call(cbind,lapply(x,function(y)getPopStats(y)$flowCore.freq))
#			rownames(r)<-rownames(getPopStats(x[[1]]));
#			r
#		})
