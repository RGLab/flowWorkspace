# TODO: Add comment
# 
# Author: wjiang2
###############################################################################

#setMethod("openWorkspace",signature=signature(file="character"),definition= function(file){
#			message("We do not fully support all features found in a flowJo workspace, nor do we fully support all flowJo workspaces at this time.")
#			if(inherits(file,"character")){
#				x<-xmlTreeParse(file,useInternal=TRUE);
#			}else{
#				stop("Require a filename of a workspace, but received ",class(x)[1]);
#			}
#			ver<-xpathApply(x,"/Workspace",function(x)xmlGetAttr(x,"version"))[[1]]
#			x<-new("flowJoWorkspace",version=ver,.cache=new.env(parent=emptyenv()),file=basename(file),path=dirname(file),doc=x)
#			x@.cache$flag=TRUE;
#			return(x);
#		})
#	
#setMethod("parseWorkspace",signature("flowJoWorkspace"),function(obj,name=NULL,execute=TRUE,isNcdf=FALSE,subset=NULL,nslaves=4,requiregates=TRUE,includeGates=TRUE,...){
#			if(isNcdf&!TRUE){
#				stop("isNcdf must be FALSE since you don't have netcdf installed");
#			}
#})
#


setMethod("parseWorkspace",signature("GatingSetInternal"),function(obj,groupID,...){
			
			
			.Call("R_parseWorkspace",obj@pointer,groupID)
			
			
		})
setMethod("[[",signature("GatingSetInternal"),function(x,i,j,...){
			
			
			if(class(i)=="character")
				ghPtr<-.Call("R_getGatingHierarchyS",x@pointer,i)
			else
				if(class(i)=="integer"||class(i)=="numeric")
					ghPtr<-.Call("R_getGatingHierarchyI",x@pointer,as.integer(i))
				else
					stop("Not valid index type(character or integer):",i
									)
			return(new("GatingHierarchyInternal",pointer=ghPtr));
		})
setMethod("show",signature("GatingSetInternal"),function(object){
#			cat("FlowJo Workspace Version ",object@version,"\n");
#			cat("File location: ",object@path,"\n");
#			cat("File name: ",object@file,"\n");
#			if(object@.cache$flag){
#				cat("Workspace is open.","\n");
#				cat("\nGroups in Workspace\n");
#				tbl<-table(Name=getSampleGroups(object)$groupName,GroupID=getSampleGroups(object)$groupID)
#				print(data.frame(Name=rownames(tbl),"Num.Samples"=diag(tbl)))
#			}else{	
#				cat("Workspace is closed.","\n")
#			}
			cat("Gating set with internal structure:")
			print(object@pointer)
			cat("\n")
		})


setMethod("getSamples","GatingSetInternal",function(x,isFullPath=FALSE){
			
			.Call("R_getSamples",x@pointer)
		})

#Return gate y for all samples in the gating set (by index).
#Warning: assume that all gating hierarchies are the same in the given gating set. We don't check for this, so be careful.
#setMethod("getGate",signature(obj="GatingSet",y="numeric"),function(obj,y,tsort=FALSE){
#			lapply(obj,function(x)getGate(x,y,tsort=tsort))
#		})
