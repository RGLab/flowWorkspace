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
