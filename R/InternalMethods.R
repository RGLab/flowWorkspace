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

setMethod("plot",signature("GatingHierarchyInternal","missing"),function(x,y,layout="dot",width=3,height=2,fontsize=14,labelfontsize=14,fixedsize=FALSE,boolean=FALSE,...){
			
			browser()
			GXLFile<-.Call("R_plotGh",x@pointer)
			GXLFile<-file.path("~/rglab/workspace/flowWorkspace/src",GXLFile)
			sf<-file(GXLFile)
			g<-fromGXL(sf)
			close(sf)
			nAttrs <- list()
			nAttrs$label<-unlist(nodeData(g,attr="label"))
			attrs <- list(node=list(shape="rectangle", fixedsize=FALSE,fontsize=30))
			plot(g,nodeAttrs=nAttrs,attrs=attrs)
			
})
setMethod("plot",signature("GatingHierarchy","missing"),function(x,y,layout="dot",width=3,height=2,fontsize=14,labelfontsize=14,fixedsize=FALSE,boolean=FALSE,...){
			if(!boolean){	
				sub<-subGraph(x@nodes[which(!unlist(lapply(nodeData(x@tree,x@nodes,"metadata"),function(x)get("isBooleanGate",envir=x)),use.names=FALSE))],x@tree)
			}else{
				sub<-x@tree
			}
			nn<-sapply(nodes(sub),function(x)strsplit(x,"\\.")[[1]][2])
			nn[1]<-nodes(sub)[1]
			natr<-list();
			natr$label<-nn;
			options("warn"=-1)
			lay<-Rgraphviz::layoutGraph(sub,layoutType=layout,nodeAttrs=natr,attrs=list(graph=list(rankdir="LR",page=c(8.5,11)),node=list(fixedsize=FALSE,fontsize=fontsize,shape="rectangle")))
			renderGraph(lay)
			#plot(sub,nodeAttrs=natr,attrs=list(node=list(fixedsize=fixedsize,labelfontsize=labelfontsize,fontsize=fontsize,width=width,height=height,shape="rectangle")),y=layout,...);
			options("warn"=0)
		})

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

setMethod("show",signature("GatingHierarchyInternal"),function(object){
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
			cat("GatingHierarchyInternal with internal structure:")
			print(object@pointer)
			cat("\n")
		})

setMethod("getSamples","GatingSet",function(x,isFullPath=FALSE){
			as.vector(unlist(lapply(x,function(y){
										getSample(y,isFullPath)
									}),use.names=FALSE))
		})
setMethod("getSample","GatingHierarchy",function(x,isFullPath=FALSE){
			ifelse(isFullPath,file.path(x@dataPath,x@name),x@name)
			
		})

setMethod("getParent",signature(obj="GatingHierarchy",y="numeric"),function(obj,y,tsort=FALSE){
			return(match(getParent(obj,getNodes(obj,tsort=tsort)[y]),getNodes(obj,tsort=tsort)));
		})
setMethod("getParent",signature(obj="GatingHierarchy",y="character"),function(obj,y){
			setdiff(adj(ugraph(obj@tree),y)[[1]],adj(obj@tree,y)[[1]])
		})
setMethod("getChildren",signature(obj="GatingHierarchy",y="character"),function(obj,y){
			adj(obj@tree,y)[[1]]
		})

setMethod("getProp",signature(x="GatingHierarchy",y="character"),function(x,y){
			#Return the proportion of the population relative to the parent and relative to the total.
			#x is a graph of a gating hierarchy that has had data added to it.
			#y is nodename
			return(get("thisTot",envir=nodeData(x@tree,y,"metadata")[[1]])/get("parentTot",envir=nodeData(x@tree,y,"metadata")[[1]]))	
		})
setMethod("getTotal",signature(x="GatingHierarchy",y="character"),function(x,y){
			return(get("thisTot",envir=nodeData(x@tree,y,"metadata")[[1]]))
		})

setMethod("getGate",signature(obj="GatingHierarchy",y="character"),function(obj,y){
			if(.isBooleanGate.graphNEL(obj,y)){
				g<-get("gate",envir=nodeData(obj@tree,y,"metadata")[[1]])[[1]];
				return(g)
			}else{
				return(get("gate",envir=nodeData(obj@tree,y,"metadata")[[1]]))
			}
		})
#return gate y for a given hierarchy (by index)
setMethod("getGate",signature(obj="GatingHierarchy",y="numeric"),function(obj,y,tsort=FALSE){
			n<-getNodes(obj,tsort=tsort)[y]
			if(flowWorkspace:::.isBooleanGate.graphNEL(obj,n)){
				g<-get("gate",envir=nodeData(obj@tree,n,"metadata")[[1]])[[1]]
				return(g);
			}else{
				return(get("gate",envir=nodeData(obj@tree,n,"metadata")[[1]]))
			}
		})
#Return gate y for all samples in the gating set (by index).
#Warning: assume that all gating hierarchies are the same in the given gating set. We don't check for this, so be careful.
setMethod("getGate",signature(obj="GatingSet",y="numeric"),function(obj,y,tsort=FALSE){
			lapply(obj,function(x)getGate(x,y,tsort=tsort))
		})
