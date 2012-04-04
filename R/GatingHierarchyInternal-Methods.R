# TODO: Add comment
# 
# Author: wjiang2
###############################################################################


setMethod("plot",signature("GatingHierarchyInternal","missing"),function(x,y,layout="dot",width=3,height=2,fontsize=14,labelfontsize=14,fixedsize=FALSE,boolean=FALSE,...){
			
#			browser()
			GXLFile<-.Call("R_plotGh",x@pointer)
			GXLFile<-file.path("~/rglab/workspace/flowWorkspace/output",GXLFile)
			sf<-file(GXLFile)
			g<-fromGXL(sf)
			close(sf)
			nAttrs <- list()
			nAttrs$label<-unlist(nodeData(g,attr="label"))
			attrs <- list(node=list(shape="rectangle", fixedsize=FALSE,fontsize=30))
			plot(g,nodeAttrs=nAttrs,attrs=attrs)
			
})
#setMethod("plot",signature("GatingHierarchy","missing"),function(x,y,layout="dot",width=3,height=2,fontsize=14,labelfontsize=14,fixedsize=FALSE,boolean=FALSE,...){
#			if(!boolean){	
#				sub<-subGraph(x@nodes[which(!unlist(lapply(nodeData(x@tree,x@nodes,"metadata"),function(x)get("isBooleanGate",envir=x)),use.names=FALSE))],x@tree)
#			}else{
#				sub<-x@tree
#			}
#			nn<-sapply(nodes(sub),function(x)strsplit(x,"\\.")[[1]][2])
#			nn[1]<-nodes(sub)[1]
#			natr<-list();
#			natr$label<-nn;
#			options("warn"=-1)
#			lay<-Rgraphviz::layoutGraph(sub,layoutType=layout,nodeAttrs=natr,attrs=list(graph=list(rankdir="LR",page=c(8.5,11)),node=list(fixedsize=FALSE,fontsize=fontsize,shape="rectangle")))
#			renderGraph(lay)
#			#plot(sub,nodeAttrs=natr,attrs=list(node=list(fixedsize=fixedsize,labelfontsize=labelfontsize,fontsize=fontsize,width=width,height=height,shape="rectangle")),y=layout,...);
#			options("warn"=0)
#		})

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


setMethod("getSample","GatingHierarchyInternal",function(x,isFullPath=FALSE){
#			ifelse(isFullPath,file.path(x@dataPath,x@name),x@name)
			.Call("R_getSample",x@pointer)
			
		})
setMethod("getNodes","GatingHierarchyInternal",function(x,tsort=FALSE,...){
			.Call("R_getNodes",x@pointer)
			
#			if(!tsort){
#				return(x@nodes)
#			}else{
#				return(RBGL::tsort(x@tree))
#			}
		})
setMethod("getParent",signature(obj="GatingHierarchyInternal",y="numeric"),function(obj,y,tsort=FALSE){
#			return(match(getParent(obj,getNodes(obj,tsort=tsort)[y]),getNodes(obj,tsort=tsort)));
			
			ind<-.Call("R_getParent",obj@pointer,as.integer(y))
			getNodes(obj)[ind]
			
		})
setMethod("getParent",signature(obj="GatingHierarchyInternal",y="character"),function(obj,y){
#			browser()
			ind<-which(getNodes(obj)%in%y)
			getParent(obj,ind)
		})
#setMethod("getParent",signature(obj="GatingHierarchyInternal",y="character"),function(obj,y){
##			setdiff(adj(ugraph(obj@tree),y)[[1]],adj(obj@tree,y)[[1]])
#			.Call("R_getParentS",x@pointer,y)
#			
#		})
#setMethod("getChildren",signature(obj="GatingHierarchyInternal",y="character"),function(obj,y){
#			adj(obj@tree,y)[[1]]
#		})
#
#setMethod("getProp",signature(x="GatingHierarchyInternal",y="character"),function(x,y){
#			#Return the proportion of the population relative to the parent and relative to the total.
#			#x is a graph of a gating hierarchy that has had data added to it.
#			#y is nodename
#			return(get("thisTot",envir=nodeData(x@tree,y,"metadata")[[1]])/get("parentTot",envir=nodeData(x@tree,y,"metadata")[[1]]))	
#		})
#setMethod("getTotal",signature(x="GatingHierarchyInternal",y="character"),function(x,y){
#			return(get("thisTot",envir=nodeData(x@tree,y,"metadata")[[1]]))
#		})
#
#setMethod("getGate",signature(obj="GatingHierarchyInternal",y="character"),function(obj,y){
#			if(.isBooleanGate.graphNEL(obj,y)){
#				g<-get("gate",envir=nodeData(obj@tree,y,"metadata")[[1]])[[1]];
#				return(g)
#			}else{
#				return(get("gate",envir=nodeData(obj@tree,y,"metadata")[[1]]))
#			}
#		})
##return gate y for a given hierarchy (by index)
#setMethod("getGate",signature(obj="GatingHierarchyInternal",y="numeric"),function(obj,y,tsort=FALSE){
#			n<-getNodes(obj,tsort=tsort)[y]
#			if(flowWorkspace:::.isBooleanGate.graphNEL(obj,n)){
#				g<-get("gate",envir=nodeData(obj@tree,n,"metadata")[[1]])[[1]]
#				return(g);
#			}else{
#				return(get("gate",envir=nodeData(obj@tree,n,"metadata")[[1]]))
#			}
#		})
