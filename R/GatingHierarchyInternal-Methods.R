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
			.Call("R_getNodes",x@pointer,tsort)
			
#			if(!tsort){
#				return(x@nodes)
#			}else{
#				return(RBGL::tsort(x@tree))
#			}
		})
setMethod("getParent",signature(obj="GatingHierarchyInternal",y="numeric"),function(obj,y,tsort=FALSE){
#			return(match(getParent(obj,getNodes(obj,tsort=tsort)[y]),getNodes(obj,tsort=tsort)));
			#make sure the index conversion is done properly between C and R convention
			ind<-.Call("R_getParent",obj@pointer,as.integer(y)-1)+1
			getNodes(obj)[ind]
			
		})
setMethod("getParent",signature(obj="GatingHierarchyInternal",y="character"),function(obj,y){
#			browser()
			ind<-which(getNodes(obj)%in%y)
			getParent(obj,ind)
		})
setMethod("getChildren",signature(obj="GatingHierarchyInternal",y="character"),function(obj,y){
			nodes<-getNodes(obj)
#			browser()
			ind<-which(nodes%in%y)
			ind<-.Call("R_getChildren",obj@pointer,as.integer(ind)-1)+1
			nodes[ind]
		})
#
setMethod("getProp",signature(x="GatingHierarchyInternal",y="character"),function(x,y){
			#Return the proportion of the population relative to the parent and relative to the total.
			#x is a graph of a gating hierarchy that has had data added to it.
			#y is nodename
#			return(get("thisTot",envir=nodeData(x@tree,y,"metadata")[[1]])/get("parentTot",envir=nodeData(x@tree,y,"metadata")[[1]]))	
		})
#setMethod("getTotal",signature(x="GatingHierarchyInternal",y="character"),function(x,y){
#			return(get("thisTot",envir=nodeData(x@tree,y,"metadata")[[1]]))
#		})
#

.getPopStat<-function(x,y){
	stopifnot(!missing(y))
	nodes<-getNodes(x)
	if(is.character(y))
	{
		y<-which(nodes%in%y)
		
	}
	
	stats<-.Call("R_getPopStats",x@pointer,as.integer(y)-1)
	pInd<-.Call("R_getParent",x@pointer,as.integer(y)-1)+1
	pstats<-.Call("R_getPopStats",x@pointer,as.integer(pInd)-1)
	
	
	list(flowCore=c(proportion=unname(stats$FlowCore/pstats$FlowCore)
					,stats$FlowCore["count"])
			,flowJo=c(proportion=unname(stats$FlowJo/pstats$FlowJo)
					,stats$FlowJo["count"])	
	)
}
setMethod("getPopStats","GatingHierarchyInternal",function(x,...){

			
#			stopifnot(!missing(y)&&(is.numeric(y)||is.integer(y)))
			nNodes<-length(getNodes(x))
			browser()
			lapply(1:nNodes,function(ind){
						print(ind)		
						.getPopStat(x,ind)
					})
			browser()
			stats<-.Call("R_getPopStats",x@pointer,as.integer(y))
			
			
			pInd<-.Call("R_getParent",x@pointer,as.integer(y))
			pstats<-.Call("R_getPopStats",x@pointer,as.integer(pInd))
			
			
			list(flowCore=c(proportion=unname(stats$FlowCore/pstats$FlowCore)
							,stats$FlowCore["count"])
				,flowJo=c(proportion=unname(stats$FlowJo/pstats$FlowJo)
							,stats$FlowJo["count"])	
				)
			###Fix for root node. Should be fixed in addDataToGatingHierarchy

#			rownames(m)<-NULL;
			
#			m<-data.frame(m);
#			m[,2]<-as.numeric(as.character(m[,2]));
#			m[,3]<-as.numeric(as.character(m[,3]));
#			m[,4]<-as.numeric(as.character(m[,4]));
#			m[,5]<-as.numeric(as.character(m[,5]))
#			m[,6]<-as.character(m[,6])
#			
#			#m[1,4]<-m[1,3]
#			m[1,c(2)]<-1;
#			m[1,5]<-m[1,4]
#			colnames(m)<-c("pop.name","flowCore.freq","flowJo.count","flowCore.count","parent.total","node")
#			rownames(m)<-m[,1]
#			m<-m[,2:6]
#			m
		})

#setMethod("getPopStats","GatingSet",function(x,...){
#			if(!haveSameGatingHierarchy(x)){
#				message("Can't retrieve population statistics table for GatingSet. The samples don't all have the same gating schemes.")
#			}
#			r<-do.call(cbind,lapply(x,function(y)getPopStats(y)$flowCore.freq))
#			rownames(r)<-rownames(getPopStats(x[[1]]));
#			r
#		})
#
#setMethod("plotPopCV","GatingHierarchy",function(x,m=2,n=2,...){
#			x<-getPopStats(x)
#			cv<-apply(as.matrix(x[,2:3]),1,function(y)IQR(y)/median(y));
#			cv<-as.matrix(cv,nrow=length(cv))
#			cv[is.nan(cv)]<-0
#			rownames(cv)<-basename(as.character(rownames(x)));
#			return(barchart(cv,xlab="Coefficient of Variation",...));
#		})
#
#setMethod("plotPopCV","GatingSet",function(x,...){
##columns are populations
##rows are samples
#			cv<-do.call(rbind,lapply(lapply(x,getPopStats),function(x)apply(x[,2:3],1,function(x){cv<-IQR(x)/median(x);ifelse(is.nan(cv),0,cv)})))
#			rownames(cv)<-getSamples(x);#Name the rows
##flatten, generate levels for samples.
#			nr<-nrow(cv)
#			nc<-ncol(cv)
#			populations<-gl(nc,nr,labels=basename(as.character(colnames(cv))))
#			samples<-as.vector(t(matrix(gl(nr,nc,labels=basename(as.character(rownames(cv)))),nrow=nc)))
#			cv<-data.frame(cv=as.vector(cv),samples=samples,populations=populations)
#			return(barchart(cv~populations|samples,cv,...,scale=list(x=list(...))));
#		})
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
