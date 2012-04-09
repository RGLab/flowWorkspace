# TODO: Add comment
# 
# Author: wjiang2
###############################################################################


setMethod("plot",signature("GatingHierarchyInternal","missing"),function(x,y,layout="dot",width=3,height=2,fontsize=14,labelfontsize=14,fixedsize=FALSE,boolean=FALSE,...){
			
#			browser()
			DotFile<-tempfile()
			.Call("R_plotGh",x@pointer,getSample(x),DotFile)
			GXLFile<-tempfile()
			system(paste("dot2gxl",DotFile, "-o",GXLFile))
			
			sf<-file(GXLFile)
			g<-fromGXL(sf)
			close(sf)
			
			rm(DotFile)
			rm(GXLFile)
			nAttrs <- list()
			nAttrs$label<-unlist(nodeData(g,attr="label"))
#			browser()
#			attrs <- list(node=list(shape="rectangle"
#									, fixedsize=F
##									,fontsize=10
##									,height=1
#									,fillcolor="lightgreen")
#						)
#			plot(g,nodeAttrs=nAttrs,attrs=attrs)
			
#			browser()
			options("warn"=-1)
			lay<-Rgraphviz::layoutGraph(g,layoutType=layout,nodeAttrs=nAttrs
										,attrs=list(graph=list(rankdir="LR",page=c(8.5,11))
													,node=list(fixedsize=FALSE
																,fontsize=fontsize
																,shape="rectangle"
																)
													)
										)
			renderGraph(lay)
			#plot(sub,nodeAttrs=natr,attrs=list(node=list(fixedsize=fixedsize,labelfontsize=labelfontsize,fontsize=fontsize,width=width,height=height,shape="rectangle")),y=layout,...);
			options("warn"=0)
			
			
})

setMethod("show",signature("GatingHierarchyInternal"),function(object){
			cat("\tFCS File: ",getSample(object),"\n");
			cat("\tGatingHierarchy with ",length(getNodes(object))," gates\n");
			print(object@pointer)
			cat("\n")
		})


#setMethod("getSample","GatingHierarchyInternal",function(x,isFullPath=FALSE){
##			ifelse(isFullPath,file.path(x@dataPath,x@name),x@name)
#			.Call("R_getSample",x@pointer)
#			
#		})
setMethod("getNodes","GatingHierarchyInternal",function(x,tsort=FALSE,isPath=FALSE,...){
			nodes<-.Call("R_getNodes",x@pointer,getSample(x),tsort,isPath)
			if(!isPath)
				nodes<-c(nodes[1],paste(2:length(nodes),nodes[-1],sep="."))
			nodes
		})
setMethod("getParent",signature(obj="GatingHierarchyInternal",y="numeric"),function(obj,y,tsort=FALSE){
#			return(match(getParent(obj,getNodes(obj,tsort=tsort)[y]),getNodes(obj,tsort=tsort)));
			#make sure the index conversion is done properly between C and R convention
#			browser()
			.Call("R_getParent",obj@pointer,getSample(obj),as.integer(y)-1)+1
		})
setMethod("getParent",signature(obj="GatingHierarchyInternal",y="character"),function(obj,y){
#			browser()
			ind<-which(getNodes(obj)%in%y)
			pind<-getParent(obj,ind)
			getNodes(obj)[pind]
		})
setMethod("getChildren",signature(obj="GatingHierarchyInternal",y="character"),function(obj,y){
			nodes<-getNodes(obj)
#			browser()
			ind<-which(nodes%in%y)
			ind<-.Call("R_getChildren",obj@pointer,getSample(obj),as.integer(ind)-1)+1
			nodes[ind]
		})
#
setMethod("getProp",signature(x="GatingHierarchyInternal",y="character"),function(x,y){
			#Return the proportion of the population relative to the parent and relative to the total.
			#y is nodename
			
			ind<-which(getNodes(x)%in%y)
			stats<-.getPopStat(x,ind)$flowJo
#			browser()
			unname(stats["count"]/stats["parent.total"])	
		})
setMethod("getTotal",signature(x="GatingHierarchyInternal",y="character"),function(x,y){
			ind<-which(getNodes(x)%in%y)
			stats<-.getPopStat(x,ind)$flowJo
#			browser()
			unname(stats["count"])
		})


.getPopStat<-function(x,y){
	stopifnot(!missing(y))
	nodes<-getNodes(x)
	if(is.character(y))
	{
		y<-which(nodes%in%y)
		
	}

	stats<-.Call("R_getPopStats",x@pointer,getSample(x),as.integer(y)-1)
	pInd<-.Call("R_getParent",x@pointer,getSample(x),as.integer(y)-1)+1
	if(length(pInd)>0)
		pstats<-.Call("R_getPopStats",x@pointer,getSample(x),as.integer(pInd)-1)
	else
		pstats<-stats#list(FlowCore=c(count=0,proportion=0),FlowJo=c(count=0,proportion=0))
	
#	browser()	
#	list(flowCore=c(proportion=as.numeric(ifelse(pstats$FlowCore["count"]==0
#										,1
#										,stats$FlowCore["count"]/pstats$FlowCore["count"]
#										)
#					,count=as.numeric(stats$FlowCore["count"])))
#		,flowJo=c(proportion=as.numeric(ifelse(pstats$FlowJo["count"]==0
#										,1
#										,stats$FlowJo["count"]/pstats$FlowJo["count"]
#										))
#					,count=as.numeric(stats$FlowJo["count"]))	
#		)
	list(flowCore=c(parent.total=as.numeric(pstats$FlowCore["count"]),count=as.numeric(stats$FlowCore["count"]))
		,flowJo=c(parent.total=as.numeric(pstats$FlowJo["count"]),count=as.numeric(stats$FlowJo["count"])
		)	
	)
}
setMethod("getPopStats","GatingHierarchyInternal",function(x,...){

			
#			stopifnot(!missing(y)&&(is.numeric(y)||is.integer(y)))
			nodeNamesPath<-getNodes(x,isPath=T)
			nodeNames<-getNodes(x)
			nNodes<-length(nodeNames)

			stats<-lapply(1:nNodes,function(ind){
						
						curStats<-.getPopStat(x,ind)
#						browser()
						curRes<-c(nodeNamesPath[ind]
									,curStats$flowCore["parent.total"]
									,curStats$flowJo["count"]
									,curStats$flowCore["count"]
									,curStats$flowJo["parent.total"]
									,nodeNames[ind]
									)
						
						curRes			
						})
#			browser()
			m<-do.call("rbind",stats)
			
			
			###Fix for root node. Should be fixed in addDataToGatingHierarchy

			rownames(m)<-NULL;
			
			m<-data.frame(m);
			m[,2]<-as.numeric(as.character(m[,2]));
			m[,3]<-as.numeric(as.character(m[,3]));
			m[,4]<-as.numeric(as.character(m[,4]));
			m[,5]<-as.numeric(as.character(m[,5]))
			m[,6]<-as.character(m[,6])


			m[1,c(2)]<-1;
#			m[1,5]<-m[1,4]
			colnames(m)<-c("pop.name","flowCore.freq","flowJo.count","flowCore.count","parent.total","node")
			rownames(m)<-m[,1]
			m<-m[,2:6]
			m
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
