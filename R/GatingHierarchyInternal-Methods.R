
#20110314
#TODO wrap isNcdf slot with get/set methods
setGeneric("isNcdf", function(x){standardGeneric("isNcdf")})
setMethod("isNcdf",c("GatingHierarchyInternal"),function(x){
#			browser()
			fs<-x@tree@nodeData@defaults$data$data$ncfs
			return (class(fs)=="ncdfFlowSet")
			
		})
setMethod("isNcdf",c("GatingHierarchy"),function(x){
			return(x@isNcdf)
		})

setMethod("plot",c("GatingHierarchyInternal","missing"),function(x,y,layout="dot",width=3,height=2,fontsize=14,labelfontsize=14,fixedsize=FALSE,boolean=FALSE,...){
			
#			browser()
			DotFile<-tempfile()
			.Call("R_plotGh",x@pointer,getSample(x),DotFile,FALSE)
			GXLFile<-tempfile()
			system(paste("dot2gxl",DotFile, "-o",GXLFile))
			
			sf<-file(GXLFile)
			g<-fromGXL(sf)
			close(sf)
			
			rm(DotFile)
			rm(GXLFile)
			
#			browser()
			##remove bool gates if necessary
			if(!boolean)
			{
				nodes<-nodeData(g,attr="isBool")
				for(i in 1:length(nodes))
				{
					if(as.logical(as.integer(nodes[[i]])))
						g <- removeNode(names(nodes[i]), g)
				}
			}
			nAttrs <- list()
			nAttrs$label<-unlist(nodeData(g,attr="label"))
			
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

setMethod("show","GatingHierarchyInternal",function(object){
			cat("\tFCS File: ",getSample(object),"\n");
			cat("\tGatingHierarchy with ",length(getNodes(object))," gates\n");
			print(object@pointer)
			cat("\n")
		})

#Return the value of the keyword given a flowWorkspace and the keyword name
setMethod("keyword",c("GatingHierarchyInternal","character"),function(object,keyword){
			
			keyword(getData(object),keyword)
		})
setMethod("getKeywords",c("GatingHierarchyInternal","missing"),function(obj,y){
			keyword(getData(obj))
		})
#Note:integer indices of nodes are based on regular order
#so whenver need to map from character node name to integer node ID,make sure
#to use default order which is regular.
setMethod("getNodes","GatingHierarchyInternal",function(x,y=NULL,order="regular",isPath=FALSE,prefix=FALSE,...){

			orderInd<-match(order,c("regular","tsort","bfs"))
			if(length(orderInd)==0)
				orderInd<-0
			else
				orderInd<-orderInd-1
			
			nodeNames<-.Call("R_getNodes",x@pointer,getSample(x),as.integer(orderInd),isPath)

			#try to remove ID prefix from node name without causing name duplication
			if(!prefix)
			{
				dotPos<-regexpr("\\.",nodeNames)
				#get unique IDs for each node
				NodeIDs<-as.integer(substr(nodeNames,0,dotPos-1))
				#strip IDs from nodeNames
				nodeNames<-substr(nodeNames,dotPos+1,nchar(nodeNames))
				#add ID only when there is conflicts in nodeNames
				toAppendIDs<-duplicated(nodeNames)
				nodeNames[toAppendIDs]<-paste(NodeIDs[toAppendIDs],nodeNames[toAppendIDs],sep=".")	
			}
			
			
			if(!is.null(y))
			{
				if(is.character(y))
					ind<-match(y,nodeNames)
				else
					ind<-y
				if(is.na(ind))
					stop("Node:", y," not found!")
				
				res<-nodeNames[ind]
				if(is.na(res)||length(res)==0)
					stop("Node:", y," not found!")
				res

			}else
					nodeNames
		})

setMethod("getParent",signature(obj="GatingHierarchyInternal",y="numeric"),function(obj,y){
#			return(match(getParent(obj,getNodes(obj,tsort=tsort)[y]),getNodes(obj,tsort=tsort)));
			#make sure the index conversion is done properly between C and R convention
#			browser()
			.Call("R_getParent",obj@pointer,getSample(obj),as.integer(y)-1)+1
		})
setMethod("getParent",signature(obj="GatingHierarchyInternal",y="character"),function(obj,y){
#			browser()
			ind<-match(y,getNodes(obj))
			if(is.na(ind)||length(ind)==0)
				stop("Node:", y," not found!")
			pind<-getParent(obj,ind)
			getNodes(obj)[pind]
		})
setMethod("getChildren",signature(obj="GatingHierarchyInternal",y="character"),function(obj,y,tsort=FALSE){
			ind<-match(y,getNodes(obj))
			if(is.na(ind)||length(ind)==0)
				stop("Node:", y," not found!")
			cind<-getChildren(obj,ind)
			getNodes(obj)[cind]
})
setMethod("getChildren",signature(obj="GatingHierarchyInternal",y="numeric"),function(obj,y){
#			browser()
			.Call("R_getChildren",obj@pointer,getSample(obj),as.integer(y)-1)+1
			
		})
#
setMethod("getProp",signature(x="GatingHierarchyInternal",y="character"),function(x,y,flowJo=TRUE){
			#Return the proportion of the population relative to the parent and relative to the total.
			#y is nodename
			
			ind<-match(y,getNodes(x))
			if(is.na(ind)||length(ind)==0)
				stop("Node:", y," not found!")
			stats<-.getPopStat(x,ind)
			if(flowJo)
				unname(stats$flowJo["proportion"])
			else
				unname(stats$flowCore["proportion"])
#			browser()
#			unname(stats["count"]/stats["parent.total"])	
			
		})
setMethod("getTotal",signature(x="GatingHierarchyInternal",y="character"),function(x,y,flowJo=TRUE){
			ind<-match(y,getNodes(x))
			if(is.na(ind)||length(ind)==0)
				stop("Node:", y," not found!")
			stats<-.getPopStat(x,ind)
			if(flowJo)
				unname(stats$flowJo["count"])	
			else
				unname(stats$flowCore["count"])
#			browser()
			
		})


.getPopStat<-function(x,y){
	stopifnot(!missing(y))
	nodes<-getNodes(x)
	if(is.character(y))
	{
		y<-which(nodes%in%y)
		

	}
#	browser()
	stats<-.Call("R_getPopStats",x@pointer,getSample(x),as.integer(y)-1)
	
	pInd<-try(.Call("R_getParent", x@pointer, getSample(x), as.integer(y) -1),silent=T)
	
	
	if(class(pInd)=="try-error")#if parent exist
		pstats<-stats
	else
	{
		pInd<-pInd+1 #convert c convention to R
		pstats<-.Call("R_getPopStats",x@pointer,getSample(x),as.integer(pInd)-1)
	}
		
	
#	browser()	
	list(flowCore=c(proportion=as.numeric(ifelse(pstats$FlowCore["count"]==0
										,1
										,stats$FlowCore["count"]/pstats$FlowCore["count"]
										))
					,count=as.numeric(stats$FlowCore["count"]))
		,flowJo=c(proportion=as.numeric(ifelse(pstats$FlowJo["count"]==0
										,1
										,stats$FlowJo["count"]/pstats$FlowJo["count"]
										))
					,count=as.numeric(stats$FlowJo["count"]))	
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
									,curStats$flowCore["proportion"]
									,curStats$flowJo["count"]
									,curStats$flowCore["count"]
									,curStats$flowJo["proportion"]
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
#			colnames(m)<-c("pop.name","flowCore.freq","flowJo.count","flowCore.count","parent.total","node")
			colnames(m)<-c("pop.name","flowCore.freq","flowJo.count","flowCore.count","flowJo.freq","node")
			rownames(m)<-m[,1]
			m<-m[,2:6]
			m
		})

		
setMethod("getGate",signature(obj="GatingHierarchyInternal",y="character"),function(obj,y){
			
#			browser()
			ind<-match(y,getNodes(obj))
			if(is.na(ind)||length(ind)==0)
				stop("Node:", y," not found!")
			g<-getGate(obj,ind)
			g
			
		})
#return gate y for a given hierarchy (by index)
#Note that this index is ordered by regular sorting method
setMethod("getGate",signature(obj="GatingHierarchyInternal",y="numeric"),function(obj,y,tsort=FALSE){
			vertexID=y-1
			if(vertexID<=0)
				return (NA)
			else
			{
#				browser()
				g<-.Call("R_getGate",obj@pointer,getSample(obj),vertexID)
				filterId<-getNodes(obj)[y]
				if(g$type==1)
				{
					
					
					mat<-matrix(c(g$x,g$y),ncol=2,dimnames=list(NULL,g$parameters))
					if(nrow(mat)==2)#convert to rectangleGate
						rectangleGate(.gate=mat,filterId=filterId)
					else
						polygonGate(.gate=mat,filterId=filterId)
				}else if(g$type==2)
					rectangleGate(.gate=matrix(g$range,dimnames=list(NULL,g$parameters)),filterId=filterId)
				else if(g$type==3)
				{
					nodeNames<-getNodes(obj)
					nodePaths<-getNodes(obj,isPath=T)
					refPaths<-unlist(lapply(g$ref,function(curPath){
										paste(curPath,collapse="/")
										
									})
								)
					fullPaths<-paste("/",refPaths,sep="")
					nodes<-nodeNames[match(fullPaths,nodePaths)]
					names(nodes)<-refPaths
					g$ref<-nodes
					attr(g,"class")<-"BooleanGate"	
					g
				}else
					stop("not supported gate type",g$type)
				
			}
		})

setMethod("getIndices",signature(obj="GatingHierarchyInternal",y="character"),function(obj,y){
			ind<-match(y,getNodes(obj))
			if(is.na(ind)||length(ind)==0)
				stop("Node:", y," not found!")
#			browser()
			getIndices(obj,ind)
			
		})
setMethod("getIndices",signature(obj="GatingHierarchyInternal",y="numeric"),function(obj,y){
			

			.Call("R_getIndices",obj@pointer,getSample(obj),as.integer(y-1))
			
		})	
#tsort argument is not used (for the compatibility with old API)
#once the R paser is deprecated,it can be safely removed as well
setMethod("getData",signature(obj="GatingHierarchyInternal"),function(obj,y=NULL,tsort=FALSE){
			if(!obj@flag){
				stop("Must run execute() before fetching data");
			}

			r<-nodeDataDefaults(obj@tree,"data")$data$ncfs[[getSample(obj)]]
					
			if(is.null(y)||y==1||getNodes(obj)[1]==y){
				return (r)	
			}else
				return (r[getIndices(obj,y),])
			
			
		})
.isBoolGate<-function(x,y){
	return (class(getGate(x,y))=="BooleanGate")
}
#setMethod("getDimensions",signature(obj="GatingHierarchyInternal",y="character"),function(obj,y,index=FALSE){
##			browser()
#			if(.isBoolGate(obj,y)){
#				getDimensions(obj,getParent(obj,y),index=index);
#			}else{
#				if(!index){
#					if(length(getGate(obj,y)@parameters)==1){
#						c(getGate(obj,y)@parameters[[1]]@parameters);
#					}else{
#						c(getGate(obj,y)@parameters[[1]]@parameters,getGate(obj,y)@parameters[[2]]@parameters)
#					}
#				}else{
#					if(length(getGate(obj,y)@parameters)==1){
#						if(.isCompensated(obj)){
#							tmp<-parameters(getData(obj,y))@data$name
#						}else{
#							tmp<-gsub(">","",gsub("<","",parameters(getData(obj,y))@data$name))
#						}
#						c(match(getGate(obj,y)@parameters[[1]]@parameters,tmp))	
#					}else{
#						if(.isCompensated(obj)){
#							tmp<-parameters(getData(obj,y))@data$name
#						}else{
#							tmp<-gsub(">","",gsub("<","",parameters(getData(obj,y))@data$name))
#						}
#						c(match(getGate(obj,y)@parameters[[1]]@parameters,tmp),match(getGate(obj,y)@parameters[[2]]@parameters,tmp))
#					}
#				}
#			}
#		})

setMethod("getAxisLabels",signature(obj="GatingHierarchyInternal",y="missing"),function(obj,y=NULL,...){
			if(exists("axis.labels",envir=nodeDataDefaults(obj@tree)$data))
				return (get("axis.labels",envir=nodeDataDefaults(obj@tree)$data))
			else
				return (NULL)
		})

#this method currently is used convert transformation funtion from c++ to R
##mainly for transforming range info
setMethod("getTransformations","GatingHierarchy",function(x){
			x@transformations
		})
setMethod("getTransformations","GatingHierarchyInternal",function(x){
#			browser()
			trans<-.Call("R_getTransformations",x@pointer,getSample(x))
			lapply(trans,function(curTrans){
#						browser()
						if(curTrans$type=="log")
						{
							f<-function(x){x<-log(x,10);x[is.nan(x)]<-0;x[is.infinite(x)]<-0;x}
							attr(f,"type")<-"log"
							
						}
						else if(curTrans$type=="lin")
						{
							f<-function(x){x*64}
							attr(f,"type")<-"gateOnly"
							
							
						}else 
						{
							f<-function (x, deriv = 0) 
							{
							    deriv <- as.integer(deriv)
							    if (deriv < 0 || deriv > 3) 
							        stop("'deriv' must be between 0 and 3")
							    if (deriv > 0) {
							        z0 <- double(z$n)
							        z[c("y", "b", "c")] <- switch(deriv, list(y = z$b, b = 2 * 
							            z$c, c = 3 * z$d), list(y = 2 * z$c, b = 6 * z$d, 
							            c = z0), list(y = 6 * z$d, b = z0, c = z0))
							        z[["d"]] <- z0
							    }
							    res <- .C(C_SplineEval, z$method, as.integer(length(x)), 
							        x = as.double(x), y = double(length(x)), z$n, z$x, z$y, 
							        z$b, z$c, z$d, PACKAGE = "stats")$y
							    if (deriv > 0 && z$method == 2 && any(ind <- x <= z$x[1L])) 
							        res[ind] <- ifelse(deriv == 1, z$y[1L], 0)
							    res
							}
							z<-curTrans$z
							z$n<-length(z$x)
							z$method<-curTrans$method
							assign("z",z,environment(f))
							assign("C_spline_eval",environment(splinefun)$C_spline_eval,environment(f))
							
							attr(f,"type")<-curTrans$type	
						}
						
						return (f)
					})
			
		})
		
##it is currently only for internal use		
setMethod("getCompensationMatrices","GatingHierarchyInternal",function(x){
			comp<-.Call("R_getCompensation",x@pointer,getSample(x))
			cid<-comp$cid
#			browser()
			if(cid=="")
				cid=-2
			if(cid!="-1" && cid!="-2"){
				marker<-comp$parameters
				compobj<-compensation(matrix(comp$spillOver,nrow=length(marker),ncol=length(marker),byrow=TRUE,dimnames=list(marker,marker)))
			}else if(cid=="-2"){
				#TODO the matrix may be acquisition defined.
#				message("No compensation");
				compobj<-NULL
			}
			else if(cid=="-1")
			{
				##Acquisition defined compensation.
				nm<-comp$comment
				
				if(grepl("Acquisition-defined",nm)){
					###Code to compensate the sample using the acquisition defined compensation matrices.
#					message("Compensating with Acquisition defined compensation matrix");
					#browser()
					compobj<-compensation(spillover(getData(x))$SPILL)
						
				}
				
			}
			compobj
			
})
.getChannelMarker<-function(pd,name=NULL)
{
	#try stain name
	
#	browser()
	if(is.null(name))
		return(NULL)
	ind<-which(toupper(pd$name)%in%toupper(name))
	
	
	
	
	if(length(ind)==0)
	{
		#try marker name
		ind<-which(unlist(lapply(pd$des,function(x){
									#split by white space and then match each individual string
									any(unlist(lapply(strsplit(x," "),function(y)toupper(y)%in%toupper(name))))
								})
				)
		)
		if(length(ind)==0)
			stop("can't find ",name)
		if(length(ind)>1)
			stop("multiple markers matched: ",name)
	}
	
	pd[ind,c("name","desc")]
}


#TODO: to inverse transform the range in order to display the raw scale
setMethod("plotGate",signature(x="GatingHierarchy",y="character"),function(x,y,...){
			
			ind<-match(y,getNodes(x))
			if(is.na(ind)||length(ind)==0)
				stop("Node:", y," not found!")
			plotGate(x,ind,...)
			
})
setMethod("plotGate",signature(x="GatingHierarchy",y="missing"),function(x,y,...){
#			browser()
		y<-2:length(getNodes(x))
		
		plotGate(x,y,...)
		})
setMethod("plotGate",signature(x="GatingHierarchy",y="numeric"),function(x,y,bool=FALSE,main=getSample(x),arrange=TRUE,merge=TRUE,...){
			if(!x@flag){
				message("Can't plot until you gate the data with 'execute()'\n");
				return();
			}
			
			
#			browser()
			plotList<-.mergeGates(x,y,bool,merge)
			plotObjs<-lapply(plotList,function(y){
						
						return(.plotGate(x,y,...))
					})
			
			if(arrange)			
				do.call(grid.arrange,c(plotObjs,main=main))
			else
				plotObjs
			
})
.mergeGates<-function(gh,i,bool,merge){
	##filter out boolean gates when bool==FALSE
#	browser()
	if(!bool)
	{
		
		boolInd<-unlist(lapply(i,.isBoolGate,x=gh))
#		browser()
		if(length(which(boolInd))>0)
			message("skipping boolean gates!")
		i<-i[!boolInd]
	}
	
	plotList<-poplist<-as.list(i)
	names(plotList)<-plotList
	if(length(plotList)==0)
		stop("not gates to plot")
	if(merge)
	{
		#check if they have same parents and parameters
		keylist<-sapply(plotList,function(y){
					
					if(!.isBoolGate(gh,y))
					{
						curGate<-getGate(gh,y)
						#							browser()
						if(extends(class(curGate),"filter"))
						{
							pid<-getParent(gh,y)
							params<-paste(sort(unname(parameters(curGate))),collapse="")
							
							paste(pid,params,sep="|")
						}else
							return(-1)
						
					}else
						return(-2)
				})
		
		invalidNodes<-sapply(keylist,function(key)key==-1)
		poplist<-poplist[!invalidNodes]
		plotList<-plotList[!invalidNodes]
		keylist<-keylist[!invalidNodes]
		
		boolNodes<-sapply(keylist,function(key)key==-2)
		keylist<-keylist[!boolNodes]
		
		#			browser()
		keylistFeq<-table(keylist)
		toMergeKeyList<-names(keylistFeq[keylistFeq>=2])
		# construct the a special list object to replace/del the one that needs to be merged
		for(curKey in toMergeKeyList)
		{
			#				browser()
			toMerge<-as.numeric(names(keylist[keylist==curKey]))
			toReplace<-sort(toMerge)[1]#replace the first merged child node with the merge list
			toRemove<-toMerge[!(toMerge==toReplace)]#remove other children
			
			toReplaceInd<-match(toReplace,poplist)
			toRemoveInd<-match(toRemove,poplist)
			#								browser()
			
			curPid<-as.numeric(strsplit(curKey,split="\\|")[[1]][1])#extract pid
			plotList[[toReplaceInd]]<-list(popIds=toMerge,parentId=curPid)
			plotList[toRemoveInd]<-NULL
			poplist[toRemoveInd]<-NULL#make sure syn y as well vector since it is used to index plotList 
		}
		
	}
	plotList
}
#copy from sfsmisc package
pretty10exp<-function (x, drop.1 = FALSE, digits.fuzz = 7) 
{
	eT <- floor(log10(abs(x)) + 10^-digits.fuzz)
	mT <- signif(x/10^eT, digits.fuzz)
	ss <- vector("list", length(x))
	for (i in seq(along = x)) ss[[i]] <- if (x[i] == 0) 
					quote(0)
				else if (drop.1 && mT[i] == 1) 
					substitute(10^E, list(E = eT[i]))
				else if (drop.1 && mT[i] == -1) 
					substitute(-10^E, list(E = eT[i]))
				else substitute(A %*% 10^E, list(A = mT[i], E = eT[i]))
	do.call("expression", ss)
}

#fitGate is used to disable behavior of plotting the gate region in 1d densityplot
#overlay is either the gate indices or logical vector(i.e. event indices)
.plotGate<-function(x,y,main=NULL,margin=FALSE,smooth=FALSE,xlab=NULL,ylab=NULL,xlim=NULL,ylim=NULL,stat=TRUE,fitGate=FALSE,type=c("xyplot","densityplot"),overlay=NULL,...){			
		
			type<- match.arg(type)
			if(is.list(y))
				pid<-y$parentId
			else
				pid<-getParent(x,y)
		
			if(is.null(main)){
				fjName<-getNodes(x,pid,isPath=T)
				main<-fjName
			}
			
			if(is.list(y))
			{
				curGate<-filters(lapply(y$popIds,function(y)getGate(x,y)))	
			}else
			{
				curGate<-getGate(x,y)
	
				if(suppressWarnings(is.na(curGate))){
					message("Can't plot. There is no gate defined for node ",getNodes(x,y));
					invisible();			
					return(NULL)
				}

			}			
			
			parentdata<-getData(x,pid)
#			browser()
			smooth<-ifelse(nrow(parentdata)<100,TRUE,smooth)
			
			
			
			#################################
			# setup axis labels and scales
			################################
			if(class(curGate)=="BooleanGate")
			{

				if(!is.null(overlay))
					stop("no overlay is supported for booleangate!In order to visualize multiple gates,try to add a new booleanGate first.")
				params<-rev(parameters(getGate(x,getParent(x,y))))
				overlay<-getIndices(x,y)
				curGate<-NULL									
				
#				panelFunc<-panel.xyplot.flowFrame.booleanGate
			}else
			{
				if(class(curGate)=="filters")
					params<-rev(parameters(curGate[[1]]))
				else
					params<-rev(parameters(curGate))
				
			}
			panelFunc<-panel.xyplot.flowframe
			
			
			
			if(type=="xyplot")
			{

				if(length(params)==1)
				{
					yParam<-"SSC-A"
					
					if(params=="SSC-A")
						xParam<-"FSC-A"
					else
						xParam<-params
					params<-c(yParam,xParam)
				}else
				{
					yParam=params[1]
					xParam=params[2]
				}
				axisObject<-.formatAxis(x,parentdata,xParam,yParam,...)
				
				#################################
				# calcuate overlay frame
				################################
				if(!is.null(overlay))
				{
					#gate indices
					if(class(overlay)=="numeric")
					{
						if(length(overlay)>1)
							stop("only one overlay gate can be added!In order to visualize multiple overlays,try to add a booleanGate first.")
						overlay<-getData(x,overlay)[,params]
					}else
						overlay<-Subset(getData(x),overlay)[,params]
				}
				
				#################################
				# the actual plotting
				################################
				
				f1<-mkformula(params)
				res<-xyplot(x=f1
							,data=parentdata[,params]
							,filter=curGate
							,xlab=axisObject$xlab
							,ylab=axisObject$ylab
							,margin=margin
							,smooth=smooth
							,scales=axisObject$scales
							,main=main
							,stat=stat
							,panel=panelFunc
							,overlay=overlay
							,...
						)
			}else
			{
				if(length(params)==1)
				{
#					browser()
					axisObject<-.formatAxis(x,parentdata,xParam=params,yParam=NULL,...)
					f1<-mkformula(params)
					res<-densityplot(x=f1
									,data=parentdata[,params]
									,filter=curGate
									,xlab=axisObject$xlab
		#							,ylab=axisObject$ylab
									,margin=margin
		#							,smooth=smooth
		#							,scales=axisObject$scales
									,main=main
									,stat=stat
									,fitGate=fitGate
		#							,panel=panelFunc
									,...
									)
				}else
					stop("Can't plot densityplot because there are two parameters associated with this gate!")
			}

		
			
		return(res)
		
				
			
			
}
##x is a gatingHierarchy, data is a flowFrame
.formatAxis<-function(x,data,xParam,yParam,scales=list(),...){
	pd<-pData(parameters(data))
#	browser()
	xObj<-.getChannelMarker(pd,xParam)
	yObj<-.getChannelMarker(pd,yParam)
	
	xlab<-sub("NA","",paste(unlist(xObj),collapse=" "))
	ylab<-sub("NA","",paste(unlist(yObj),collapse=" "))
#			browser()
	
		xParam.ind<-match(xParam,pd$name)
		yParam.ind<-match(yParam,pd$name)
		x.labels<-getAxisLabels(x)[[xParam.ind]]
		y.labels<-getAxisLabels(x)[[yParam.ind]]
		
		
		#init the scales and x,y lim
		
		xlim=range(data)[,xParam]
		ylim=range(data)[,yParam]
		
		#update axis when applicable
		if(!is.null(x.labels))
		{
			x.labels$label<-pretty10exp(as.numeric(x.labels$label),drop.1=TRUE)
			xscales<-list(x=list(at=x.labels$at,labels=x.labels$label))
			scales<-lattice:::updateList(xscales,scales)
			xlim=range(x.labels$at)
		}
		if(!is.null(y.labels))
		{	
			y.labels$label<-pretty10exp(as.numeric(y.labels$label),drop.1=TRUE)
			yscales<-list(y=list(at=y.labels$at,labels=y.labels$label))
			scales<-lattice:::updateList(scales,yscales)
			ylim=range(y.labels$at)
		}
		
	
	
	list(scales=scales,xlab=xlab,ylab=ylab)
}
panel.xyplot.flowFrame.booleanGate<-function(x,y
		,frames
		,filter=NULL#filter here is actually a subsetted flowFrame
		,channel.x.name
		,channel.y.name
		,binTrans=sqrt
		,xbins=0
		,...){
	
	

	x.b <- exprs(filter[,channel.x.name])
	y.b <- exprs(filter[,channel.y.name])
	
	if(xbins>0)
	{
		#using hexbin package to do the hexagon plot	
		bin<-hexbin(x,y,xbins=xbins)
	
		grid.hexagons(bin,trans=binTrans)		
#		plotType("gpoints", c(channel.x.name, channel.y.name))
	}else
	{
		panel.points(x,y,col="black",pch=".")
			
	}
#	browser()	
	panel.points(x.b,y.b,col="red",pch=".")
	
	
}
