
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



setMethod("getNodes","GatingHierarchyInternal",function(x,y=NULL,tsort=FALSE,isPath=FALSE,...){
#			browser()
			nodePaths<-.Call("R_getNodes",x@pointer,getSample(x),tsort,isPath)
			
			nodeNames<-c(nodePaths[1],paste(2:length(nodePaths),nodePaths[-1],sep="."))
				
			if(!is.null(y))
			{
				if(is.character(y))
					y<-match(y,nodeNames)
				ifelse(isPath,nodePaths[y],nodeNames[y])
			}else
				if(isPath)
					nodePaths
				else
					nodeNames
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
setMethod("getChildren",signature(obj="GatingHierarchyInternal",y="character"),function(obj,y,tsort=FALSE){
			ind<-which(getNodes(obj)%in%y)
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
			
			ind<-which(getNodes(x)%in%y)
			stats<-.getPopStat(x,ind)
			if(flowJo)
				unname(stats$flowJo["proportion"])
			else
				unname(stats$flowCore["proportion"])
#			browser()
#			unname(stats["count"]/stats["parent.total"])	
			
		})
setMethod("getTotal",signature(x="GatingHierarchyInternal",y="character"),function(x,y,flowJo=TRUE){
			ind<-which(getNodes(x)%in%y)
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
	stats<-.Call("R_getPopStats",x@pointer,getSample(x),as.integer(y)-1)
	pInd<-.Call("R_getParent",x@pointer,getSample(x),as.integer(y)-1)+1
#	browser()
	if(length(pInd)>0)#if parent exist
	{
		pstats<-.Call("R_getPopStats",x@pointer,getSample(x),as.integer(pInd)-1)
	}else
		pstats<-stats#list(FlowCore=c(count=0,proportion=0),FlowJo=c(count=0,proportion=0))
	
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
#	list(flowCore=c(parent.total=as.numeric(pstats$FlowCore["count"]),count=as.numeric(stats$FlowCore["count"]))
#		,flowJo=c(parent.total=as.numeric(pstats$FlowJo["count"]),count=as.numeric(stats$FlowJo["count"])
#					)	
#	)	
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


#setMethod("execute",signature(hierarchy="GatingHierarchyInternal"),function(hierarchy,cleanup=FALSE,keep.indices=TRUE,isNcdf=FALSE,ncfs=NULL,dataEnvironment=NULL,...){
#			##Conditional compilation for HAVE_NETCDF
#			if(isNcdf&!TRUE){
#				stop("isNcdf must be FALSE, since you don't have netcdf installed");
#			}
#			if(hierarchy@flag){
#				message("This file is already gated\n")
#				return()
#			}
#			
#			x<-hierarchy@tree;
#			
#			#doc<-workspace@doc
#			nlist<-RBGL::bfs(x,nodes(x)[1]);
#			file<-getSample(hierarchy,isFullPath=TRUE)
#			
#			message("Loading data file: ",file);
#			data<-read.FCS(file);
#			
#			cid<-getCompID(hierarchy)
##			cid<-get("compID",envir=nodeData(x,nlist[1],"metadata")[[1]])
##			cal<-hierarchy@transformations;
#			cal<-getTransformations(hierarchy);
#			
#			if(cid!=-1 & cid!=-2){
#				message("Compensating");
#				#compobj<-compensation(.getCompensationMatrices(doc)[[as.numeric(cid)]])
#				compobj<-compensation(getCompensation(hierarchy));
#				#TODO this compensation will fail if the parameters have <> braces (meaning the data is stored compensated).
#				#I need to handle this case properly.
#				res<-try(compensate(data,compobj),silent=TRUE)
#				if(inherits(res,"try-error")){
#					message("Data is probably stored already compensated");
#				}else{
#					data<-res
#					rm(res);
#					#gc(reset=TRUE);
#				}
#				cnd<-colnames(data)
#				if(is.null(cnd)){cnd<-as.vector(parameters(data)@data$name)}
#				wh<-cnd%in%parameters(compobj)
#				cnd[wh]<-paste("<",parameters(compobj),">",sep="")
#				
#				#colnames(data)<-cnd;
#				e<-exprs(data)
#				d<-description(data);
#				p<-parameters(data);
#				p@data$name<-cnd
#				colnames(e)<-cnd;
#				data<-new("flowFrame",exprs=e,description=d,parameters=p)						
#				#cmnm<-names(.getCompensationMatrices(doc))[as.numeric(cid)]
#				#Don't need this anymore.
#				# #cal<-.getCalibrationTableSearch(doc,cmnm)
#				# if(length(cal)==0){
#				# 	stop("Can't find the calibration table named : ", cmnm, " in this workspace.");
#				# }
#			}
#			else if(cid==-2){
#				#TODO the matrix may be acquisition defined.
#				message("No compensation");
#			}
#			else if(cid==-1){
#				##Acquisition defined compensation.
#				#nm<-unique(do.call(c,lapply(strsplit(.getCalibrationTableNames(doc)," "),function(x)x[1])))[2]
#				nm<-getCompensation(hierarchy)
#				if(grepl("Acquisition-defined",nm)){
#					###Code to compensate the sample using the acquisition defined compensation matrices.
#					message("Compensating with Acquisition defined compensation matrix");
#					#browser()
#					comp<-compensation(spillover(data)$SPILL)
#					hierarchy@compensation<-spillover(data)$SPILL
#					res<-try(compensate(data,comp),silent=TRUE)
#					if(inherits(res,"try-error")){
#						message("Data is probably stored already compensated");
#					}else{
#						data<-res
#						rm(res);
#						#gc(reset=TRUE);
#					}
#					cnd<-colnames(data)
#					wh<-cnd%in%parameters(comp)
#					cnd[wh]<-paste("<",parameters(comp),">",sep="")
#					e<-exprs(data)
#					d<-description(data);
#					p<-parameters(data);
#					p@data$name<-cnd
#					colnames(e)<-cnd;
#					data<-new("flowFrame",exprs=e,description=d,parameters=p)
#					#gc(reset=TRUE);
#				}
#				#Ditto
#				#cal<-.getCalibrationTableSearch(doc,nm)
#			}
#			#gc(reset=TRUE)
#			message("Transforming");
#			axis.labels<-list();
#			.flowJoTransform(environment(),cal);
#			#gc(reset=TRUE);
#			#wh<-which(data@parameters@data$name=="Time")
#			wh<-grep("^Time$",data@parameters@data$name)
#			if(length(wh!=0)){
#				#gc(reset=TRUE);
#				parameters(data)@data[wh,4:5]<-range(exprs(data[,wh]));
#				#gc(reset=TRUE);
#				parameters(data)@data[wh,3]<-diff(range(exprs(data[,wh])));
#			}
#			#gc(reset=TRUE);
#			# if(!isNcdf){
#			e<-new.env(parent=.GlobalEnv);
#			# }
#			#########################################
#			#if use ncdfFlowSet,then add the transformed
#			#matrix to the ncdf file,assign it to env
#			#########################################
#			if(!isNcdf)
#				assign("data",data,envir=e)
#			else
#			{
#				sampleName<-getSample(hierarchy)
#				
#				#create ncdf file for indices
#				#moving this code to writeGatesToNetCDF
#				
#				if(.packageLoaded("Rmpi")){
#					#ask for permission to write to netcdf
#					mpi.send.Robj(0,0,3)
#					mpi.recv.Robj(0,4)
#					addFrame(dataEnvironment$ncfs,data,sampleName)
#					
#					
#					#let master know I'm done
#					mpi.send.Robj(0,0,8)
#					
#				}else{
#					addFrame(dataEnvironment$ncfs,data,sampleName)
#				}
#				
#				###all the annotData including colnames for each flowFrame slots are already updated in addFrame funciton
#				###but not the colnames slot in ncdfFlow,this redundant slot seems cause problem,consider to be removed
#				colnames(dataEnvironment$ncfs)<-colnames(data)##currently it is updated repeatedly by each sample
#				colnames(ncfs)<-colnames(data)
#				multiassign(c("data","sampleName"),list(dataEnvironment,sampleName),envir=e)
#			}
#			assign("axis.labels",axis.labels,envir=e);
#			rm(data);
#			#gc(reset=TRUE)
#			nodeDataDefaults(x,"data")<-e;
#			.Call("R_gating",hierarchy@pointer,getSample(hierarchy))
#			#Nodes to parse later
##			skipforlater<-list();
##			lastparent<-nlist[1];
##			for (node in nlist){
##				#20110314 check if the current node has already been gated
##				if(.isGated.graphNEL(hierarchy,node))
##					next
##				else
##				{
##					parentname<-(setdiff(adj(ugraph(x),node)[[1]],adj(x,node)[[1]]));
##					if(length(parentname)==0){
##						parentname<-node
##						
##						assign("thisTot",as.numeric(nodeData(x,node,"metadata")[[1]][["count"]]),envir=nodeData(x,node,"metadata")[[1]]);
##						hierarchy@tree<-x;
##						##the bug discovered and fixed on 06132011
###					assign("thisIndices",list(getIndices(hierarchy,node),env=nodeData(x,node,"metadata")[[1]]));
##						l<-list(getIndices(hierarchy,node))
###					browser()
##						.saveIndices(x,sampleName,node,l,isNcdf=FALSE)
##						
##						#20110314 set the flag after gating
##						assign("isGated",TRUE,envir=nodeData(x,node,"metadata")[[1]])
##						lastparent<-parentname;
##						next;
##					};
##					if(lastparent!=parentname&length(skipforlater)!=0){
##						hierarchy@tree<-x;
##						for(i in skipforlater){
###						browser()
##							
##							hierarchy<-.calcBooleanGate(hierarchy,i)
##						}
##						skipforlater<-list();
##						lastparent<-parentname;
##					}
##					hierarchy@tree<-x;
##					if(.isBooleanGate.graphNEL(hierarchy,node)){
##						#Skip Boolean Gates and Compute them later
##						skipforlater<-c(skipforlater,node);
##						lastparent<-parentname;
##						next;
##					}
##					#check if the current node has been gated during the skipforlater loop
##					#Perhaps this check should go into .calcGate?
##					#Done - check is inside .calcGate
##					#gating the regular gate
##					
##					.calcGate(hierarchy,node)
##					#}
##					lastparent<-parentname;
##				}
##			}
##			if(length(skipforlater)!=0){
##				hierarchy@tree<-x;
##				for(i in skipforlater){
##					hierarchy<-.calcBooleanGate(hierarchy,i)
##				}
##				skipforlater<-list();
##			}
#			
#			#Done gating, now write all the indices to netcdf
#			#Checks the ncdf flag of hierarchy in the function
##		browser()
#			hierarchy@flag<-TRUE;
##			writeGatesToNetCDF(hierarchy,isNew=TRUE)
#			
#			if(cleanup){
#				nodeDataDefaults(x,"data")<-new.env(parent=.GlobalEnv);
#				#gc(reset=TRUE);
#			}
##			hierarchy@tree<-x;
#			# message("Computing Boolean Gates");
#			# hierarchy<-.calcBooleanGates(hierarchy);
#			
##			hierarchy
#		})
		
setMethod("getGate",signature(obj="GatingHierarchyInternal",y="character"),function(obj,y){
			
#			browser()
			ind<-which(getNodes(obj,y)%in%y)
			g<-getGate(obj,ind)
			g
			
		})
		#return gate y for a given hierarchy (by index)
setMethod("getGate",signature(obj="GatingHierarchyInternal",y="numeric"),function(obj,y,tsort=FALSE){
			vertexID=y-1
			if(vertexID<=0)
				return (NA)
			else
			{
#				browser()
				g<-.Call("R_getGate",obj@pointer,getSample(obj),vertexID)
#				browser()
				if(g$type==1)
					polygonGate(.gate=matrix(c(g$x,g$y),ncol=2,dimnames=list(NULL,g$parameters)),filterId=getNodes(obj)[y])
				else if(g$type==2)
					rectangleGate(.gate=matrix(g$range,dimnames=list(NULL,g$parameters)),filterId=getNodes(obj)[y])
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
			ind<-which(getNodes(obj)%in%y)
#			browser()
			getIndices(obj,ind)
			
		})
setMethod("getIndices",signature(obj="GatingHierarchyInternal",y="numeric"),function(obj,y){
			

			.Call("R_getIndices",obj@pointer,getSample(obj),as.integer(y-1))
			
		})	
setMethod("getData",signature(obj="GatingHierarchyInternal"),function(obj,y=NULL,tsort=FALSE){
			if(!obj@flag){
				stop("Must run execute() before fetching data");
			}

			r<-nodeDataDefaults(obj@tree,"data")$data$ncfs[[getSample(obj)]]
#			browser()			
			if(is.null(y)||y==1||getNodes(obj)[1]==y){
				return (r)	
			}else if(is.numeric(y)){
				n<-getNodes(obj,tsort=tsort)[y]
				return (getData(obj,n,tsort=tsort))
			}else{
				return (r[getIndices(obj,y),])
			}
			
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
			get("axis.labels",envir=nodeDataDefaults(obj@tree)$data)
		})

#this method currently is used convert transformation funtion from c++ to R
##mainly for transforming range info
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
							    res <- .C(C_spline_eval, z$method, as.integer(length(x)), 
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
.getChannelMarker<-function(pd,name)
{
	#try stain name
	
#	browser()
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
			y<-match(y,getNodes(x))
			plotGate(x,y,...)
			
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
			##filter out boolean gates when bool==FALSE
			if(!bool)
			{
				boolInd<-unlist(lapply(y,.isBoolGate,x=x))
				y<-y[!boolInd]
			}
			plotList<-poplist<-as.list(y)
			names(plotList)<-plotList
			
			if(merge)
			{
				#check if they have same parents and parameters
				keylist<-sapply(plotList,function(y){
							
							if(!.isBoolGate(x,y))
							{
								curGate<-getGate(x,y)
	#							browser()
								if(extends(class(curGate),"filter"))
								{
									pid<-getParent(x,y)
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
			plotObjs<-lapply(plotList,function(y){
						
						return(.plotGate(x,y,...))
					})
			
			if(arrange)			
				do.call(grid.arrange,c(plotObjs,main=main))
			else
				plotObjs
			
})
.plotGate<-function(x,y,add=FALSE,border="red",tsort=FALSE,main=NULL,margin=FALSE,smooth=FALSE,xlab=NULL,ylab=NULL,xlim=NULL,ylim=NULL,stat=TRUE,scales=list(),...){			
		
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
					message("Can't plot. There is no gate defined for node ",getNode(x,y));
					invisible();			
					return(NULL)
				}

			}			
			
			parentdata<-getData(x,pid)

			#################################
			# setup axis labels and scales
			################################
			if(class(curGate)=="BooleanGate")
			{
				
				params<-rev(parameters(getGate(x,getParent(x,y))))
				ind<-getIndices(x,y)
				curGate<-getData(x)[ind,params]##get gated pop from indexing the root pop because ind here is global
#				attr(curGate,"class")<-"filter"
				
				panelFunc<-panel.xyplot.flowFrame.booleanGate
			}else
			{
				if(class(curGate)=="filters")
					params<-rev(parameters(curGate[[1]]))
				else
					params<-rev(parameters(curGate))
				panelFunc<-panel.xyplot.flowframe
			}
		
			
			if(length(params)==1)
			{
				xParam=params
				yParam="SSC-A"
				params<-c(yParam,xParam)
			}else
			{
				yParam=params[1]
				xParam=params[2]
				
			}
			pd<-pData(parameters(parentdata))
			xObj<-.getChannelMarker(pd,xParam)
			yObj<-.getChannelMarker(pd,yParam)
			
			xlab<-sub("NA","",paste(unlist(xObj),collapse=" "))
			ylab<-sub("NA","",paste(unlist(yObj),collapse=" "))
#			browser()
				if(length(params)==2){
					xParam.ind<-match(xParam,pd$name)
					yParam.ind<-match(yParam,pd$name)
					x.labels<-getAxisLabels(x)[[xParam.ind]]
					y.labels<-getAxisLabels(x)[[yParam.ind]]
					
					#init the scales and x,y lim
					
					xlim=range(parentdata)[,xParam]
					ylim=range(parentdata)[,yParam]
					
					#update axis when applicable
					if(!is.null(x.labels))
					{
						xscales<-list(x=list(at=x.labels$at,labels=x.labels$label,rot=45))
						scales<-lattice:::updateList(xscales,scales)
						xlim=range(x.labels$at)
					}
					if(!is.null(y.labels))
					{	
						yscales<-list(y=list(at=y.labels$at,labels=y.labels$label))
						scales<-lattice:::updateList(scales,yscales)
						ylim=range(y.labels$at)
					}
#					browser()	
					# extend lim by comparing to gate boundary
#					gateBoundary<-curGate@boundaries
#					xdataRange<-range(exprs(parentdata)[,xParam])
#					ydataRange<-range(exprs(parentdata)[,yParam])
#					xlim[1]<-min(min(xdataRange),xlim[1])
#					xlim[2]<-max(max(xdataRange),xlim[2])
#					
#					ylim[1]<-min(min(ydataRange),ylim[1])
#					ylim[2]<-max(max(ydataRange),ylim[2])
				}



	
		#################################
		# the actual plotting
		################################
		if(add)
		{
#			if(class(curGate)=="BooleanGate")
#			{
#				points(exprs(pd[,dims]),col=as.numeric(ind[ind.p])+1,pch='.',xlab=paste(trimWhiteSpace(na.omit(dims[1])),desc[1],sep=" "),ylab=paste(trimWhiteSpace(na.omit(dims[2])),desc[2],sep=" "));
#			}else
#			{
#				trellis.focus(highlight=FALSE)
#				panel.polygon(getBoundaries(y),border=border,...)
#				trellis.unfocus();
#			}
		}else
		{
			f1<-mkformula(params)
			res<-xyplot(x=f1
						,data=parentdata[,params]
						,filter=curGate
						,xlab=xlab
						,ylab=ylab
						,margin=margin
						,smooth=smooth
#						,scales=scales
						,main=main
						,stat=stat
						,panel=panelFunc
						,...
						)	
			return(res)
		}
				
			
			
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