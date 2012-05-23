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
			
			ind<-which(getNodes(obj)%in%y)
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
#				else if(g$type==3)
					
				else
					stop("not supported gate type",g$type)
				
			}
		})

setMethod("getIndices",signature(obj="GatingHierarchyInternal",y="character"),function(obj,y){
			ind<-which(getNodes(obj)%in%y)
#			browser()
			.Call("R_getIndices",obj@pointer,getSample(obj),ind-1)
			
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
##dummy function for now
.isBoolGate<-function(x,y){
	return (FALSE)
}
setMethod("getDimensions",signature(obj="GatingHierarchyInternal",y="character"),function(obj,y,index=FALSE){
#			browser()
			if(.isBoolGate(obj,y)){
				getDimensions(obj,getParent(obj,y),index=index);
			}else{
				if(!index){
					if(length(getGate(obj,y)@parameters)==1){
						c(getGate(obj,y)@parameters[[1]]@parameters);
					}else{
						c(getGate(obj,y)@parameters[[1]]@parameters,getGate(obj,y)@parameters[[2]]@parameters)
					}
				}else{
					if(length(getGate(obj,y)@parameters)==1){
						if(.isCompensated(obj)){
							tmp<-parameters(getData(obj,y))@data$name
						}else{
							tmp<-gsub(">","",gsub("<","",parameters(getData(obj,y))@data$name))
						}
						c(match(getGate(obj,y)@parameters[[1]]@parameters,tmp))	
					}else{
						if(.isCompensated(obj)){
							tmp<-parameters(getData(obj,y))@data$name
						}else{
							tmp<-gsub(">","",gsub("<","",parameters(getData(obj,y))@data$name))
						}
						c(match(getGate(obj,y)@parameters[[1]]@parameters,tmp),match(getGate(obj,y)@parameters[[2]]@parameters,tmp))
					}
				}
			}
		})
#setGeneric("dataEnv",function(obj){
#			standardGeneric("dataEnv")
#		})
#setMethod("dataEnv",signature(obj="GatingHierarchyInternal"),function(obj){
#			nodeDataDefaults(obj@tree,"data")
#		})

setMethod("getAxisLabels",signature(obj="GatingHierarchyInternal",y="missing"),function(obj,y=NULL,...){
			get("axis.labels",envir=nodeDataDefaults(obj@tree)$data)
		})
setMethod("getTransformations","GatingHierarchyInternal",function(x){
#			browser()
			trans<-.Call("R_getTransformations",x@pointer,getSample(x))
			lapply(trans,function(curTrans){
#						browser()
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
							return (f)
					})
			
		})
		
##it is currently only for internal use		
setMethod("getCompensationMatrices","GatingHierarchyInternal",function(x){
			.Call("R_getCompensation",x@pointer,getSample(x))
})
setMethod("plotGate",signature(x="GatingHierarchyInternal",y="character"),function(x,y,add=FALSE,border="red",tsort=FALSE,...){
#plotGate1<-function(x,y,add=FALSE,border="red",tsort=FALSE,smooth=FALSE,fast=FALSE,...){
			if(!x@flag){
				message("Can't plot until you gate the data with 'execute()'\n");
				return();
			}
#			browser()
			nodelist<-getNodes(x)
			nodeInd<-nodelist%in%y
			
			#do we pass a "main" argument for the title?
			if(is.null(match.call()$main)){
				#fjName
				fjName<-getNodes(x,isPath=T)[nodeInd]
				#sampleName
				sname<-getSample(x)
				#construct plot title for this gate
				mtitle<-paste(sname,fjName,sep="\n")
				cl<-match.call(expand.dots=TRUE)
				cl$main<-mtitle
				return(eval(cl,parent.frame()));
			}
#			cachedata<-getData(x,y);
			parentdata<-getData(x,getParent(x,y));
#			rootdata<-getData(x);
			
			main<-match.call()$main;
			##Two cases: gate is boolean, or gate is normal
			##Boolean gates are treated differently
#			browser()
			if(.isBoolGate(x,y)){
				p<-getParent(x,y);
				ind<-getIndices(x,y)
				ind.p<-getIndices(x,p)
				pd<-parentdata
				dims<-getDimensions(x,p);
				if(!.isCompensated(x)){
#					pnames.data<-cachedata@parameters@data$name
#					pnames.desc<-cachedata@parameters@data$desc
					pnames.data<-parentdata@parameters@data$name
					pnames.desc<-parentdata@parameters@data$desc
					pnames<-gsub(">","",gsub("<","",pnames.data))
				}else{
#					pnames<-pnames.data<-cachedata@parameters@data$name
#					pnames.desc<-cachedata@parameters@data$desc
					pnames<-pnames.data<-parentdata@parameters@data$name
					pnames.desc<-parentdata@parameters@data$desc
				}
				dims<-pnames.data[match(dims,pnames)][na.omit(match(pnames,dims))]
				desc<-pnames.desc[match(dims,pnames)][na.omit(match(pnames,dims))]
				
				dim.ind<-getDimensions(x,p,index=TRUE)[na.omit(match(pnames,dims))]
				#dims<-dims[na.omit(match(pnames,dims))]
				if(add){
					points(exprs(pd[,dims]),col=as.numeric(ind[ind.p])+1,pch='.',xlab=paste(trimWhiteSpace(na.omit(dims[1])),desc[1],sep=" "),ylab=paste(trimWhiteSpace(na.omit(dims[2])),desc[2],sep=" "));
				}else{
#					if(!fast){
						plot((pd[,dims]),xlab=paste(trimWhiteSpace(na.omit(dims[1])),desc[1],sep=" "),ylab=paste(trimWhiteSpace(na.omit(dims[2])),desc[2],sep=" "),col=as.numeric(ind[ind.p])+1,smooth=smooth,main=main);
#					}else{
#						plot(hexbin(pd[,dims]),xlab=paste(trimWhiteSpace(na.omit(dims[1])),desc[1],sep=" "),ylab=paste(trimWhiteSpace(na.omit(dims[2])),desc[2],sep=" "),col=as.numeric(ind[ind.p])+1)
#					}
					points(exprs(pd[,dims][ind[ind.p],]),col="red",cex=2,pch='.');
					
				}
				invisible()
			}else if(suppressWarnings(is.na(getGate(x,y)))){
				message("Can't plot. There is no gate defined for node ",y);
				invisible();			
			}else{
				if(add==FALSE){
#					
					dims<-getDimensions(x,y)
					if(!.isCompensated(x)){
						pnames.data<-(getData(x,y,tsort=tsort)@parameters@data$name)
						pnames<-gsub(">","",gsub("<","",pnames.data))
					}else{
						pnames<-pnames.data<-(getData(x,y,tsort=tsort)@parameters@data$name)
						
					}
#					browser()
#					dims2<-pnames.data[match(dims,pnames)][na.omit(match(pnames,dims))]
#					dim.ind<-getDimensions(x,y,index=TRUE)[na.omit(match(pnames,dims))]
					dims2<-pnames.data[match(dims,pnames)]
					dim.ind<-getDimensions(x,y,index=TRUE)
					par.desc<-parameters(parentdata)@data$desc[dim.ind]
					if(!any(is.na(par.desc))){
						dflag<-TRUE
					}else{
						dflag<-FALSE
					}
					pd<-parentdata[,dims2];
					if(dflag){
						#warning this may sometimes fail	
						colnames(pd)<-parameters(pd)@data$desc
					}
					#use dimnames from the data
					form<-mkformula(rev(dims2));
					if(length(dims2)==2){
#						browser()
						if(is.null(getAxisLabels(x)[[dim.ind[1]]])&is.null(getAxisLabels(x)[[dim.ind[2]]])){
							scales<-list()
							xlim=range(parentdata[,dims2[1]])
							ylim=range(parentdata[,dims2[2]])
						}else if(!is.null(getAxisLabels(x)[[dim.ind[1]]])&is.null(getAxisLabels(x)[[dim.ind[2]]])){
							scales<-list(x=list(at=getAxisLabels(x)[[dim.ind[1]]]$at,labels=getAxisLabels(x)[[dim.ind[1]]]$label),x=list(rot=45))
							xlim=range(getAxisLabels(x)[[dim.ind[1]]]$at)
							ylim=range(parentdata[,dims2[2]])
						}else if(is.null(getAxisLabels(x)[[dim.ind[1]]])&!is.null(getAxisLabels(x)[[dim.ind[2]]])){
							scales<-list(y=list(at=getAxisLabels(x)[[dim.ind[2]]]$at,labels=getAxisLabels(x)[[dim.ind[2]]]$label),x=list(rot=45))
							xlim=range(parentdata[,dims2[1]])
							ylim=range(getAxisLabels(x)[[dim.ind[2]]]$at)		
						}else if(!is.null(getAxisLabels(x)[[dim.ind[1]]])&!is.null(getAxisLabels(x)[[dim.ind[2]]])){
							scales<-list(x=list(rot=45,at=getAxisLabels(x)[[dim.ind[1]]]$at,labels=getAxisLabels(x)[[dim.ind[1]]]$label),y=list(at=getAxisLabels(x)[[dim.ind[2]]]$at,labels=getAxisLabels(x)[[dim.ind[2]]]$label))
							xlim=range(getAxisLabels(x)[[dim.ind[1]]]$at)
							ylim=range(getAxisLabels(x)[[dim.ind[2]]]$at)		
						}
						#If 2D use xyplot.
						#TODO add stains to labels
#						xylab<-gsub("NA","",paste(pData(parameters(rootdata))$name,pData(parameters(rootdata))$desc))
						xylab<-gsub("NA","",paste(pData(parameters(parentdata))$name,pData(parameters(parentdata))$desc))

						res<-xyplot(x=form
									,data=parentdata[,dims2]
#									,smooth=smooth
									,xlab=xylab[dim.ind[1]]
									,ylab=xylab[dim.ind[2]]
									,frame.plot=TRUE
									,scales=scales
									,nbin=512
									,filter=getGate(x,y)
									,panel=function(gh=x,g=y,tsort=tsort,main=main,...){
#	#									gp <- list(...)[["par.settings"]]
#										browser()
										panel.xyplot.flowframe(...)
#										
#										#the gate names sometimes don't match the dimension names.. 
#										#If there's a mix of compensated and uncompensated samples.
#										dims<-colnames(getBoundaries(gh,g))
#	#										browser()
#										if(.isCompensated(gh)){
#											dims<-dims[na.omit(match((getData(gh,g,tsort=tsort)@parameters@data$name),dims))]
#										}else{
#											tmp<-gsub(">","",gsub("<","",getData(gh,g,tsort=tsort)@parameters@data$name))
#											dims<-colnames(getBoundaries(gh,g))
#											dims<-na.omit(dims[match(tmp,dims)])
#										}
#										#Case for rectangle or polygon gate
#										if(length(dims)>1){
#											panel.polygon(getBoundaries(gh,g)[,dims],border="red",lwd=list(...)$lwd);
#										}else{
#											
#											apply(getBoundaries(gh,g)[,dims,drop=FALSE],1,function(x)panel.abline(v=x,col="red"))
#										}
									}
									,...
									)
						return(res)
						
					}else{
#						browser()
						if(is.null(getAxisLabels(x)[[dim.ind[1]]])){
							scales<-list();
						}
						else{
							scales<-list(x=list(at=getAxisLabels(x)[[dim.ind[1]]]$at,labels=getAxisLabels(x)[[dim.ind[1]]]$label))
						}
						data=data.frame(exprs(parentdata[,dim.ind]))
						colnames(data)<-flowViz:::expr2char(form[[2]])
						res<-densityplot(x=form,data=data,scales=scales,#prepanel=
								panel=function(...,gh=x,g=y){
									panel.densityplot(...);
#									browser()
									apply(getBoundaries(gh,g)[,dims,drop=FALSE],1,function(x)panel.abline(v=x,col="red"))
								},...)
						return(res)
					}
				}else{
					#add=TRUE
					trellis.focus(highlight=FALSE)
					dims<-colnames(getBoundaries(x,y))
					dims<-dims[na.omit(match((getData(x,y,tsort=tsort)@parameters@data$name),dims))]
					panel.polygon(getBoundaries(x,y)[,dims],border="red",...)
					trellis.unfocus();
				}
			}
		})
setMethod("plotGate",signature(x="GatingHierarchyInternal",y="numeric"),function(x,y,add=FALSE,border="red",tsort=FALSE,...){
			node<-getNodes(x,tsort=tsort)[y];
			if(is.na(node)){
				warning("Can't plot gate ", y, " doesn't exist.");
				return(1);
			}
			plotGate(x,y=node,add=add,border=border,tsort=tsort,...);
		})