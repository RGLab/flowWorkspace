setMethod("openWorkspace",signature=signature(file="character"),definition= function(file){
 	message("We do not fully support all features found in a flowJo workspace, nor do we fully support all flowJo workspaces at this time.")
	if(inherits(file,"character")){
		x<-xmlTreeParse(file,useInternal=TRUE);
	}else{
		stop("Require a filename of a workspace, but received ",class(x)[1]);
	}
	ver<-xpathApply(x,"/Workspace",function(x)xmlGetAttr(x,"version"))[[1]]
	x<-new("flowJoWorkspace",version=ver,.cache=new.env(parent=emptyenv()),file=basename(file),path=dirname(file),doc=x)
	x@.cache$flag=TRUE;
	return(x);
})

setAs("list", "GatingSet", function(from, to ){
	if(!all(unlist(lapply(from,function(y)class(y)=="GatingHierarchy")))){
		stop("Can't coerce this list to class GatingSet");
	}
	new(to, set=from)
})


setMethod("show",signature("flowJoWorkspace"),function(object){
	cat("FlowJo Workspace Version ",object@version,"\n");
	cat("File location: ",object@path,"\n");
	cat("File name: ",object@file,"\n");
	if(object@.cache$flag){
		cat("Workspace is open.","\n");
	}else{
		cat("Workspace is closed.","\n")
	}
})


setMethod("closeWorkspace","flowJoWorkspace",function(workspace){
	free(workspace@doc);
	workspace@.cache$flag<-FALSE;
})
setOldClass("summary")

setMethod("summary",signature("flowJoWorkspace"),function(object,...){
	show(object,...);
})

setMethod("parseWorkspace",signature("flowJoWorkspace"),function(obj,name=NULL,execute=FALSE,isNcdf=FALSE,subset=NULL,...){
	if(isNcdf&!TRUE){
	stop("isNcdf must be FALSE since you don't have netcdf installed");
	}
	message("We do not fully support all versions of flowJo XML workspaces at this time.")
	message("If your workspace is not supported or if this package throws an error, we encourage you to contact the package maintainter with a bug report. We will endeavour to add support for other features upon request.")
	##########################################################
	#path needs to be specified anyway to get full path of fcs
	#########################################################
#	if(execute){
		m<-match("path",names(list(...)))
		if(is.na(m)){
			stop("If execute=TRUE, you must specify a path to the fcs files via path= argument");
		}
		path=list(...)$path
#	}
#	browser()
	x<-obj@doc;
	.hasNN(x);
	wsversion<-xpathApply(x,"/Workspace",function(z)xmlGetAttr(z,"version")[[1]])[[1]];
	if(wsversion=="1.6"){
		#Windows version code
		s<-.getSamples(x,win=TRUE);
		g<-.getSampleGroups(x,win=TRUE);
	}else{
		s<-.getSamples(x);
		g<-.getSampleGroups(x);
	}
	sg<-merge(s,g,by="sampleID");
	##Remove samples where there are no gates:
	#emptysid<-as.numeric(unlist(lapply(unlist(lapply(xpathApply(obj@doc,"/Workspace/SampleList/Sample"),function(x)xpathApply(x,"./ancestor-or-self::Sample[count(descendant::Population)=0]")),recursive=FALSE),function(x)xpathApply(x,".",function(x)xmlGetAttr(x,"sampleID")))))
	#pop.counts<-unlist(lapply(xpathApply(obj@doc,"/Workspace/SampleList/Sample"),function(x)xpathApply(x,"count(descendant::Population)")))
	sg<-sg[sg$pop.counts>0,]
	if(!missing(subset)){
		message("Parsing ",length(subset)," samples");
	}else{
			message("Parsing ",length(unique(sg$sampleID))," samples");
		}
	if(wsversion=="2.0"){
		##samples may have no compID if they are only log-transformed, for example.
		##Keep samples with compID = NA and set it to -2
		#If the compID = NA, check the Sample Parameter attributes for the transformation information.
		#sg<-sg[!is.na(sg$compID),]		
		#sg.na<-is.na(sg$compID);
		message("Version recognised. Continuing..")
	}else if (wsversion=="1.6"){
		#Windows compensation and transformation work differently.. there is no comp id
		stop("Sorry, we don't support this type of workspace (flowJo Windows) at the moment. But we are working on it!")
		
	}else{
		stop("Workspace Version not supported");
	}
	sg$groupName<-factor(sg$groupName)
	groups<-levels(sg$groupName)
	if(is.null(name)){
	message("Choose which group of samples to import:\n");
	result<-menu(groups,graphics=FALSE);
	}else if(is.numeric(name)){
		if(length(groups)<name)
			stop("Invalid sample group index.")
		result<-name
	}else if(is.character(name)){
		if(is.na(match(name,groups)))
			stop("Invalid sample group name.")
		result<-match(name,groups)
	}
	if(wsversion=="2.0"){
		l<-sapply(sg[sg$groupName==groups[result],]$sampleID,function(i){
			xpathApply(x,paste("/Workspace/SampleList/Sample[@sampleID='",i,"']",sep=""))[[1]]
			})
	}else if(wsversion=="1.6"){
		l<-sapply(sg[sg$groupName==groups[result],]$sampleID,function(i){
			xpathApply(x,paste("/Workspace/SampleList/Sample/DataSet[@sampleID='",i,"']",sep=""))[[1]]
			})
	}else{
		stop("Workspace Version not Supported");
	}
	# Allow import of a subset of samples
	if(!missing(subset)){
		if(max(subset)<=length(l)&min(subset)>=1)
		l<-l[subset]
	}
	G<-lapply(l,function(x){
		message("Parsing sampleID ",xmlGetAttr(x,"sampleID"));
		.getPopulations(x,env=NULL);
		graph<-get("gr",get("env",globalenv()))
		if(exists("transformations",env)){
			transformations<-get("transformations",get("env",globalenv()))
		}else{
			transformations<-list();
		}
		if(exists("compensation",env)){
			compensation<-get("compensation",get("env",globalenv()))
		}else{
			compensation<-matrix();
		}
		list(graph=graph,transformations=transformations,compensation=compensation)
		}
		)
	
		fn<-do.call(c,lapply(G,function(x){		
			#tmp<-try(nodeData(x$graph)[[1]])
			get("fcsfile",env=nodeData(x$graph)[[1]]$metadata)
		}))
		names(G)<-fn
	
		for(i in 1:length(G)){
			nodeDataDefaults(G[[i]]$graph,"group")<-groups[result]
			#########################################################
			#get full path for each fcs and store in dataPath slot
			#########################################################
			file<-names(G[i])
			absPath<-list.files(pattern=file,path=path,recursive=TRUE,full=TRUE)
			if(length(absPath)==0){
				stop("Can't find ",file," in directory: ",path,"\n");
			}
			G[[i]]$dataPath<-dirname(absPath[1])
		}
		
		G<-lapply(G,function(x)new("GatingHierarchy",tree=x$graph,nodes=nodes(x$graph),name=get("fcsfile",env=nodeData(x$graph)[[1]]$metadata),flag=FALSE,transformations=x$transformations,compensation=x$compensation,dataPath=x$dataPath))
		G<-new("GatingSet",set=G);

		if(execute){
			##################################################
			#create ncdf file without adding matrices yet
			#################################################	
			if(isNcdf){
				ncfs1  <- createNcdfFlowSet(files=getSamples(G,isFullPath=TRUE),flowSetId="fs1",isWriteSlice=FALSE)
				dataEnvironment=new.env(parent=.GlobalEnv);
				assign("ncfs",ncfs1,envir=dataEnvironment)

			}
			
#			if("path"%in%names(list(...))){
				G<-lapply(G,function(x)execute(hierarchy=x,isNcdf=isNcdf,ncfs=ncfs1,dataEnvironment=dataEnvironment))
#				G<-lapply(G,function(x)execute(hierarchy=x,isNcdf=isNcdf,ncfs=ncfs1,e=NULL))
#			}else{
#				G<-lapply(G,function(x)execute(hierarchy=x,path=obj@path,isNcdf=isNcdf,ncfs=ncfs1))
#			}
		}
		return(G);
})
.parseBooleanGate<-function(x,y,z){
	x<-gsub("! G","!G",x)
	
	tokens<-strsplit(x," ")[[1]]
	##Pull off each token. 
	#eliminate space between ! G..
	N<-(length(tokens)+1)/2
	#N<-length(grep("G",tokens)) #Number of gates;
	gind<-(1:N)*2-1	
	#gind<-grep("G",tokens); #gate indices
	if(N>1){
		lind<-(1:(N-1))*2
	}else{
		lind<-0;
	}
	reord<-order(as.numeric(gsub("!","",gsub("G","",tokens[gind]))))
	refs<-sapply(y,function(nme){
		##conditionally generate a query based on the full population name
		if(length(strsplit(nme,"/")[[1]])==2&strsplit(nme,"/")[[1]][1]==""){
			#Code for relative pop name
			nd<-xpathApply(z,paste("./ancestor::Population[2]/descendant::Population[@name='",basename(nme),"']",sep=""))[[1]]
			paste(xmlGetAttr(nd,"nn"),xmlGetAttr(nd,"name"),sep=".");
		}else{
			prefix<-"./ancestor::Sample/";
			query<-paste(prefix,sapply(strsplit(nme,"/")[[1]][!strsplit(nme,"/")[[1]]==""],function(q){
				paste("descendant::Population[@name='",q,"']",sep="")
				}),collapse="/")
			nd<-xpathApply(z,query)[[1]]
			paste(xmlGetAttr(nd,"nn"),xmlGetAttr(nd,"name"),sep=".")
		}
	})[reord]
	not<-grep("!",tokens[gind])
	and<-grep("\\&",tokens[lind])
	or<-grep("\\|",tokens[lind])
	e<-grep("!",tokens[gind],invert=TRUE)
 	v<-vector(mode="character",length=length(gind))
	v2<-vector(mode="character",length=length(lind));
	v[not]<-"!"
	v[e]<-""
	v2[and]<-"&"
	v2[or]<-"|"
	g<-list(v=v,v2=v2,ref=refs);
	class(g)<-"BooleanGate";
	return(g)
}
.parseBooleanGates<-function(x,g){
	
	x<-xpathApply(x,"./BooleanGate")[[1]];
	#x is a single gate now.
	def<-xmlGetAttr(x,"specification")
	## generate a query based on the full population name
	gnames<-unlist(xpathApply(x,".//String",function(x)(xmlValue(x))))
	nm<-xpathApply(x,"./parent::Population",function(x){nm<-xmlGetAttr(x,"name");nn<-xmlGetAttr(x,"nn");paste(nn,nm,sep=".");})[[1]]
	attachto<-sapply(1,function(i){p<-xpathApply(x,"./parent::Population/parent::Population",xmlAttrs)[[1]];paste(p["nn"],".",p["name"],sep="")})
	counts<-sapply(1,function(i){xpathApply(x,"./parent::Population",function(x)xmlGetAttr(x,"count"))[[1]]})
	fjname<-sapply(1,function(i){paste(unlist(xpathApply(x,"./ancestor::Population",function(x)xmlGetAttr(x,"name"))),collapse="/")});
	message("Boolean Gate",nm,"\n")
	g<-graph::addNode(nm,g)
	g<-graph::addEdge(attachto,nm,g)
	nodeData(g,nm,"metadata")<-new.env(parent=emptyenv());
	assign("isBooleanGate",TRUE,env=nodeData(g,nm,"metadata")[[1]])
	struct<-list(.parseBooleanGate(def,gnames,x))
	attr(struct,"type")<-"BooleanGate";
	assign("gate",struct,env=nodeData(g,nm,"metadata")[[1]])
	assign("count",counts,env=nodeData(g,nm,"metadata")[[1]]);
	assign("fjName",fjname,env=nodeData(g,nm,"metadata")[[1]]);
	#20110314
	assign("isGated",FALSE,env=nodeData(g,nm,"metadata")[[1]])
	
	#Don't have these yet.. get them later.
	#assign("parentTot",NA,env=nodeData(g,nm,"metadata")[[1]])
	#assign("thisTot",NA,env=nodeData(g,nm,"metadata")[[1]])
	#assign("thisIndices",list(),env=nodeData(g,nm,"metadata")[[1]])
	return(g);
}
#Parses all gates in a sample, in a loop
# .parseBooleanGates_deprecated<-function(x,g){
# 		allgates<-xpathApply(x,".//BooleanGate")
# 		if(length(allgates)==0){
# 			return(g);
# 		}
# 		def<-lapply(allgates,function(x)xmlGetAttr(x,"specification"))
# 		#Get the gate names
# 		## generate a query based on the full population name
# 		gnames<-lapply(allgates,function(x)unlist(xpathApply(x,".//String",function(x)(xmlValue(x)))))
# 		nm<-lapply(allgates,function(x)xpathApply(x,"./parent::Population",function(x){nm<-xmlGetAttr(x,"name");nn<-xmlGetAttr(x,"nn");paste(nn,nm,sep=".");})[[1]])
# 		attachto<-lapply(allgates,function(x){p<-xpathApply(x,"./parent::Population/parent::Population",xmlAttrs)[[1]];paste(p["nn"],".",p["name"],sep="")})
# 		counts<-lapply(allgates,function(x){xpathApply(x,"./parent::Population",function(x)xmlGetAttr(x,"count"))[[1]]})
# 		fjname<-lapply(allgates,function(x){paste(unlist(xpathApply(x,"./ancestor::Population",function(x)xmlGetAttr(x,"name"))),collapse="/")});
# 		for(i in 1:length(attachto)){
# 			print(nm[[i]])
# 			g<-graph::addNode(nm[[i]],g)
# 			g<-graph::addEdge(attachto[[i]],nm[[i]],g)
# 			nodeData(g,nm[[i]],"metadata")<-new.env(parent=emptyenv());
# 			assign("isBooleanGate",TRUE,env=nodeData(g,nm[[i]],"metadata")[[1]])
# 			struct<-list(.parseBooleanGate(def[[i]],gnames[[i]],allgates[[i]]))
# 			attr(struct,"type")<-"BooleanGate";
# 			assign("gate",struct,env=nodeData(g,nm[[i]],"metadata")[[1]])
# 			assign("count",counts[[i]],env=nodeData(g,nm[[i]],"metadata")[[1]]);
# 			assign("fjName",fjname[[i]],env=nodeData(g,nm[[i]],"metadata")[[1]]);
# 			assign("parentTot",NA,env=nodeData(g,nm[[i]],"metadata")[[1]])
# 			assign("thisTot",NA,env=nodeData(g,nm[[i]],"metadata")[[1]])
# 			assign("thisIndices",list(),env=nodeData(g,nm[[i]],"metadata")[[1]])
# 			
# 		}
# 		##For each Boolean gate.. get the siblings on which it acts.
# 		##Put together an expression or call to calculate the gate 
# 		
# 		##return an update tree structure
# 		return(g);
# }
setMethod("[",signature("GatingSet"),function(x,i,j,...,drop){
	x@set<-x@set[i]
	x@metadata<-x@metadata[i]
	return(x);
})
setMethod("[[",signature("GatingSet"),function(x,i,j,...){
	return(x@set[[i]]);
})
setReplaceMethod("[[",signature("GatingSet",value="GatingHierarchy"),function(x,i,j,...,value){
	if(length(i)!=1){
		stop("subscript out of bounds (index must have length 1)");
	}
	x@set[[i]]<-value
	return(x)
})
## Test this method.. not sure if it works correctly.
setReplaceMethod("[",signature("GatingSet",value="GatingSet"),function(x,i,j,...,value){
	x@set[i]<-value@set
	pData(x)[i,]<-pData(value)[i,]
	return(x);
})
setMethod("length","GatingSet",function(x){
	length(x@set);
})
setMethod("lapply","GatingSet",function(X,FUN,...){
	##return a GatingSet unless the function FUN is not suitable, in which case return the list.
	r<-lapply(X@set,FUN,...)
	result<-try(new("GatingSet",set=r),silent=TRUE);
	if(inherits(result,"try-error")){
		result<-r;
	}else{
		result@metadata<-X@metadata;
	}
	result;
})

setMethod("show","GatingSet",function(object){
	cat("A GatingSet with ",length(object), " samples\n")
	for(i in 1:length(object)){
		cat(i,". ");
		show(object[[i]])
	}
})

setMethod("pData","GatingSet",function(object){
	pData(object@metadata)
})
setReplaceMethod("pData",c("GatingSet","data.frame"),function(object,value){
	pData(object@metadata)<-value;
	return(object);
})

setMethod("show","GatingHierarchy",function(object){
	cat("\tFCS File: ",object@name,"\n");
	cat("\tGatingHierarchy with ",length(object@nodes)," gates\n");
})
setMethod("getNodes","GatingHierarchy",function(x,tsort=FALSE,...){
	if(!tsort){
		return(x@nodes)
	}else{
		return(RBGL::tsort(x@tree))
	}
})

setMethod("execute",signature(hierarchy="GatingHierarchy"),function(hierarchy,cleanup=FALSE,keep.indices=TRUE,isNcdf=FALSE,ncfs=NULL,dataEnvironment=NULL,...){
		##Conditional compilation for HAVE_NETCDF
		if(isNcdf&!TRUE){
		stop("isNcdf must be FALSE, since you don't have netcdf installed");
		}
		if(hierarchy@flag){
			message("This file is already gated\n")
			return()
		}
		
		x<-hierarchy@tree;
		
		#doc<-workspace@doc
		nlist<-RBGL::bfs(x,nodes(x)[1]);
		file<-getSample(hierarchy,isFullPath=TRUE)

#		file<-get("fcsfile",env=nodeData(x,nlist[1],"metadata")[[1]])
#		if(is.null(path)){
#			stop("You need to include the path to the files")
#		}
#		file<-list.files(pattern=file,path=path,recursive=T,full=T)
#		if(length(file)==0){
#			stop("Can't find ",get("fcsfile",env=nodeData(x,nlist[1],"metadata")[[1]])," in directory: ",path,"\n");
#		}
		message("Loading data file: ",file);
		data<-read.FCS(file);

		cid<-get("compID",env=nodeData(x,nlist[1],"metadata")[[1]])
		cal<-hierarchy@transformations;

		if(cid!=-1 & cid!=-2){
			message("Compensating");
			#compobj<-compensation(.getCompensationMatrices(doc)[[as.numeric(cid)]])
			compobj<-compensation(hierarchy@compensation);
			#TODO this compensation will fail if the parameters have <> braces (meaning the data is stored compensated).
			#I need to handle this case properly.
			res<-try(compensate(data,compobj),silent=TRUE)
			if(inherits(res,"try-error")){
				message("Data is probably stored already compensated");
			}else{
				data<-res
				rm(res);
				gc(reset=TRUE);
			}
			cnd<-colnames(data)
			if(is.null(cnd)){cnd<-as.vector(parameters(data)@data$name)}
			wh<-cnd%in%parameters(compobj)
			cnd[wh]<-paste("<",parameters(compobj),">",sep="")
			
			#colnames(data)<-cnd;
			e<-exprs(data)
			d<-description(data);
			p<-parameters(data);
			p@data$name<-cnd
			colnames(e)<-cnd;
			data<-new("flowFrame",exprs=e,description=d,parameters=p)						
			#cmnm<-names(.getCompensationMatrices(doc))[as.numeric(cid)]
			#Don't need this anymore.
			# #cal<-.getCalibrationTableSearch(doc,cmnm)
			# if(length(cal)==0){
			# 	stop("Can't find the calibration table named : ", cmnm, " in this workspace.");
			# }
		}
		else if(cid==-2){
			#TODO the matrix may be acquisition defined.
			message("No compensation");
		}
		else if(cid==-1){
			##Acquisition defined compensation.
			#nm<-unique(do.call(c,lapply(strsplit(.getCalibrationTableNames(doc)," "),function(x)x[1])))[2]
			nm<-hierarchy@compensation;
			if(grepl("Acquisition-defined",nm)){
				###Code to compensate the sample using the acquisition defined compensation matrices.
				message("Compensating with Acquisition defined compensation matrix");
				#browser()
				#TODO make sure we don't compensate data that's stored compensated and that we don't throw an error either.
				comp<-compensation(spillover(data)$SPILL)
				hierarchy@compensation<-spillover(data)$SPILL
				res<-try(compensate(data,comp),silent=TRUE)
				if(inherits(res,"try-error")){
					message("Data is probably stored already compensated");
				}else{
					data<-res
					rm(res);
					gc(reset=TRUE);
				}
				cnd<-colnames(data)
				wh<-cnd%in%parameters(comp)
				cnd[wh]<-paste("<",parameters(comp),">",sep="")
				e<-exprs(data)
				d<-description(data);
				p<-parameters(data);
				p@data$name<-cnd
				colnames(e)<-cnd;
				data<-new("flowFrame",exprs=e,description=d,parameters=p)
				gc(reset=TRUE);
			}
			#Ditto
			#cal<-.getCalibrationTableSearch(doc,nm)
		}
		gc(reset=TRUE)
		message("Transforming");
		axis.labels<-list();
		.flowJoTransform(environment(),cal);
		gc(reset=TRUE);
		#wh<-which(data@parameters@data$name=="Time")
		wh<-grep("^Time$",data@parameters@data$name)
		if(length(wh!=0)){
		gc(reset=TRUE);
		parameters(data)@data[wh,4:5]<-range(exprs(data[,wh]));
		gc(reset=TRUE);
		parameters(data)@data[wh,3]<-diff(range(exprs(data[,wh])));
		}
		gc(reset=TRUE);
		# if(!isNcdf){
			e<-new.env(parent=.GlobalEnv);
		# }

		#########################################
		#if use ncdfFlowSet,then add the transformed
		#matrix to the ncdf file,assign it to env
		#########################################
		if(!isNcdf)
			assign("data",data,envir=e)
		else
		{
			sampleName<-getSample(hierarchy)
			.addSlice(dataEnvironment$ncfs,data,sampleName)
			###upate the colnames due to the compenstation
			dataEnvironment$ncfs@parameters[[sampleName]]<-data@parameters
			#ncfs@parameters[[sampleName]]@data$name<-colnames(data)
			assign("data",dataEnvironment,envir=e)
			assign("sampleName",sampleName,envir=e)
		}
		assign("axis.labels",axis.labels,envir=e);
		rm(data);
		gc(reset=TRUE)
		nodeDataDefaults(x,"data")<-e;
		##The data below needs to be added to metadata for each node.. probably best done in parseWorkspace
		##These are now assigned in parseWorkspace. Comment out.
		# nodeDataDefaults(x,"thisIndices")<-list();
		# nodeDataDefaults(x,"parentTot")<-NA;
		# nodeDataDefaults(x,"thisTot")<-0;
		#Nodes to parse later
		skipforlater<-list();
		lastparent<-nlist[1];
		for (node in nlist){
			#20110314 check if the current node has already been gated
			if(.isGated.graphNEL(hierarchy,node))
				next
			else
			{
				parentname<-(setdiff(adj(ugraph(x),node)[[1]],adj(x,node)[[1]]));
				if(length(parentname)==0){
					parentname<-node
					
					assign("thisTot",as.numeric(nodeData(x,node,"metadata")[[1]][["count"]]),env=nodeData(x,node,"metadata")[[1]]);
					hierarchy@tree<-x;
					assign("thisIndices",list(getIndices(hierarchy,node),env=nodeData(x,node,"metadata")[[1]]));
					#20110314 set the flag after gating
					assign("isGated",TRUE,env=nodeData(x,node,"metadata")[[1]])
					lastparent<-parentname;
					next;
				};
				if(lastparent!=parentname&length(skipforlater)!=0){
					hierarchy@tree<-x;
					for(i in skipforlater){
#						browser()
						
						hierarchy<-.calcBooleanGate(hierarchy,i)
					}
					skipforlater<-list();
					lastparent<-parentname;
				}
				hierarchy@tree<-x;
				if(.isBooleanGate.graphNEL(hierarchy,node)){
					#Skip Boolean Gates and Compute them later
					skipforlater<-c(skipforlater,node);
					lastparent<-parentname;
					next;
				}
				#gating the regular gate
				.calcGate(hierarchy,node)
				lastparent<-parentname;
			}
		}
		if(length(skipforlater)!=0){
			hierarchy@tree<-x;
			for(i in skipforlater){
				hierarchy<-.calcBooleanGate(hierarchy,i)
			}
			skipforlater<-list();
		}
			
			
			
			
			
		# 	a<-adj(x,node)[[1]];
		# 	for (ai in a){
		# 		#Skip Boolean Gates and compute them later. (or maybe on the fly)
		# 		if(.isBooleanGate.graphNEL(hierarchy,ai)){
		# 			next
		# 		}
		# 		g<-get("gate",env=nodeData(x,ai,"metadata")[[1]]);
		# 		#Need parent node name
		# 		parentname<-(setdiff(adj(ugraph(x),ai)[[1]],adj(x,ai)[[1]]))
		# 		if(parentname!=nlist[1]){
		# 			#If the parent is not the root.
		# 			##get the parent indices vector and AND with the current vector. Store the result
		# 			message("Gating ", ai);
		# 			
		# 			if(get("negated",env=nodeData(x,ai,"metadata")[[1]])){
		# 				l<-list(get("thisIndices",env=nodeData(x,parentname,"metadata")[[1]])[[1]]&(!filter(nodeData(x,ai,"data")[[1]][["data"]],get("gate",nodeData(x,ai,"metadata")[[1]]))@subSet))
		# 				assign("thisIndices",l,env=nodeData(x,ai,"metadata")[[1]])						
		# 			}else{
		# 				l<-list(get("thisIndices",env=nodeData(x,parentname,"metadata")[[1]])[[1]]&(filter(nodeData(x,ai,"data")[[1]][["data"]],get("gate",env=nodeData(x,ai,"metadata")[[1]]))@subSet))
		# 				
		# 				assign("thisIndices",l,env=nodeData(x,ai,"metadata")[[1]])
		# 				
		# 			}
		# 			l<-ifelse(is.na(table(get("thisIndices",env=nodeData(x,ai,"metadata")[[1]]))["TRUE"]),0,table(get("thisIndices",env=nodeData(x,ai,"metadata")[[1]]))["TRUE"])
		# 			assign("thisTot",l,env=nodeData(x,ai,"metadata")[[1]])
		# 			l<-get("thisTot",env=nodeData(x,parentname,"metadata")[[1]])
		# 			assign("parentTot",l,env=nodeData(x,ai,"metadata")[[1]])
		# 			##Check if there's any siblings left 
		# 			if(!any(unlist(sapply(unlist(adj(x,parentname)),function(b)is.na(get("parentTot",env=nodeData(x,b,"metadata")[[1]])))))){
		# 				#Do we keep indices?
		# 				if(!keep.indices){
		# 					assign("thisIndices",list(),env=nodeData(x,parentname,"metadata")[[1]])
		# 					gc(reset=T)
		# 				}
		# 			}
		# 			##Check if it has children.. otherwise remove the index also
		# 			if(length(adj(x,ai)[[1]])==0){
		# 				if(!keep.indices){
		# 					assign("thisIndices",list(),env=nodeData(x,ai,"metadata")[[1]])
		# 					gc(reset=T)
		# 				}
		# 			}
		# 		}
		# 		else{
		# 			#if the parent is the root
		# 			message("Gating ", ai);
		# 			
		# 			#Indices is just the filter subset
		# 			l<-list(filter(nodeData(x,ai,"data")[[1]][["data"]],get("gate",env=nodeData(x,ai,"metadata")[[1]]))@subSet)
		# 			assign("thisIndices",l,env=nodeData(x,ai,"metadata")[[1]])
		# 			#Parent total is all events
		# 			l<-length(get("thisIndices",env=nodeData(x,ai,"metadata")[[1]])[[1]]);
		# 			assign("parentTot",l,env=nodeData(x,ai,"metadata")[[1]])
		# 			#Current total is the contingency table of the subset
		# 			l<-table(get("thisIndices",env=nodeData(x,ai,"metadata")[[1]]))["TRUE"];	
		# 			assign("thisTot",l,env=nodeData(x,ai,"metadata")[[1]])	
		# 			##Update parent to the computed counts rather than the defaults.
		# 			l<-get("parentTot",env=nodeData(x,ai,"metadata")[[1]]);
		# 			assign("thisTot",l,env=nodeData(x,parentname,"metadata")[[1]])
		# 			l<-get("parentTot",env=nodeData(x,ai,"metadata")[[1]]);
		# 			assign("parentTot",l,nodeData(x,parentname,"metadata")[[1]])
		# 		}
		# 	}
		# }
		# ###Once we're done gating, clean up the data if cleanup==TRUE
		if(cleanup){
			nodeDataDefaults(x,"data")<-new.env(parent=.GlobalEnv);
			gc(reset=TRUE);
		}
		hierarchy@tree<-x;
		# message("Computing Boolean Gates");
		# hierarchy<-.calcBooleanGates(hierarchy);
		hierarchy@flag<-TRUE;
		hierarchy
})

#20110314
.calcGate<-function(hierarchy,node){
#	browser()
	parentname<-getParent(hierarchy,node)
	x<-hierarchy@tree
	message("Gating ", node);
	g<-get("gate",env=nodeData(x,node,"metadata")[[1]]);
	#			browser()
	if(exists("negated",env=nodeData(x,node,"metadata")[[1]])){
		if(get("negated",env=nodeData(x,node,"metadata")[[1]])){
			hierarchy@tree<-x;
			
			
			l<-list(getIndices(hierarchy,parentname)&(!filter(getData(x),get("gate",nodeData(x,node,"metadata")[[1]]))@subSet))
			assign("thisIndices",l,env=nodeData(x,node,"metadata")[[1]])						
		}else{
			hierarchy@tree<-x;
			l<-list(getIndices(hierarchy,parentname)&(filter(getData(x),get("gate",env=nodeData(x,node,"metadata")[[1]]))@subSet))
			assign("thisIndices",l,env=nodeData(x,node,"metadata")[[1]])
		}
	}else{
		hierarchy@tree<-x;
		l<-list(getIndices(hierarchy,parentname)&(filter(getData(x),get("gate",env=nodeData(x,node,"metadata")[[1]]))@subSet))
		assign("thisIndices",l,env=nodeData(x,node,"metadata")[[1]])
	}
	l<-ifelse(is.na(table(get("thisIndices",env=nodeData(x,node,"metadata")[[1]]))["TRUE"]),0,table(get("thisIndices",env=nodeData(x,node,"metadata")[[1]]))["TRUE"])	
	assign("thisTot",l,env=nodeData(x,node,"metadata")[[1]])
	l<-get("thisTot",env=nodeData(x,parentname,"metadata")[[1]])
	assign("parentTot",l,env=nodeData(x,node,"metadata")[[1]])
	#20110314 set the flag after gating
	assign("isGated",TRUE,env=nodeData(x,node,"metadata")[[1]])
}
.calcBooleanGate<-function(x,y){
	message("Gating BooleanGate ",y, "\n");
	z<-table(getIndices(x,y))["TRUE"]
	z<-ifelse(is.na(z),0,z);
	assign("thisTot",z,env=nodeData(x@tree,y,"metadata")[[1]]);
	z<-table(getIndices(x,getParent(x,y)))["TRUE"]
	z<-ifelse(is.na(z),0,z)
	assign("parentTot",z,env=nodeData(x@tree,y,"metadata")[[1]]);
	#20110314 set the flag after gating
	assign("isGated",TRUE,env=nodeData(x@tree,y,"metadata")[[1]]);
	return(x);
}

setMethod("plot",signature("GatingHierarchy","missing"),function(x,y,layout="dot",width=3,height=2,fontsize=14,labelfontsize=14,fixedsize=FALSE,boolean=FALSE,...){
	if(!boolean){	
			sub<-subGraph(x@nodes[which(!unlist(lapply(nodeData(x@tree,x@nodes,"metadata"),function(x)get("isBooleanGate",env=x))))],x@tree)
	}else{
		sub<-x@tree
	}
		nn<-sapply(nodes(sub),function(x)strsplit(x,"\\.")[[1]][2])
		nn[1]<-nodes(sub)[1]
		natr<-list();
		natr$label<-nn;
		options("warn"=-1)
		renderGraph(Rgraphviz::layoutGraph(sub,layoutType=layout,attrs=list(graph=list(rankdir="LR",page=c(8.5,11)),node=list(fixedsize=FALSE,fontsize=fontsize,shape="rectangle"))))
		#plot(sub,nodeAttrs=natr,attrs=list(node=list(fixedsize=fixedsize,labelfontsize=labelfontsize,fontsize=fontsize,width=width,height=height,shape="rectangle")),y=layout,...);
		options("warn"=0)
})

setMethod("plotGate",signature(x="GatingHierarchy",y="numeric"),function(x,y,add=FALSE,border="red",tsort=FALSE,...){
	node<-getNodes(x,tsort=tsort)[y];
	if(is.na(node)){
		warning("Can't plot gate ", y, " doesn't exist.");
		return(1);
	}
	plotGate(x,y=node,add=add,border=border,tsort=tsort,...);
})

setMethod("plotGate",signature(x="GatingHierarchy",y="character"),function(x,y,add=FALSE,border="red",tsort=FALSE,...){
	#TODO fix plotting of rectangleGates that are one dimensional
	
		if(!x@flag){
				message("Can't plot until you gate the data with 'execute()'\n");
			return();
		}
		##Two cases: gate is boolean, or gate is normal
		##Boolean gates are treated differently
		if(.isBooleanGate.graphNEL(x,y)){
			p<-getParent(x,y);
			ind<-getIndices(x,y)
			ind.p<-getIndices(x,p)
			pd<-getData(x,p)
			dim.ind<-getDimensions(x,p,index=TRUE)
			dims<-getDimensions(x,p);
			dims<-dims[na.omit(match((getData(x,y)@parameters@data$name),dims))]
			if(add){
				points(exprs(pd[,dims]),col=as.numeric(ind[ind.p])+1,pch='.');
			}else{
				plot((pd[,dims]),col=as.numeric(ind[ind.p])+1,smooth=FALSE);
			}
			invisible()
		}else if(suppressWarnings(is.na(getGate(x,y)))){
			message("Can't plot. There is no gate defined for node ",y);
			invisible();			
		}else{
			if(add==FALSE){
			dims<-getDimensions(x,y)
			dims2<-dims[na.omit(match((getData(x,y,tsort=tsort)@parameters@data$name),dims))]
			dim.ind<-getDimensions(x,y,index=TRUE)[na.omit(match((getData(x,y,tsort=tsort)@parameters@data$name),dims))]
			if(is.null(getAxisLabels(x)[[dim.ind[1]]])&is.null(getAxisLabels(x)[[dim.ind[2]]])){
				flowViz:::fplot(getData(x,getParent(x,y))[,dims2],smooth=FALSE,...)
			}else if(!is.null(getAxisLabels(x)[[dim.ind[1]]])&is.null(getAxisLabels(x)[[dim.ind[2]]])){
				
				flowViz:::fplot(getData(x,getParent(x,y))[,dims2],smooth=FALSE,axes=FALSE,frame.plot=TRUE,xlim=range(getAxisLabels(x)[[dim.ind[1]]]$at),...)
				axis(side=1,at=getAxisLabels(x)[[dim.ind[1]]]$at,labels=getAxisLabels(x)[[dim.ind[1]]]$label);
			}else if(is.null(getAxisLabels(x)[[dim.ind[1]]])&!is.null(getAxisLabels(x)[[dim.ind[2]]])){
				
				flowViz:::fplot(getData(x,getParent(x,y))[,dims2],smooth=FALSE,axes=FALSE,frame.plot=TRUE,ylim=range(getAxisLabels(x)[[dim.ind[2]]]$at),...)
				axis(side=2,at=getAxisLabels(x)[[dim.ind[2]]]$at,labels=getAxisLabels(x)[[dim.ind[2]]]$label)	
			}else if(!is.null(getAxisLabels(x)[[dim.ind[1]]])&!is.null(getAxisLabels(x)[[dim.ind[2]]])){
				
				flowViz:::fplot(getData(x,getParent(x,y))[,dims2],smooth=FALSE,axes=FALSE,frame.plot=TRUE,xlim=range(getAxisLabels(x)[[dim.ind[1]]]$at),ylim=range(getAxisLabels(x)[[dim.ind[2]]]$at),...)
				axis(side=1,at=getAxisLabels(x)[[dim.ind[1]]]$at,labels=getAxisLabels(x)[[dim.ind[1]]]$label);
				axis(side=2,at=getAxisLabels(x)[[dim.ind[2]]]$at,labels=getAxisLabels(x)[[dim.ind[2]]]$label)	
			}
		}
		dims<-colnames(getBoundaries(x,y))
		dims<-dims[na.omit(match((getData(x,y,tsort=tsort)@parameters@data$name),dims))]
		#Case for rectangle or polygon gate
		if(length(dims)>1){
			polygon(getBoundaries(x,y)[,dims],border=border,...);
		}else{
			apply(getBoundaries(x,y)[,dims,drop=FALSE],1,function(x)abline(v=x,col="red"))
		}
	}
})

setMethod("getPopStats","GatingHierarchy",function(x,...){
	if(!x@flag){
		message("Can't extract population statistics until the data has been gated with 'execute()'\n");
		return()
	}
	
 m<-do.call(rbind,(sapply(RBGL::tsort(x@tree),function(y)list(c(get("fjName",env=nodeData(x@tree,y,"metadata")[[1]]),getProp(x,y),get("count",env=nodeData(x@tree,y,"metadata")[[1]]),get("thisTot",env=nodeData(x@tree,y,"metadata")[[1]]),get("parentTot",env=nodeData(x@tree,y,"metadata")[[1]]),y)))))
	###Fix for root node. Should be fixed in addDataToGatingHierarchy
	rownames(m)<-NULL;
	m<-data.frame(m);
	m[,2]<-as.numeric(as.character(m[,2]));
	m[,3]<-as.numeric(as.character(m[,3]));
	m[,4]<-as.numeric(as.character(m[,4]));
	m[,5]<-as.numeric(as.character(m[,5]))
	m[,6]<-as.character(m[,6])
	
	#m[1,4]<-m[1,3]
	m[1,c(2)]<-1;
	m[1,5]<-m[1,4]
	colnames(m)<-c("pop.name","flowCore.freq","flowJo.count","flowCore.count","parent.total","node")
	rownames(m)<-m[,1]
	m<-m[,2:6]
	m
})

setMethod("plotPopCV","GatingHierarchy",function(x,m=2,n=2,...){
	x<-getPopStats(x)
	cv<-apply(as.matrix(x[,2:3]),1,function(y)IQR(y)/median(y));
	cv<-as.matrix(cv,nrow=length(cv))
	cv[is.nan(cv)]<-0
	rownames(cv)<-rownames(x);
	return(barchart(cv,xlab="Coefficient of Variation",...));
})

setMethod("plotPopCV","GatingSet",function(x,...){
#columns are populations
#rows are samples
cv<-do.call(rbind,lapply(lapply(G,getPopStats),function(x)apply(x[,2:3],1,function(x){cv<-IQR(x)/median(x);ifelse(is.nan(cv),0,cv)})))
#flatten, generate levels for samples.
nr<-nrow(cv)
nc<-ncol(cv)
populations<-gl(nc,nr,labels=colnames(cv))
samples<-as.vector(t(matrix(gl(nr,nc,labels=rownames(cv)),nrow=nc)))
cv<-data.frame(cv=as.vector(cv),samples=samples,populations=populations)
return(barchart(cv~populations|samples,cv,...,scale=list(x=list(...))));
})

setMethod("getAxisLabels",signature(obj="GatingHierarchy",y="missing"),function(obj,y=NULL,...){
	get("axis.labels",env=nodeData(obj@tree)[[1]]$data)
})



setMethod("getData",signature(obj="graphNEL"),function(obj,y=NULL,tsort=FALSE){
			y<-nodes(obj)[1];
			r<-nodeData(obj,y,"data")[[1]][["data"]]
			if(class(r)=="environment")
				r<-.getFlowFrame(r$ncfs,getSample(obj))
			r
		})

setMethod("getData",signature(obj="GatingHierarchy"),function(obj,y=NULL,tsort=FALSE){
	if(!obj@flag){
		stop("Must run execute() before fetching data");
	}  
	if(is.null(y)){
		r<-nodeData(obj@tree,getNodes(obj,tsort=tsort)[1],"data")[[1]][["data"]]
		if(class(r)=="environment")
			r<-.getFlowFrame(r$ncfs,getSample(obj))
	}else if((y==getNodes(obj)[1])){
		r<-nodeData(obj@tree,getNodes(obj,tsort=tsort)[1],"data")[[1]][["data"]]
		if(class(r)=="environment")
			r<-.getFlowFrame(r$ncfs,getSample(obj))
	}else if(is.numeric(y)){
		n<-getNodes(obj,tsort=tsort)[y]
		r<-getData(obj,n,tsort=tsort);
	}else{
		r<-nodeData(obj@tree,y,"data")[[1]][["data"]]
		if(class(r)=="environment")
			r<-.getFlowFrame(r$ncfs,getSample(obj))	
		r<-r[getIndices(obj,y),]
	}
	r
})
setMethod("getData",signature(obj="GatingSet"),function(obj,y=NULL,tsort=FALSE){
	if(is.null(y)){
		return(flowSet(lapply(obj,function(x)getData(x,tsort=tsort))))
	}else if(is.numeric(y)){
		return(flowSet(lapply(obj,function(x)getData(x,y,tsort=tsort))))
	}else{
		stop("Invalid value for y. Must be class \"numeric\"");
	}
})
setMethod("getKeywords",signature("flowJoWorkspace","character"),function(obj,y){
	w <- which(xpathApply(obj@doc,"/Workspace/SampleList/Sample/Keywords/Keyword[@name='$FIL']",function(x)xmlGetAttr(x,"value"))%in%y)
	l<-xpathApply(obj@doc,paste("/Workspace/SampleList/Sample[",w,"]/Keywords/node()",sep=""),xmlAttrs)
	names(l)<-lapply(l,function(x)x[["name"]])
	l<-lapply(l,function(x)x[["value"]])
	return(l);
})

.getKeywords<-function(doc,y){
	w<-which(xpathApply(doc,"/Workspace/SampleList/Sample/Keywords/Keyword[@name='$FIL']",function(x)xmlGetAttr(x,"value"))%in%y)
	if(length(w)==0){
		warning("Sample ",y," not found in Keywords");
		##Use the DataSet tag to locate the sample
		w<-which(xpathApply(doc,"/Workspace/SampleList/Sample/DataSet",function(x)xmlGetAttr(x,"uri"))%in%y)
	}
	l<-xpathApply(doc,paste("/Workspace/SampleList/Sample[",w,"]/Keywords/node()",sep=""),xmlAttrs)
	names(l)<-lapply(l,function(x)x[["name"]])
	l<-lapply(l,function(x)x[["value"]])
	return(l)
}

setMethod("getKeywords",signature("GatingHierarchy","missing"),function(obj,y){
	get("keywords",env=nodeData(obj@tree)[[1]]$metadata);
})
#Return the list of keywords given a GatingSet and a sample name
setMethod("getKeywords",signature("GatingSet","character"),function(obj,y){
	ind<-which(getSamples(obj)%in%y)
	if(length(ind)>0){
		getKeywords(obj,ind);
	}else{
		stop(paste("Sample ",y," not in GatingSet",sep=""));
	}
})
setMethod("getKeywords",signature("GatingSet","numeric"),function(obj,y){
	if(length(obj)<y){
		stop("index out of range");
	}else{
		getKeywords(obj[[y]]);
	}
})
#Return the value of the keyword given a flowWorkspace and the keyword name
setMethod("keyword",signature("GatingHierarchy","character"),function(object,keyword){
	kw<-as.environment(getKeywords(object))
	mget(keyword,kw);
})
setMethod("keyword",signature("GatingSet","character"),function(object,keyword){
	data.frame(unlist(lapply(object,function(x)keyword(x,keyword)),use.names=FALSE));
})
setMethod("getGate",signature(obj="GatingHierarchy",y="character"),function(obj,y){
	if(.isBooleanGate.graphNEL(obj,y)){
		g<-get("gate",env=nodeData(obj@tree,y,"metadata")[[1]])[[1]];
		# p<-g$ref
		# 		if(length(ref)==1){
		# 			return(ref);
		# 		}else{
		# 	 		p<-paste(g$v,p,sep="")
		# 			p<-paste(p,c(g$v2,""),sep="")
		# 			return(paste(p,collapse=""))
		# 		}
		return(g)
	}else{
		return(get("gate",env=nodeData(obj@tree,y,"metadata")[[1]]))
	}
})
#return gate y for a given hierarchy (by index)
setMethod("getGate",signature(obj="GatingHierarchy",y="numeric"),function(obj,y,tsort=FALSE){
	n<-getNodes(obj,tsort=tsort)[y]
	if(flowWorkspace:::.isBooleanGate.graphNEL(obj,n)){
		g<-get("gate",env=nodeData(obj@tree,n,"metadata")[[1]])[[1]]
		# p<-g$ref
		# 		p<-paste(g$v,p,sep="")
		# 		p<-paste(p,c(g$v2,""),sep="")
		# 		return(paste(p,collapse=""))
		return(g);
	}else{
		return(get("gate",env=nodeData(obj@tree,n,"metadata")[[1]]))
	}
})
#Return gate y for all samples in the gating set (by index).
#Warning: assume that all gating hierarchies are the same in the given gating set. We don't check for this, so be careful.
setMethod("getGate",signature(obj="GatingSet",y="numeric"),function(obj,y,tsort=FALSE){
	lapply(obj,function(x)getGate(x,y,tsort=tsort))
})
setMethod("getParent",signature(obj="GatingHierarchy",y="numeric"),function(obj,y,tsort=FALSE){
	return(match(getParent(obj,getNodes(obj,tsort=tsort)[y]),getNodes(obj,tsort=tsort)));
})
setMethod("getParent",signature(obj="GatingHierarchy",y="character"),function(obj,y){
	setdiff(adj(ugraph(obj@tree),y)[[1]],adj(obj@tree,y)[[1]])
})
setMethod("getBoundaries",signature(obj="GatingHierarchy",y="character"),function(obj,y){
	g<-getGate(obj,y);
	if(length(g@parameters)==1){
		rbind(g@min,g@max)
	}else{
		g@boundaries
	}
})
setMethod("getDimensions",signature(obj="GatingHierarchy",y="character"),function(obj,y,index=FALSE){
	if(.isBooleanGate.graphNEL(obj,y)){
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
					c(match(getGate(obj,y)@parameters[[1]]@parameters,parameters(getData(obj,y))@data$name))	
				}else{
					c(match(getGate(obj,y)@parameters[[1]]@parameters,parameters(getData(obj,y))@data$name),match(getGate(obj,y)@parameters[[2]]@parameters,parameters(getData(obj,y))@data$name))
				}
			}
	}
})
setMethod("getIndices",signature(obj="GatingHierarchy",y="character"),function(obj,y){
	if(.isBooleanGate.graphNEL(obj,y)){
		g<-get("gate",env=nodeData(obj@tree,y,"metadata")[[1]])[[1]]
		if(length(g$ref)==1){
			p<-paste("getIndices(obj,\"",g$ref,"\")",sep="")
			p<-paste(g$v,p,sep="")
	 		parent<-getParent(obj,y)
			p<-c("getIndices(obj,parent)&(",p,")");
			return(eval(parse(text=paste(p,collapse=""))))
		}else{
			p<-paste("getIndices(obj,\"",g$ref,"\")",sep="")
	 		p<-paste(g$v,p,sep="")
			p<-paste(p,c(g$v2,""),sep="")
			parent<-getParent(obj,y)
			p<-c("getIndices(obj,parent)&(",p,")");
			return(eval(parse(text=paste(p,collapse=""))))
		}
	}else{
		if(y==getNodes(obj)[1]){
			return(rep(TRUE,nodeData(obj@tree,y,"metadata")[[1]][["thisTot"]]))
		}else{
			#20110314 if not gated yet,then do the gating first 
			if(!.isGated.graphNEL(obj,y))
			{
				.calcGate(obj,y)
			}
			return(get("thisIndices",env=nodeData(obj@tree,y,"metadata")[[1]])[[1]])
		}
	}
})

setMethod("getChildren",signature(obj="GatingHierarchy",y="character"),function(obj,y){
	adj(obj@tree,y)[[1]]
})

setMethod("getProp",signature(x="GatingHierarchy",y="character"),function(x,y){
	# if(.isBooleanGate.graphNEL(x,y)){
	# 		z<-table(getIndices(x,y))["TRUE"]
	# 		return(ifelse(is.na(z),0,z)/table(getIndices(x,getParent(x,y)))["TRUE"])
	# 	}else{
	#Return the proportion of the population relative to the parent and relative to the total.
	#x is a graph of a gating hierarchy that has had data added to it.
	#y is nodename
	
		return(get("thisTot",env=nodeData(x@tree,y,"metadata")[[1]])/get("parentTot",env=nodeData(x@tree,y,"metadata")[[1]]))
	# }
	
})
setMethod("getTotal",signature(x="GatingHierarchy",y="character"),function(x,y){
		return(get("thisTot",env=nodeData(x@tree,y,"metadata")[[1]]))
})


.getGains<-function(x){
	##Sometimes the gains are not in the keywords but in the Parameter section of the sample. Where do they come from? Only flowJo knows..
	n<-as.numeric(xmlGetAttr(xpathApply(x,"./ancestor::Sample/Keywords/Keyword[@name='$PAR']")[[1]],"value"));
	t(sapply(1:n,function(i){
		cn<-paste("$P",i,"N",sep="")
		cg<-paste("$P",i,"G",sep="");
		cnquery<-paste("./ancestor::Sample/Keywords/Keyword[@name='",cn,"']",sep="");
		cgquery<-paste("./ancestor::Sample/Keywords/Keyword[@name='",cg,"']",sep="");
		cname<-xpathApply(x,cnquery,function(q)xmlGetAttr(q,"value"))[[1]];
		cgain<-xpathApply(x,cgquery,function(q)xmlGetAttr(q,"value",default=NA));
		#Test if we found it
		if(length(cgain)!=0){
			cgain<-as.numeric(cgain[[1]])
		}else{
			#Otherwise look for the gain elsewhere.
			cgain<-xpathApply(x,paste("./ancestor::Sample/Parameter[",i,"]",sep=""),function(q)xmlGetAttr(q,"gain"));
			if(length(cgain)!=0){
				cgain<-as.numeric(cgain[[1]])
			}else{
				stop("Oops! Can't find the GAIN for parameter ",cname);
			}
		}
		c(cname,cgain);
	}))
}
# ===================================================================================
# = Convert rectangular to polar coordinates. Rows are data points, columns are x,y =
# ===================================================================================
.rectangular2polar<-function(points,center){
	dif<-t(t(points)-center);
	radians<-atan(dif[,2]/dif[,1])
	radius <- sqrt(apply(dif^2,1,sum))
	list(radius=radius,radians=radians)
}
# =============================
# = Given the angle, X, and Y =
# = X is rightmost, Y is 	  =
# + Y is leftmost point 	  =
# = Centered at 0,0           =
# =============================
.ellipse2Cov<-function(angle,Xdim,Ydim){	
	CxxI<-cos(angle)^2/Xdim^2+sin(angle)^2/Ydim^2;
	CyyI<-sin(angle)^2/Xdim^2+cos(angle)^2/Ydim^2;
	CxyI<-sin(angle)*cos(angle)*(1/Xdim^2-1/Ydim^2)
	C<-solve(matrix(c(CxxI,CxyI,CxyI,CyyI),ncol=2))
	C/qchisq(0.95,2);
}
# =======================================================
# = This function is adapted from the ellipse package.  =
# = Outputs bottom, top, right, and left most points on =
# = the ellipse given by the covariance matrix		    =
# = The level argument specifies the % contour.			=
# = scale should not be used.							=
# =======================================================
.cov2Ellipse<-function (x, scale = c(1, 1), centre = c(0, 0), level = 0.95, 
    t = sqrt(qchisq(level, 2)), which = c(1, 2),...) 
{
    names <- c("x", "y")
    if (is.matrix(x)) {
        xind <- which[1]
        yind <- which[2]
        r <- x[xind, yind]
        if (missing(scale)) {
            scale <- sqrt(c(x[xind, xind], x[yind, yind]))
            if (scale[1] > 0) 
                r <-r/scale[1]
            if (scale[2] > 0) 
                r <- r/scale[2]
        }
        if (!is.null(dimnames(x)[[1]])) 
            names <- dimnames(x)[[1]][c(xind, yind)]
    }
    else r <- x
    r <- min(max(r, -1), 1)
    d <- acos(r)
    a <- c(3/2*pi,pi/2,0,pi)
    matrix(c(t * scale[1] * cos(a + d/2) + centre[1], t * scale[2] * 
        cos(a - d/2) + centre[2]), 4, 2, dimnames = list(NULL, 
        names))
}
# =============================================================================
# = S4 method for converting and ellipsoid gate to vertices for flowJo export =
# =============================================================================
setMethod("ellipsoidGate2FlowJoVertices",signature(gate="ellipsoidGate"),function(gate,level=0.95,...){
	vertices<-.cov2Ellipse(gate@cov,level=level,centre=gate@mean);
	colnames(vertices)<-parameters(gate);
	vertices;
})
.ellipseFit<-function(x){
	if(all(dim(x)!=c(4,2))){
		stop("Coordinates of the ellipse gate are not as expected. Was expecting 4x2 matrix but got ",dim(x))
	}else{
		B<-x[1,];
		T<-x[2,];
		R<-x[3,];
		L<-x[4,];
		E<-c(norm(as.matrix(L-R),"F"),norm(as.matrix(T-B),"F"))/2
		m<-which.max(E);
		o<-order(E,decreasing=TRUE)
		v<-list(T,L)[[o]]
		phi<-acos(v[1]/norm(as.matrix(v),"F"))
		CY<-(B[2]+T[2])/2
		CX<-(R[1]+L[1])/2
		T<-seq(0,2*pi,l=100)
		X<-CX+E[1]*cos(T)*cos(phi)-E[2]*sin(T)*sin(phi);
		Y<-CY+E[1]*cos(T)*sin(phi)+E[2]*sin(T)*cos(phi);
		return(data.frame(x=X,y=Y));
	}
}
.booleanGate<-function(x,y){
	#X is a graph
	##Y is a list of nodes
	##Will output all possible combinations of the supplied gates
	tt<-rep(list(c(TRUE,FALSE)),length(y));
	names(tt)<-y;
	tt<-expand.grid(tt);
	len<-length(getIndices(x,y[1]))
	ind<-sapply(y,function(q){getIndices(x,q)})
	r<-matrix(TRUE,len,dim(tt)[1])
	for(q in 1:dim(tt)[1]){
		pos<-y[which(tt[q,]==TRUE)]
		neg<-y[which(tt[q,]==FALSE)]
		r[,q]<-tryCatch(.bGate(ind[,pos]),error=function(x)matrix(rep(TRUE,len),ncol=1))&tryCatch(.bGate(!ind[,neg]),error=function(x)matrix(rep(TRUE,len),ncol=1))
	}
	gates<-sapply(y,function(y)getGate(x,y))
	nnames<-names(gates)
	dims<-sapply(nnames,function(y)getDimensions(x,y));
	dlevels<-factor(apply(dims,2,function(x)paste(x,collapse=":")))
	udims<-length(unique(as.vector(dims)))
	for(i in 1:ncol(r)){
		d<-getData(x,y[1])[r[,i],udims]
	}
}

.bGate<-function(X){
	nc<-dim(X)[2];
	if(nc==2){
		return(X[,1]&X[,2])
	}
	else{
		return(X[,1]&Recall(X[,-(1)]))
	}
}


.mergetables<-function(...){
	l<-as.list(...)
	if(length(l)==1){
		return(l[[1]]);
	}else{
	  return(.mergetables(c(list(merge(l[[1]],l[[2]],by="pop.name",all=TRUE)),l[-c(1:2)])))
	}
}
# .getPopulations_1.6<-function(x,env=NULL){
# 	if(is.null(env)){
# 		level=0;
# 		##Get the Sample node.
# 		x <- xpathApply(x,"./parent::Sample/SampleNode")[[1]];
# 		env=new.env(parent=globalenv());
# 		#assign("count",0,env=env);
# 		assign("groups",.getSampleGroups(xmlRoot(x),win=TRUE),env=env);
# 		assign("gr",new("graphNEL",edgemode="directed"),env=env);
# 		gr<-get("gr",env)
# 		nodeDataDefaults(gr,"metadata")<-new.env(parent=emptyenv());
# 		assign("gr",gr,env=env);
# 		#The level of the hierarchy we're working with.
# 		assign("level",level,env=env)		
# 		assign("env",env,env=globalenv());
# 	}
# 	if(.isPopulation(x)){
# 		#If it's a Population, we've already assigned the root.
# 		parentpop <- tryCatch(rev(xpathApply(x,"./ancestor::Population",xmlAttrs))[[1]][["name"]],error= function(x) NA );
# 		root<-xpathApply(x,"./ancestor::SampleNode",function(x)xmlGetAttr(x,"name"))[[1]];
# 		rootcount<-xpathApply(x,"./ancestor::SampleNode",function(x)xmlGetAttr(x,"count"))[[1]];
# 		if(.hasGate_1.6(x)){
# 				mygate<-.extractGate(x,env=env);
# 				ng<-mygate[[2]] ##Should we exclude samples in this gate?
# 				mygate<-mygate[[1]]
# 				#Do this if it's not a Boolean Gate
# 				#print(mygate);
# 				assign("gr",graph::addNode(mygate@filterId,get("gr",env=env)),env=env);
# 				if(!is.na(parentpop)){
# 					assign("gr",addEdge(parentpop,mygate@filterId,get("gr",env=env)),env=env);
# 					}else{
# 						assign("gr",addEdge(root,mygate@filterId,get("gr",env=env)),env=env);
# 					}
# 					fcsfile<- unlist(tryCatch(xpathApply(x,"./ancestor::Sample/Keywords/Keyword[@name='$FIL']",function(z)xmlGetAttr(z,"value")),error=function(q)NA))
# 					compID<-xpathApply(x,"./ancestor::Sample",function(x)xmlGetAttr(x,"compensationID"))[[1]];
# 					if(is.null(compID)){
# 						compID<--2;
# 					}
# 					count<-xmlGetAttr(x,"count");
# 					gr<-get("gr",env=env)
# 					##get the flowJo name for the current population
# 					##Generate the flowJo population name
# 					finish<-mygate@filterId
# 					start<-RBGL::tsort(gr)[2]
# 					fjname<-RBGL::sp.between(gr,start,finish)[[1]]$path_detail
# 					fjname<-unlist(lapply(strsplit(fjname,"\\."),function(x)x[2]))
# 					fjname<-paste("/",paste(fjname,collapse="/"),sep="")
# 					nodeData(gr,mygate@filterId,"metadata")<-new.env(parent=emptyenv());
# 					assign("fjName",fjname,env=nodeData(gr,mygate@filterId,"metadata")[[1]])
# 					assign("gate",mygate,env=nodeData(gr,mygate@filterId,"metadata")[[1]])
# 					assign("fcsfile",fcsfile,env=nodeData(gr,mygate@filterId,"metadata")[[1]])
# 					assign("count",count,env=nodeData(gr,mygate@filterId,"metadata")[[1]])
# 					assign("compID",compID,env=nodeData(gr,mygate@filterId,"metadata")[[1]])
# 					assign("negated",ng,env=nodeData(gr,mygate@filterId,"metadata")[[1]])
# 					assign("isBooleanGate",FALSE,env=nodeData(gr,mygate@filterId,"metadata")[[1]])
# 					assign("thisIndices",list(),env=nodeData(gr,mygate@filterId,"metadata")[[1]])
# 					assign("parentTot",NA,env=nodeData(gr,mygate@filterId,"metadata")[[1]])
# 					assign("thisTot",0,env=nodeData(gr,mygate@filterId,"metadata")[[1]])
# 					#20110314
# 					assign("isGated",FALSE,env=nodeData(gr,parentpop,"metadata")[[1]])
# 					assign("gr",gr,env=env);
# 		}
# 		
# 	}else if(xmlName(x)=="SampleNode"){
# 		#We're at the root.
# 		root<-xmlGetAttr(x,"name");
# 		rootcount<-xmlGetAttr(x,"count");
# 		assign("gr",graph::addNode(root,get("gr",env=env)),env=env)
# 		gr<-get("gr",env=env)
# 		nodeData(gr,root,"metadata")<-new.env(parent=emptyenv());
# 		assign("count",rootcount,env=nodeData(gr,root,"metadata")[[1]]);
# 		##Don't have a compensation ID for version 1.6 XML workpsaces
# 		#compID<-xpathApply(x,"./ancestor::Sample",function(x)xmlGetAttr(x,"compensationID"))[[1]];
# 		#assign("compID",compID,env=nodeData(gr,root,"metadata")[[1]]);
# 		sampleID<-xmlGetAttr(x,"sampleID")[[1]];
# 		fcsfile<- unlist(tryCatch(xpathApply(x,"./ancestor::Sample/Keywords/Keyword[@name='$FIL']",function(z)xmlGetAttr(z,"value")),error=function(q)NA))
# 		if(length(fcsfile)==0){
# 			warning("The keywords for sample ",sampleID," don't contain the $FIL filename keyword.");
# 			#Use the name attribute of SampleNode
# 			fcsfile<-xmlGetAttr(x,"name")[[1]];
# 		}
# 		assign("fcsfile",fcsfile,env=nodeData(gr,root,"metadata")[[1]]);
# 		
# 		#More root node defaults
# 			assign("fjName",fcsfile,env=nodeData(gr,root,"metadata")[[1]])
# 			assign("gate",NA,env=nodeData(gr,root,"metadata")[[1]])
# 			assign("negated",FALSE,env=nodeData(gr,root,"metadata")[[1]])
# 			assign("isBooleanGate",FALSE,env=nodeData(gr,root,"metadata")[[1]])
# 			assign("thisIndices",list(),env=nodeData(gr,root,"metadata")[[1]])
# 			assign("parentTot",NA,env=nodeData(gr,root,"metadata")[[1]])
# 			assign("thisTot",0,env=nodeData(gr,root,"metadata")[[1]])
# 			##get keywords and assign keywords
# 			##.getKeywords has been fixed for version 1.6 XML workspace			
# 			kw<-.getKeywords(x,fcsfile);
# 			assign("keywords",kw,env=nodeData(gr,root,"metadata")[[1]])
# 			
# 		# nodeData(gr,root,"fcsfile")<-fcsfile;
# 		assign("gr",gr,env=env)
# 		level<-level+1;
# 		assign("level",env=env);
# 		#If there's child populations...
# 		if(length(.nextPopulation(x,level))!=0){
# 			lapply(.nextPopulation(x,level),function(x).getPopulations_1.6(x,env));
# 		}	
# 	}
# 	return()
# }

#20110314
.isGated.graphNEL<-function(x,y){
	return(get("isGated",env=nodeData(x@tree,y,"metadata")[[1]]));
}

.nextPopulation<-function(x,level){
	#Get all the population nodes one "level" below this node.
	xpathApply(x,paste("./descendant::Population[count(ancestor::Population) = ",level,"]",sep=""));
}
.getPopulations<-function(x,env=NULL){
	#Things to do the first time we encounter a sample
	if(is.null(env)){
		env=new.env(parent=globalenv());
		assign("groups",.getSampleGroups(xmlRoot(x),win=FALSE),env=env);
		assign("gr",new("graphNEL",edgemode="directed"),env=env)
		gr<-get("gr",env)
		nodeDataDefaults(gr,"metadata")<-new.env(parent=emptyenv());
		#Store the compensation matrices
		if(is.null(xpathApply(x,"./ancestor-or-self::Sample",function(x)xmlGetAttr(x,"compensationID"))[[1]])){
			#No compensation matrix
			#Here the matrix could be acquisition defined and stored in the spillover keyword, but the data is also stored compensated so it doesn't need to be applied.
			spillover.matrix<-try(strsplit(xpathApply(x,"./ancestor-or-self::Sample/Keywords/Keyword[@name='SPILL']",function(x)xmlGetAttr(x,"value"))[[1]],",")[[1]]);
			if(!inherits(spillover.matrix,"try-error")){
				dims<-as.numeric(spillover.matrix[1]);
				spillover.matrix<-spillover.matrix[-1L]
				cn<-spillover.matrix[1:dims];
				spillover.matrix<-matrix(as.numeric(spillover.matrix[-c(1:dims)]),byrow=TRUE,ncol=dims)
				colnames(spillover.matrix)<-cn;
				compensation<-spillover.matrix
			}else{
				compensation<-matrix();
			}
			
			assign("compensation",compensation,env);
		}else{
			compID<-xpathApply(x,"./ancestor-or-self::Sample",function(x)xmlGetAttr(x,"compensationID"))[[1]]
			if(compID!=-1 & compID!=-2){
				compobj<-.getCompensationMatrices(xmlRoot(x))[[as.numeric(compID)]]
				assign("compensation",compobj,env);
			}else if(compID==-1){
				#browser()
				#acquisition defined.. usually table 2 should have the "Acquisition defined" string.
				#This is not always true
				#warning("Compensation ID=-1 for sample")
				#data could be stored compensated. We want the compensation matrix.
			#str<-strsplit(.getCalibrationTableNames(xmlRoot(x))," ")
			#str<-lapply(str,function(x)x[1])
			#nm<-as.matrix(unique(do.call(c,str))[2])
				assign("compensation",as.matrix("Acquisition-defined"),env);
			}
		}
		assign("gr",gr,env=env);
		assign("env",env,env=globalenv())
	}
	#If we're at a population node
	if(.isPopulation(x)){		
		parentpop <- tryCatch(rev(xpathApply(x,"./ancestor::Population",xmlAttrs))[[1]][["name"]],error= function(x) NA );
		root<-xpathApply(x,"./ancestor::SampleNode",function(x)xmlGetAttr(x,"name"))[[1]];
		rootcount<-xpathApply(x,"./ancestor::SampleNode",function(x)xmlGetAttr(x,"count"))[[1]];
		if(!is.na(parentpop)){	
			parent.nn<-tryCatch(rev(xpathApply(x,"./ancestor::Population",xmlAttrs))[[1]][["nn"]],error=function(x)NA)
			parentpop<-paste(parent.nn,parentpop,sep=".");
		}else{
			#Parent is the root
			parentpop<-root;
			#Is the root already assigned?
			if(is.na(match(parentpop,nodes(get("gr",env=env))))){
				assign("gr",graph::addNode(parentpop,get("gr",env=env)),env=env)
			
				gr<-get("gr",env=env)
				nodeData(gr,parentpop,"metadata")<-new.env(parent=emptyenv());
				assign("count",rootcount,env=nodeData(gr,parentpop,"metadata")[[1]]);
				compID<-xpathApply(x,"./ancestor::Sample",function(x)xmlGetAttr(x,"compensationID"))[[1]];
				if(is.null(compID)){
					# -2 means there's no compensation ID
					compID<--2;
				}
				assign("compID",compID,env=nodeData(gr,parentpop,"metadata")[[1]]);
				fcsfile<- unlist(tryCatch(xpathApply(x,"./ancestor::Sample/Keywords/Keyword[@name='$FIL']",function(z)xmlGetAttr(z,"value")),error=function(q)NA))
				assign("fcsfile",fcsfile,env=nodeData(gr,parentpop,"metadata")[[1]]);
				#More root node defaults
				assign("fjName",fcsfile,env=nodeData(gr,parentpop,"metadata")[[1]])
				assign("gate",NA,env=nodeData(gr,parentpop,"metadata")[[1]])
				assign("negated",FALSE,env=nodeData(gr,parentpop,"metadata")[[1]])
				assign("isBooleanGate",FALSE,env=nodeData(gr,parentpop,"metadata")[[1]])
				assign("thisIndices",list(),env=nodeData(gr,parentpop,"metadata")[[1]])
				assign("parentTot",NA,env=nodeData(gr,parentpop,"metadata")[[1]])
				assign("thisTot",0,env=nodeData(gr,parentpop,"metadata")[[1]])
				#20110314
				assign("isGated",FALSE,env=nodeData(gr,parentpop,"metadata")[[1]])
				##get keywords and assign keywords
				kw<-.getKeywords(x,fcsfile);
				assign("keywords",kw,env=nodeData(gr,parentpop,"metadata")[[1]])	
				assign("gr",gr,env=env)
			}
		}
		#If there's a gate below this population and it's not a boolean gate proceed as before
		if(.hasGate(x)&!.isBooleanGate(x)){
			#Extract the gate
			mygate<-.extractGate(x,env=env);
			ng<-mygate[[2]] ##Is this gate negated?
			mygate<-mygate[[1]]
				
			#Assign the node to the graph
			assign("gr",graph::addNode(mygate@filterId,get("gr",env=env)),env=env);
			#And add an edge to the parent.
			assign("gr",addEdge(parentpop,mygate@filterId,get("gr",env=env)),env=env);
			#Pull the filename from the keywords
			fcsfile<- unlist(tryCatch(xpathApply(x,"./ancestor::Sample/Keywords/Keyword[@name='$FIL']",function(z)xmlGetAttr(z,"value")),error=function(q)NA))

			#The node metadata is node-specific and has to be pulled for each node. That's why the assignments below are replicated.
			compID<-xpathApply(x,"./ancestor::Sample",function(x)xmlGetAttr(x,"compensationID"))[[1]];
			if(is.null(compID)){
				compID<--2;
			}
			count<-xmlGetAttr(x,"count");
			# Presume that a NULL count is for empty pops.
			if(is.null(count))
				count<-0
			gr<-get("gr",env=env)
			##get the flowJo name for the current population
			##Generate the flowJo population name
			finish<-mygate@filterId
			start<-RBGL::tsort(gr)[2]
			fjname<-RBGL::sp.between(gr,start,finish)[[1]]$path_detail
			fjname<-unlist(lapply(strsplit(fjname,"\\."),function(x)x[2]))
			fjname<-paste("/",paste(fjname,collapse="/"),sep="")
			nodeData(gr,mygate@filterId,"metadata")<-new.env(parent=emptyenv());
			assign("fjName",fjname,env=nodeData(gr,mygate@filterId,"metadata")[[1]])
			assign("gate",mygate,env=nodeData(gr,mygate@filterId,"metadata")[[1]])
			assign("fcsfile",fcsfile,env=nodeData(gr,mygate@filterId,"metadata")[[1]])
			assign("count",count,env=nodeData(gr,mygate@filterId,"metadata")[[1]])
			assign("compID",compID,env=nodeData(gr,mygate@filterId,"metadata")[[1]])
			assign("negated",ng,env=nodeData(gr,mygate@filterId,"metadata")[[1]])
			assign("isBooleanGate",FALSE,env=nodeData(gr,mygate@filterId,"metadata")[[1]])
			assign("thisIndices",list(),env=nodeData(gr,mygate@filterId,"metadata")[[1]])
			assign("parentTot",NA,env=nodeData(gr,mygate@filterId,"metadata")[[1]])
			assign("thisTot",0,env=nodeData(gr,mygate@filterId,"metadata")[[1]])
			#20110314
			assign("isGated",FALSE,env=nodeData(gr,mygate@filterId,"metadata")[[1]])
			assign("gr",gr,env=env);	
		}
		#If it is a boolean gate.
		else if(.hasGate(x)&.isBooleanGate(x)){
			#we want to parse it and add it to the gating hierarchy. But we don't want to evaluate the counts just yet.
			gr<-get("gr",env=env)
			gr<-.parseBooleanGates(x,gr)
			assign("gr",gr,env=env);
		}
	}
	if(.hasChildren(x)){
		lapply(.children(x),function(x).getPopulations(x,env=env));
	}
	return();
}

.hasGate<-function(x){
	(length(xpathApply(x,"./child::PolygonGate|./child::BooleanGate"))!=0)
}
.hasGate_1.6<-function(x){
	(length(xpathApply(x,"./child::Gate"))!=0)
}
.isPopulation<-function(x){
	xmlName(x)=="Population"
}
##Familiar tree accessor methods
.hasChildren<-function(x){
	length(xmlChildren(x))!=0
}
.children<-function(x){
	if(.hasChildren(x)){
		xmlChildren(x);
	}else{
		NULL
	}
}
.parent<-function(x){
	if(!.isRoot(x)){
		return(xmlParent(x));
	}else
	return(NA)
}
.isLeaf<-function(x){
	!.hasChildren(x)
}
.isRoot<-function(x){
	is.null(xmlParent(x))
}
.hasNN<-function(x){
	if(is.null(xmlGetAttr(xpathApply(x,"/Workspace/SampleList/descendant::Sample[1]")[[1]],"nn"))){
		message("Need to do some preprocessing of the XML document tree for the first time. \n This will take a moment.");
	env<-new.env();
	assign("nn",1,envir=env);
	xpathApply(x,"/Workspace/SampleList/descendant::Sample|/Workspace/SampleList/descendant::Population|/Workspace/SampleList/descendant::SampleNode",function(x,e=env){addAttributes(x,"nn"=e[["nn"]]);e[["nn"]]<-e[["nn"]]+1;})
	return(0);	
	}
}

.isBooleanGate.graphNEL<-function(x,y){
	return(get("isBooleanGate",env=nodeData(x@tree,y,"metadata")[[1]]));
}
.isBooleanGate<-function(x){
	p<-xpathApply(x,"./BooleanGate")
	if(length(p)==0)
		return(FALSE)
	else
		return(TRUE)
}
#TODO Update this for all gate types when you have a complete example of a workspace.
# .extractGate_1.6<-function(x,env){
# 	gateType<-xpathApply(xpathApply(x,"./child::Gate")[[1]],"./child::*",xmlName)[[1]];
# }
.extractGate<-function(x,env){
	gateType<-setdiff(unlist(xpathApply(x,"./PolygonGate/node()",xmlName)),c("ParameterNames","Graph"))
	#gateType<-setdiff(unlist(xpathApply(x,"./node()",xmlName)),c("Graph","Population"));
	
	axes<-tryCatch(xpathApply(x,"./PolygonGate/PolyRect|./PolygonGate/Polygon|./PolygonGate/Ellipse",function(x){
		axes<-c(xmlGetAttr(x,"xAxisName"),xmlGetAttr(x,"yAxisName"));
		axes;
	})[[1]],error=function(y){
		##If it's not a Polygon or PolyRect, it may be one of the following.
		if(gateType=="Range"){
			axes<-xpathApply(x, "./PolygonGate/Range",function(q)xmlGetAttr(q,"xAxisName"))[[1]]
		}
		axes
	})
	#nm<-tryCatch(xpathApply(x,"./PolygonGate/PolyRect|./PolygonGate/Polygon|./PolygonGate/Ellipse",function(x){
	#	nm<-xmlGetAttr(x,"name");
	#	nm
	#})[[1]],error=function(y){
		#Get population name
		#nm<-tryCatch(xpathApply(x,"./PolygonGate/Range",function(y)xmlGetAttr(y,"name"))[[1]],error=function(y){
		#	nm<-xpathApply(x,"./PolygonGate/Ellipse",function(y)xmlGetAttr(y,"name"))[[1]]
		#	nm
		#})
	#	nm
	#});
	nm<-xmlGetAttr(x,"name")
	nn<-xmlGetAttr(x,"nn");
	nm<-paste(nn,nm,sep=".");
	vertices<-tryCatch(t(do.call(cbind,xpathApply(x,"./PolygonGate/PolyRect/Polygon/Vertex|./PolygonGate/Polygon/Polygon/Vertex",function(x){
		vrt<-c(as.numeric(xmlGetAttr(x,"x")),as.numeric(xmlGetAttr(x,"y")))
		vrt
	}))),error=function(y){
		vrt<-tryCatch(t(do.call(cbind,xpathApply(x,"./PolygonGate/Range//Vertex",function(y){
			vrt<-c(as.numeric(xmlGetAttr(y,"x")))
			vrt
			}))),error=function(y){
				vrt<-t(do.call(cbind,xpathApply(x,"./PolygonGate/Ellipse//Vertex",function(y){
					vrt<-c(as.numeric(xmlGetAttr(y,"x")),as.numeric(xmlGetAttr(y,"y")))
					vrt
				})))
			})
	})
	
	##set a one D gate as a rectangleGate.
	if(gateType=="Range"){
		vertices<-matrix(vertices,ncol=1);
	}
	colnames(vertices)<-axes
	##Get the PnG gain for the two axes.
	gains<-.getGains(x)
	#The keywords may or may not have the <>.
	#Best to remove them altogether and match?
	
	gains<-sapply(gsub("<","",gsub(">","",axes)),function(q){
		as.numeric(gains[match(q,gsub(">","",gsub("<","",gains[,1]))),2])
	})
	##Get the current sample name
	attrs<-xpathApply(x,"./ancestor::Sample",xmlAttrs)[[1]];
	sid<-attrs[["sampleID"]];
	compID<-tryCatch(attrs[["compensationID"]],error=function(x)-2);
	if(is.null(compID)){
		##In this case, we need to get more information from the parameters section. See below
		compID<--2;
	}
	if(compID!=-1&compID!=-2){
		compnames<-names(.getCompensationMatrices(xmlRoot(x)));
	}
	groups<-get("groups",env=get("env",env=globalenv()));
	samples<-.getSamples(x);
	# =======================================
	# = construct transformations code here =
	# =======================================
	if((compID!=-2)&(compID!=-1)){
		if(exists("transformations",env)){
			cal<-get("transformations",env);
		}else{
			##search for a calibration table name prefixes specific to this compID
			#It may not exist, so construct a table by default.
			#TODO test with Aaron's workspace - where the names don't match ..
			#Currently this doesn't work with Aaron's workspace where the names don't match. 
			#Need a default case.. etc.
			cal<-.getCalibrationTableSearch(xmlRoot(x),compnames[as.numeric(compID)])
			cal<-lapply(cal,function(x){
				at<-attr(x,"type")
				if(length(at)==0)
					attr(x,"type")<-"flowJo";
				x
			})
			#Is it empty?
			if(length(cal)==0){
				#Case where we can't find the transformation we expect.
				#Case where the transformations are not named after the parameters
				#Should we log-transform these dimensions?
				calnames<-unlist(xpathApply(x,"./ancestor::Sample/Parameter",function(x)xmlGetAttr(x,"name")))

				callog<-as.logical(as.numeric(unlist(xpathApply(x,"./ancestor::Sample/Parameter",function(x)xmlGetAttr(x,"log")))))
				#4096, the magic number..
				calrange<-(as.numeric(unlist(xpathApply(x,"./ancestor::Sample/Parameter",function(x)xmlGetAttr(x,"range"))))<4096)
				callog<-callog&calrange

				cal<-list();
				
				#default is identity transform
				cal<-rep(list(function(x){x}),length(calnames))
				names(cal)<-calnames
				for(i in 1:length(cal)){
					attr(cal[[i]],"type")<-"identity"
				}
				#Now log transform what needs to be log transformed
				llen<-length(which(callog))
				
				if(llen!=0){
					for(i in which(callog)){
						cal[[i]]<-function(x){x<-log(x,10);x[is.nan(x)]<-0;x[is.infinite(x)]<-0;x}
						attr(cal[[i]],"type")<-"log";
					}
				}
				##Finally flowJo-specific transform whatever is left.
					
				if(!is.null(.getCalibrationTableNames(x))){
					#Apply flowJo transform
					#Not clear how this should be assigned to the correct parameters
					#I have seen cases with wierd behaviour.
					#this range >=4096 & log=1, means flowJo defined transform.
					calfj<-as.logical(as.numeric(unlist(xpathApply(x,"./ancestor::Sample/Parameter",function(x)xmlGetAttr(x,"log")))))&!calrange
					if(length(which(calfj))!=length(.getCalibrationTableNames(x))){
						stop("I'm sorry, but the number of flowJo defined transformations doesn't match the number of transformed parameters in .extractGate. Please notify the package authors. Likely your workspace contains a case we haven't dealt with before.")
					}else if(length(calfj!=0)){
						#These are either ordered as in calnames or as in the StainChannelList
						stainnames<-unlist(xpathApply(xmlRoot(x),"/Workspace/StainChannelList/StringArray/String",xmlValue));
						cn<-names(cal[calfj])
						cal[calfj]<-sapply(flowWorkspace:::.getCalibrationTableNames(x),function(y).getCalibrationTable(x,y))[match(calnames[calfj],stainnames)]
						names(cal[calfj])<-cn;
						for(i in which(calfj))
							attr(cal[[i]],"type")<-"flowJo"
					}
				}
			}
			if(as.numeric(strsplit(.getFlowJoVersion(x),"\\.")[[1]][1])<9){
				cal<-.constructCalTables8.2(cal,x,compID,compnames)
			}
			assign("transformations",cal,env);
		}
	}else if(compID==-1){
		#Usually means acquisition defined compensation matrix
		if(exists("transformations",env)){
			cal<-get("transformations",env);
		}else{	
			#In Aaron's workspace, the names don't necessarily match the calibration tables. I don't know why that is, but we need to deal with it.
			
	r<-unique(unlist(lapply(strsplit(.getCalibrationTableNames(xmlRoot(x)),"<"),function(x)x[1])))[grep("Acquisition-defined",unique(unlist(lapply(strsplit(.getCalibrationTableNames(xmlRoot(x)),"<"),function(x)x[1]))))]
			if(length(r)!=0){	
				cal<-.getCalibrationTableSearch(xmlRoot(x),r)
				for(zz in 1:length(cal)){
					at<-attr(x,"type")
					if(length(at)==0)
					attr(cal[[zz]],"type")<-"flowJo"
				}
			}else{
				#Case where the transformations are not named after the parameters
				#Should we log-transform these dimensions?
				calnames<-unlist(xpathApply(x,"./ancestor::Sample/Parameter",function(x)xmlGetAttr(x,"name")))

				callog<-as.logical(as.numeric(unlist(xpathApply(x,"./ancestor::Sample/Parameter",function(x)xmlGetAttr(x,"log")))))
				#4096, the magic number..
				calrange<-(as.numeric(unlist(xpathApply(x,"./ancestor::Sample/Parameter",function(x)xmlGetAttr(x,"range"))))<4096)
				callog<-callog&calrange

				cal<-list();
				
				#default is identity transform
				cal<-rep(list(function(x){x}),length(calnames))
				names(cal)<-calnames
				for(i in 1:length(cal)){
					attr(cal[[i]],"type")<-"identity"
				}
				#Now log transform what needs to be log transformed
				llen<-length(which(callog))
				
				if(llen!=0){
					for(i in which(callog)){
						cal[[i]]<-function(x){x<-log(x,10);x[is.nan(x)]<-0;x[is.infinite(x)]<-0;x}
						attr(cal[[i]],"type")<-"log";
					}
				}
				##Finally flowJo-specific transform whatever is left.
					
				if(!is.null(.getCalibrationTableNames(x))){
					#Apply flowJo transform
					#Not clear how this should be assigned to the correct parameters
					#I have seen cases with wierd behaviour.
					#this range >=4096 & log=1, means flowJo defined transform.
					calfj<-as.logical(as.numeric(unlist(xpathApply(x,"./ancestor::Sample/Parameter",function(x)xmlGetAttr(x,"log")))))&!calrange
					if(length(which(calfj))!=length(.getCalibrationTableNames(x))){
						stop("I'm sorry, but the number of flowJo defined transformations doesn't match the number of transformed parameters in .extractGate. Please notify the package authors. Likely your workspace contains a case we haven't dealt with before.")
					}else if(length(calfj!=0)){
						#These are either ordered as in calnames or as in the StainChannelList
						stainnames<-unlist(xpathApply(xmlRoot(x),"/Workspace/StainChannelList/StringArray/String",xmlValue));
						cn<-names(cal[calfj])
						cal[calfj]<-sapply(flowWorkspace:::.getCalibrationTableNames(x),function(y).getCalibrationTable(x,y))[match(calnames[calfj],stainnames)]
						names(cal[calfj])<-cn;
						for(i in which(calfj))
							attr(cal[[i]],"type")<-"flowJo"
					}
				}
			}
			if(as.numeric(strsplit(.getFlowJoVersion(x),"\\.")[[1]][1])<9){
				#TODO there may be bugs in the 8.2 code for the case of acquisition defined comp matrices.. to be seen.
				cal<-.constructCalTables8.2(cal,x,compID,compnames)
			}
			assign("transformations",cal,env);
		}
	}else if(compID==-2){
		##Do we already have the transformations for this gating hierarchy?
		if(exists("transformations",env)){
			cal<-get("transformations",env);
			cal<-lapply(cal,function(x){
				at<-attr(x,"type")
				if(length(at)==0){
					attr(x,"type")<-"flowJo"
				}
				x
			})
		}else{
			#Should we log-transform these dimensions?
			calnames<-unlist(xpathApply(x,"./ancestor::Sample/Parameter",function(x)xmlGetAttr(x,"name")))
			
			callog<-as.logical(as.numeric(unlist(xpathApply(x,"./ancestor::Sample/Parameter",function(x)xmlGetAttr(x,"log")))))
			#4096, the magic number..
			calrange<-(as.numeric(unlist(xpathApply(x,"./ancestor::Sample/Parameter",function(x)xmlGetAttr(x,"range"))))<4096)
			callog<-callog&calrange
			
			cal<-list();
			#Probably need to do something here for 8.2 flowJo workspaces.
			#Yes we do! It's broken!
			
			if(as.numeric(strsplit(.getFlowJoVersion(x),"\\.")[[1]][1])<9){
				cal<-.constructCalTables8.2(cal,x,compID,compnames=NULL)
			}else{
				#identity transform by default
				cal<-rep(list(function(x){x}),length(calnames))
				names(cal)<-calnames
				for(i in 1:length(cal)){
					attr(cal[[i]],"type")<-"identity"
				}
				#Now log transform what needs to be log transformed
				llen<-length(which(callog))
				
				if(llen!=0){
					for(i in which(callog)){
						cal[[i]]<-function(x){x<-log(x,10);x[is.nan(x)]<-0;x[is.infinite(x)]<-0;x}
						attr(cal[[i]],"type")<-"log";
					}
				}
				##Finally flowJo-specific transform whatever is left.
					
				if(!is.null(.getCalibrationTableNames(x))){
					#Apply flowJo transform
					#Not clear how this should be assigned to the correct parameters
					#I have seen cases with wierd behaviour.
					#this range >=4096 & log=1, means flowJo defined transform.
					calfj<-as.logical(as.numeric(unlist(xpathApply(x,"./ancestor::Sample/Parameter",function(x)xmlGetAttr(x,"log")))))&!calrange
					if(length(which(calfj))!=length(.getCalibrationTableNames(x))){
						stop("I'm sorry, but the number of flowJo defined transformations doesn't match the number of transformed parameters in .extractGate. Please notify the package authors. Likely your workspace contains a case we haven't dealt with before.")
					}else if(length(calfj!=0)){
						#These are either ordered as in calnames or as in the StainChannelList
						stainnames<-unlist(xpathApply(xmlRoot(x),"/Workspace/StainChannelList/StringArray/String",xmlValue));
						cn<-names(cal[calfj])
						cal[calfj]<-sapply(flowWorkspace:::.getCalibrationTableNames(x),function(y).getCalibrationTable(x,y))[match(calnames[calfj],stainnames)]
						names(cal[calfj])<-cn;
						for(i in which(calfj))
							attr(cal[[i]],"type")<-"flowJo"
					}
				}
			}
			
			# Cleaned this up a bit.
			# for(i in 1:length(callog)){
			# 				if(callog[i]){
			# 					
			# 					cal[[i]]<-function(x){x<-log(x,10);x[is.nan(x)]<-0;x[is.infinite(x)]<-0;x}
			# 					attr(cal[[i]],"type")<-"log";
			# 				}else{
			# 					#the remainder are either identity or are transformed by 1/64.. 
			# 					if(as.numeric(strsplit(.getFlowJoVersion(x),"\\.")[[1]][1])<9){
			# 						cal<-.constructCalTables8.2(cal,x,compID,compnames=NULL)
			# 					}
			# 					#Check what's left and if it should be identity.
			# 					if(!calnames[i]%in%names(cal)){
			# 						cal<-c(function(x){x},cal)
			# 						attr(cal[[i]],"type")<-"identity";
			# 					}
			# 				}
			# 			}
			
			#names(cal)<-calnames;
			assign("transformations",cal,env);
		}
		# =========================================
		# = End of code that builds the trasforms =
		# =========================================
		##cal should now be the required transformation functions (log, or identity), when a compensation matrix is not defined.
		##the transformations should be saved to the metadata or data environment of the hierarchy to save time.
	}else{
		stop("File doesn't have a compensation matrix identified. I'm sorry but we don't support this yet. Contact the package maintainer, your workspace xml file has structure that we want to support, but don't yet.")
	}
	{
v<-sapply(1:length(axes),function(i)if(any(grepl(axes[i],names(cal)))){cal[[grep(axes[i],names(cal))]](vertices[,i])}else{vertices[,i]})
		colnames(v)<-colnames(vertices);
		vertices<-v;rm(v);
	}
	if(gateType=="PolyRect"||gateType=="Polygon"){
		mygate<-polygonGate(filterId=nm[[1]],.gate=t(t(vertices)/gains));
	}else if (gateType=="Range"){		
		mygate<-rectangleGate(filterId=nm[[1]],.gate=t(t(vertices)/gains));
	}else if(gateType=="Ellipse"){
		mygate<-polygonGate(filterId=nm[[1]],.gate=t(t(lapply(list(.ellipseFit(vertices)),function(y){colnames(y)<-colnames(vertices);y})[[1]])/gains));
	}
	##Check if the gate is "negated"
	ng<-xpathApply(x,paste("./PolygonGate/",gateType),function(x)xmlGetAttr(x,"negated"))[[1]]
	print(mygate);
	if(is.null(ng)){
		return(list(mygate,FALSE))
	}else{
		ng<-as.logical(as.numeric(ng));
	}
	return(list(mygate,ng))
	
}
.constructCalTables8.2<-function(cal,x,compID,compnames){
			if(length(cal)==0){
				#get all parameter names using a calibration.
 				calpars<-unlist(lapply(xpathApply(x,"./ancestor::Sample/Parameter",xmlAttrs),function(y){y[["name"]]["calibrationIndex"%in%names(y)];}))						
 calinds<-na.omit(as.numeric(unlist(lapply(xpathApply(x,"./ancestor::Sample/Parameter",xmlAttrs),function(y){try(y[["calibrationIndex"]]["calibrationIndex"%in%names(y)],silent=TRUE);}))))				
				cal<-sapply(calinds,function(index).getCalibrationTableByIndex(xmlRoot(x),index));
				cal<-lapply(cal,function(x){attr(x,"type")<-"flowJo";x})
				names(cal)<-paste(compnames[as.numeric(compID)],paste("<",calpars,">",sep=""))
			}
			# =============================================================================================================
			# = Check the lower and upper range of parameters that are linear. We may need to transform the data as well. =
			# =============================================================================================================
			lintrans<-unlist(xpathApply(x,"./ancestor::Sample/Parameter",function(x){if(xmlGetAttr(x,"highValue")[[1]]=="4096"){xmlGetAttr(x,"name")[[1]]}else NULL}))
			if(length(lintrans)!=0){
				lt<-sapply(lintrans,function(y){f<-function(x){x*64};attr(f,"type")<-"gateOnly";f})
				names(lt)<-lintrans;
				cal<-c(lt,cal)
			}
	return(cal);
}

# ===============================================================================
# = Return the version string of the flowJo version that generated the XML file =
# ===============================================================================
.getFlowJoVersion<-function(x){
	as.character(xpathApply(x,"/Workspace",function(x)xmlGetAttr(x,"flowJoVersion"))[[1]]);
}

setMethod("getSamples","flowJoWorkspace",function(x){
	.getSamples(x@doc)
})
setMethod("getSamples","GatingSet",function(x,isFullPath=FALSE){
	as.vector(unlist(lapply(x,function(y){
					getSample(y,isFullPath)
	})))
})
setMethod("getSample","GatingHierarchy",function(x,isFullPath=FALSE){
			ifelse(isFullPath,file.path(x@dataPath,x@name),x@name)
								
		})
setMethod("getSample","graphNEL",function(x){
			x@nodeData@defaults$data$sampleName
		})

setMethod("getSampleGroups","flowJoWorkspace",function(x){
	.getSampleGroups(x@doc)
})

.getSampleGroups<-function(x,win=FALSE){
	if(!win){
		do.call(rbind,xpathApply(x,"/Workspace/Groups/GroupNode",function(x){
			gid<-c(xmlGetAttr(x,"name"),xmlGetAttr(x,"groupID"));
			sid<-do.call(c,xpathApply(x,".//SampleRef",function(x){
				as.numeric(xmlGetAttr(x,"sampleID"))
			}))
			groups<-data.frame(groupName=gid[[1]],groupID=as.numeric(gid[2]),sampleID=as.numeric(sid));
		}))
	}else{
		do.call(rbind,xpathApply(x,"/Workspace/Groups/GroupNode",function(x){
			gid<-c(xmlGetAttr(x,"name"),xmlGetAttr(x,"groupID"));
			sid<-do.call(c,xpathApply(x,".//SampleRef",function(x){
				as.numeric(xmlGetAttr(x,"sampleID",default=NA))
			}))
			if(length(sid)==0){
				sid<-NA;
			}
			groups<-data.frame(groupName=gid[[1]],sampleID=as.numeric(sid));
		}))
	}
}

.flowJoTransform<-function(dataenv,cal){
	assign("axis.labels",list(),env=dataenv);
	#this should save some memory
	for (i in 1:dim(get("data",dataenv))[2]){
		j<-grep(as.vector(parameters((get("data",dataenv)))@data$name)[i],names(cal));
		if(length(j)!=0){
			#transform the data only if it's appropriate
			if(attr(cal[[j]],"type")!="gateOnly"){
				assign("tmp",cal[[j]](get("data",dataenv)@exprs[,i]),dataenv)
				assign("i",i,dataenv)
				eval(expression(data@exprs[,i]<-tmp),envir=dataenv)
			}
		}
	}
	datarange<-sapply(1:dim(range(get("data",dataenv)))[2],function(i){
		
		j<-grep(names(range(get("data",dataenv)))[i],names(cal));
		if(length(j)!=0){
			rw<-range(get("data",dataenv))[,i];
			if(attr(cal[[j]],"type")!="gateOnly"){
				r<-cal[[j]](c(rw))
			}else{
				r<-rw
			}
			###An unfortunate hack. If we use the log transformation, then negative values are undefined, so
			##We'll test the transformed range for NaN and convert to zero.
			r[is.nan(r)]<-0;
			###Is this transformed?
			if(all(rw==r)){
				#No transformation
				raw<-seq(r[1],r[2],by=(r[2]-r[1])/10)
				signif(raw,2)
				pos<-raw;
			}else{
				#based on the range
				#Inverse transform;
				f<-splinefun(cal[[j]](seq(rw[1],rw[2],l=100000)),seq(rw[[1]],rw[[2]],l=100000),method="natural")
				raw<-signif(f(seq(r[1],r[2],l=20)),2);
				pos<-signif(cal[[j]](raw),2)
			}
			assign("i",i,dataenv)
			assign("raw",raw,dataenv);
			assign("pos",pos,dataenv);
			eval(expression(axis.labels[[i]]<-list(label=as.character(raw),at=pos)),envir=dataenv);
			return(r);
		}else{
			range(get("data",dataenv))[,i]
		}
	})
	#Don't need to mess with the column names anymore.
	# cn<-sapply(1:dim(range(get("data",dataenv)))[2],function(i){
	# 		j<-grep(names(range(get("data",dataenv)))[i],names(cal));
	# 		if(length(j)!=0){
	# 			paste("<",parameters(get("data",dataenv))@data$name[i],">",sep="")
	# 		}else{
	# 			parameters(get("data",dataenv))@data$name[i]
	# 		}
	# 	})
	datarange<-t(rbind(datarange[2,]-datarange[1,],datarange))
	datapar<-parameters(get("data",dataenv));
	datapar@data[,c("range","minRange","maxRange")]<-datarange
	#assign("cn",cn,dataenv)
	#eval(expression(colnames(data@exprs)<-cn),envir=dataenv)
	#eval(expression(data@parameters@data$name<-cn),envir=dataenv)
	gc(reset=TRUE)
	#datapar@data$name<-cn
	assign("datapar",datapar,dataenv)
	eval(expression(data@parameters<-datapar),envir=dataenv)
	gc(reset=TRUE)
}

##This function should return all the calibration vectors associated with a single prefix name.
.getCalibrationTableSearch<-function(doc,term){
	nms<-.getCalibrationTableNames(doc)[grep(term,.getCalibrationTableNames(doc))]
 	trange<-sapply(nms,function(i).getCalibrationTable(doc,i))
	names(trange)<-nms;
	trange;
}

.getSamples<-function(x,win=FALSE){
	lastwarn<-options("warn")[[1]]
	options("warn"=-1)
	top<-xmlRoot(x)
	s<-do.call(rbind,xpathApply(top,"/Workspace/SampleList/Sample/SampleNode",function(x){
		attrs<-xmlAttrs(x);
	data.frame(tryCatch(as.numeric(attrs[["sampleID"]]),error=function(x)NA),tryCatch(attrs[["name"]],error=function(x)NA),tryCatch(as.numeric(attrs[["count"]]),error=function(x)NA))
		}))
		if(!win){
			cid<-as.numeric(paste(xpathApply(top,"/Workspace/SampleList/Sample",function(x)xmlGetAttr(x,"compensationID"))))
			pop.counts<-as.numeric(unlist(lapply(xpathApply(top,"/Workspace/SampleList/Sample"),function(x)xpathApply(x,"count(descendant::Population)"))))
			s<-data.frame(s,cid,pop.counts)
			colnames(s)<-c("sampleID","name","count","compID","pop.counts");
		}else{
			##Code for flowJo windows 1.6 xml
			#No compensation ID for windows. Use name
			colnames(s)<-c("sampleID","name","count");
		}
		s[,2]<-as.character(s[,2])
		options("warn"=lastwarn);
		s
}
.trimWhiteSpace<-function (x) 
{
	###Taken from the limma package.
    sub("[ \t\n\r]*$", "", sub("^[ \t\n\r]*", "", x))
}

setMethod("getCompensationMatrices","flowJoWorkspace",function(x){
	.getCompensationMatrices(x@doc);
})

## choose the correct transformation based on the compensation ID. If it's -2, we check the Parameters section for the sample.
setMethod("getTransformations","flowJoWorkspace",function(x){
	nms<-.getCalibrationTableNames(x@doc)
	u<-as.list(unique(unlist(lapply(sapply(nms,function(y)strsplit(y,"<")),function(y).trimWhiteSpace(y[1])))))
	T<-lapply(u,function(y).getCalibrationTableSearch(x@doc,y))
	names(T)<-u
	return(T)
})

.getCompensationMatrices<-function(z){
	top<-xmlRoot(z);
	matrices<-xpathApply(top,"/Workspace/CompensationMatrices/CompensationMatrix",function(x)xmlAttrs(x)[["name"]]);
	cmats<-lapply(matrices,function(mat){
	cmat<-as.numeric(unlist(xpathApply(top,paste("/Workspace/CompensationMatrices/CompensationMatrix[@name='",mat,"']/Channel/ChannelValue",sep=""),xmlGetAttr,"value")))
	d<-sqrt(length(cmat));
	cmat<-matrix(cmat,d,byrow=TRUE)
	colnames(cmat)<-matrix((unlist(xpathApply(top,paste("/Workspace/CompensationMatrices/CompensationMatrix[@name='",mat,"']/Channel/ChannelValue",sep=""),xmlGetAttr,"name"))),byrow=TRUE,d)[1,]
	rownames(cmat)<-matrix((unlist(xpathApply(top,paste("/Workspace/CompensationMatrices/CompensationMatrix[@name='",mat,"']/Channel/ChannelValue",sep=""),xmlGetAttr,"name"))),byrow=TRUE,d)[1,]
	cmat;
	})
	names(cmats)<-matrices;
	cmats;
}
# .getKeywords<-function(x){
# 	top<-xmlRoot(x)
# 	keywords<-xpathApply(top,"/Workspace/Keywords/Keyword",function(x){
# 		attrs<-xmlAttrs(x);
# 		if(length(attrs)==2){
# 			l<-list(attrs[["numeric"]]);
# 			names(l)<-attrs[["name"]]
# 		}else{
# 			l<-list(NULL);
# 			names(l)<-attrs[["name"]];
# 		}
# 		l
# 	});
# 	unlist(keywords,recursive=FALSE);
# }
.getStainChannels<-function(x){
	top<-xmlRoot(x)
	channels<-xpathApply(top,"/Workspace/StainChannelList/StringArray/String",xmlValue);
	channels<-unlist(channels);
	channels;
}



.getCalibrationTableNames<-function(x){
	top<-xmlRoot(x)
	tblnames<-xpathApply(top,"/Workspace/CalibrationTables/node()",function(x)xmlGetAttr(x,"name"))
	unlist(tblnames);
}

.getCalibrationTable<-function(x,name){
	top<-xmlRoot(x)
	if(inherits(name,"character")&length(name)==1){
		tbl<-xpathApply(top,paste("/Workspace/CalibrationTables/Table[@name='",name,"']",sep=""),xmlValue)
		tbl<-strsplit(gsub("\n","",tbl[[1]]),",")[[1]]
		#tbl<-ecdf(t(matrix(as.numeric(tbl),2))[,2])
		tbl<-splinefun(t(matrix(as.double(tbl),2))[,2:1],method="natural")
	}else{
		stop("Invalid name argument");
	}
	tbl;
}
.getCalibrationTableByIndex<-function(x,ind){
	top<-xmlRoot(x)
		tbl<-xpathApply(top,paste("/Workspace/CalibrationTables/Table[",ind,"]",sep=""),xmlValue)
		tbl<-strsplit(gsub("\n","",tbl[[1]]),",")[[1]]
		#tbl<-ecdf(t(matrix(as.numeric(tbl),2))[,2])
		tbl<-splinefun(t(matrix(as.double(tbl),2))[,2:1],method="natural")
	tbl;
}
.getCalibrationTableByIndex_inverse<-function(x,ind){
	top<-xmlRoot(x)
		tbl<-xpathApply(top,paste("/Workspace/CalibrationTables/Table[",ind,"]",sep=""),xmlValue)
		tbl<-strsplit(gsub("\n","",tbl[[1]]),",")[[1]]
		#tbl<-ecdf(t(matrix(as.numeric(tbl),2))[,2])
		tbl<-splinefun(t(matrix(as.double(tbl),2))[,1:2],method="natural")
	tbl;
}

#Function that returns the dimension specific transformation (from transformed space to raw intensity space)
#meant to be used for writing out gates to XML for flowJo to read.
.getCalibrationTable_inverse<-function(x,name){
	top<-xmlRoot(x)
	if(inherits(name,"character")&length(name)==1){
		tbl<-xpathApply(top,paste("/Workspace/CalibrationTables/Table[@name='",name,"']",sep=""),xmlValue)
		tbl<-strsplit(gsub("\n","",tbl[[1]]),",")[[1]]
		#tbl<-ecdf(t(matrix(as.numeric(tbl),2))[,2])
		tbl<-splinefun(t(matrix(as.double(tbl),2))[,1:2],method="natural")
	}else{
		stop("Invalid name argument");
	}
	tbl;
}

#TODO: 
# define what old XML is in terms of FlowJo versions.
# there's some kind of transformation that either needs to occur on the range values, or be sent 
# with the gate XML.  Otherwise, this doesn't match (at all) the FlowJo read in to create the flowWorkspace
.rectangleGateToOldFlowJoXML <- function(gate){
    gate_node <- NULL
    if(length(gate@min) == 1){
	    # building the "Range" node from the bottom up
	    vertices <- vector(mode = "list", length = 2)
		# the "as.character" conversion will produce 15 significant digits.
		# Old FlowJo XML appears to have "y" coordinates on these "Range" gates.  They are equal for both vertices.
        vertices[[1]] <- newXMLNode(name = "Vertex", attrs = list(x = as.character(gate@min), y = "0.0"))
		vertices[[2]] <- newXMLNode(name = "Vertex", attrs = list(x = as.character(gate@max), y = "0.0"))
		polygon <- newXMLNode(name = "Polygon", .children = list(vertices))
		range_name <- gate@filterId
		range_xAxisName <- gate@parameters[[1]]@parameters #wrapped in a "unitytransform". what.
        range <- newXMLNode(name = "Range", .children = polygon, attrs = list(name = range_name, xAxisName = range_xAxisName))
		
		# the "PolygonGate" will be comprised of the Range node above, and a ParameterNames.
		# Working backwards again...
		strings <- vector(mode = "list", length = 2)
		strings[[1]] <- newXMLNode(name = "String", text = range_xAxisName)
		strings[[2]] <- newXMLNode(name = "String", text = "")
		string_array <- newXMLNode(name = "StringArray", .children = strings)
		parameter_names <- newXMLNode(name = "ParameterNames", .children = string_array)
		gate_node <- newXMLNode(name = "PolygonGate", .children = list(parameter_names, range))
		
	} else {
	    stop('Only 1-d "Range" gates are currently supported.')
	}
    return(gate_node)	
}
