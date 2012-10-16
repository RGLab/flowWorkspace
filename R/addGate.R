setMethod("add",
		signature=signature(wf="GatingSet", "list"),
		definition=function(wf, action, ...)
		{
			
			flist<-filterList(action)
			add(wf,flist,...)
			
		})
setMethod("add",
		signature=signature("GatingSetInternal", "filterList"),
		definition=function(wf, action, ...)
		{
			nodeIDs<-lapply(names(action),function(sample){
								curFilter<-action[[sample]]
								gh<-wf[[sample]]
#								browser()
								add(wf=gh,action=curFilter,...)
							})
					
			nodeID<-nodeIDs[[1]]
		
		if(!all(sapply(nodeIDs[-1],function(x)identical(x,nodeID))))
			stop("nodeID are not identical across samples!")
		
		nodeID
			
		})
setMethod("add",
		signature=signature("GatingSetInternal", "filter"),
		definition=function(wf, action, ...)
		{
			
			message("replicating filter '",identifier(action),"' across samples!")
			
			actions<-sapply(getSamples(wf),function(x)return(action))
			add(wf,actions,...)
			
		})
.addGate<-function(gh,filterObject,parent=NULL, name=NULL,negated=FALSE){

	if(is.null(name))
		name<-filterObject$filterId
#	browser()
	##get node ID
	nodes<-getNodes(gh)
	if(is.null(parent))
		pid<-1
	else
	{
		pid<-match(parent,getNodes(gh))
		if(length(pid)==0)
			stop(node," not found in gating tree")
	}
	filterObject$negated<-negated
#	browser()	
	nodeID<-.Call("R_addGate",gh@pointer,getSample(gh),filterObject,pid-1,name)
	nodeID+1
}
setMethod("add",
		signature=signature("GatingHierarchyInternal", "rectangleGate"),
		definition=function(wf, action,... )
		{
#			browser()
			
			params<-parameters(action)
			
			if(length(params)==1)
			{
				#convert to rangeGate
				filterObject<-list(type=as.integer(2)
									,params=params
									,range=c(action@min,action@max)	
									,filterId=action@filterId
									)
				
			}else
			{
				#convert to polygon gate
				mat<-rbind(action@min,action@max)
				filterObject<-list(type=as.integer(5)
						,params=params
						,boundaries=mat
						,filterId=action@filterId)	
			}
			
			.addGate(wf,filterObject,...)
				
			
		})

setMethod("add",
		signature=signature("GatingHierarchyInternal", "polygonGate"),
		definition=function(wf, action, ...)
		{

			
#			browser()
			params<-parameters(action)
			
			filterObject<-list(type=as.integer(1)
								,params=params
								,boundaries=action@boundaries
								,filterId=action@filterId)	
		
			.addGate(wf,filterObject,...)
		})
setMethod("add",
		signature=signature("GatingHierarchyInternal", "booleanFilter"),
		definition=function(wf, action, ...)
		{
			
			
#			browser()
			expr<-action@deparse
			pattern<-"&|\\|"
			#get the position of logical operators
			op_ind<-unlist(gregexpr(pattern=pattern,expr))
			#extract these operators
			op<-trimWhiteSpace(substring(expr,op_ind,op_ind+1))
			##append & for the first node element(as C parser convention requires)
			op<-c("&",op)
			#split into node elements by operators
			refs<-unlist(strsplit(expr,split=pattern)) 
			refs<-trimWhiteSpace(refs)
			#extract is not operator
			isNot<-as.logical(regexpr("!",refs)+1) 
			#strip is not operator from node elements
			refs<-gsub("!","",refs)
			
			nNodes<-length(refs)
			if(length(isNot)!=nNodes)
				stop("the number of ! operators are inconsistent with nodes!")
			if(length(op)!=nNodes)
				stop("the number of logical operators are inconsistent with nodes!")
			filterObject<-list(type=as.integer(3)
								,refs=refs
								,isNot=isNot
								,op=op
								,filterId=action@filterId)	
#						browser()
			.addGate(wf,filterObject,...)
		})
setMethod("add",
		signature=signature("GatingHierarchyInternal", "quadGate"),
		definition=function(wf, action,names=NULL,... )
		{
			
			#convert to four recgates			
			params<-parameters(action)
			pd<-pData(parameters(getData(wf)))
			desc<-sapply(params,function(x)flowWorkspace:::.getChannelMarker(pd,x)$des)
			
			v <- action@boundary[params[1]]
			h <- action@boundary[params[2]]
			mat <- matrix(c(-Inf, v, h, Inf, v, Inf, h, Inf, -Inf, v, -Inf,
							h, v, Inf, -Inf, h), byrow=TRUE, ncol=4)              
			#clock-wise from top left quadrant
			if(is.null(names))
				names <- matrix(c(sprintf("%s-%s+", desc[1], desc[2]),
											sprintf("%s-%s-", desc[1], desc[2]),
											sprintf("%s+%s+", desc[1], desc[2]),
											sprintf("%s+%s-", desc[1], desc[2])),
										ncol=2)
			if(length(unique(names))!=4)
				stop("names have to be four unique strings!")
			unlist(lapply(1:4,function(i){
#								browser()
							rg <- rectangleGate(.gate=matrix(mat[i,], ncol=2,
											dimnames=list(c("min", "max"), params))
											,filterId=names[i])
							add(wf,rg,...)
						})
					)
			
			
			
		})

setMethod("Rm",
		signature=signature(symbol="character",
				envir="GatingSetInternal",
				subSymbol="character"),
		definition=function(symbol, envir, subSymbol, ...)
		{
			invisible(lapply(envir,function(gh){
#								browser()
								Rm(symbol,gh,subSymbol,...)
							}))
		})

setMethod("Rm",
		signature=signature(symbol="character",
				envir="GatingHierarchyInternal",
				subSymbol="character"),
		definition=function(symbol, envir, subSymbol, ...)
		{
#			browser()
			##remove all children nodes as well
			childrenNodes<-getChildren(envir,symbol)
			lapply(childrenNodes,function(child)Rm(child,envir))
			
			nid<-match(symbol,getNodes(envir))
			if(length(nid)==0)
				stop(symbol," not found in gating tree")
			.Call("R_removeGate",envir@pointer,getSample(envir),nid-1)
		})
#construct a gatingset with empty trees (just root node) 
setMethod("GatingSet",c("flowSet"),function(x,dMode=0,...){
			
			
			samples<-sampleNames(x)
			G<-new("GatingSetInternal")
			G@pointer<-.Call("R_NewGatingSet_rootOnly",samples,dMode=as.integer(dMode))
			
			
			globalDataEnv<-new.env()
			
			assign("ncfs",x,globalDataEnv)
#			nFiles<-length(samples)
#			set<-vector(mode="list",nFiles)	
#
#			for(i in 1:nFiles)
#			{
#				file<-files[i]		
#				sampleName<-samples[i]
#				gh<-new("GatingHierarchyInternal",pointer=G@pointer,name=sampleName)
##			browser()
#				localDataEnv<-nodeDataDefaults(gh@tree,"data")
#				localDataEnv$data<-globalDataEnv
#				
#				gh@flag<-FALSE #set gate flag
#				set[[i]]<-gh
#			}
#			names(set)<-samples
#			G@set<-set
						
			G@set<-sapply(samples,function(sampleName){
						gh<-new("GatingHierarchyInternal",pointer=G@pointer,name=sampleName)
						localDataEnv<-nodeDataDefaults(gh@tree,"data")
						localDataEnv$data<-globalDataEnv
						gh@flag<-TRUE #set gate flag
						gh
					})
			recompute(G)
			G
			
		})