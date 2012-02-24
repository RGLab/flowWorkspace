#################################################################################### 
#this routine extract the compenstation matrix,transformation functions and 
# all the gates from GatingHierarchy object in flowWorkspace package
# and convert them to the respective components of workFlow class for flowCore package  
# so that the ALLExportMethods.R can further deal with flowCore objects and export to flowjo IssueID #3 @2h
# Author: mike
###############################################################################
#getPramaNames<-function(transNames)
#{
#	ifelse(length(grep("<",transNames))>0,paste("<",unlist(strsplit(transNames,"<|>"))[2],">",sep=""),transNames)
#	
#}
.getTransParamList<-function(paramNames,transfromList)
{
	paramList<-NULL
	for(i in 1:length(transfromList))
	{
		curTrans<-transfromList[i]
		
		for(j in 1:length(paramNames))
		{
			if(length(grep(paramNames[j],names(curTrans)))>0)
				break
		}
		paramList<-c(paramList,paramNames[j])
		
	}
	paramList
}
.addBrackets<-function(orderedParamNames,compParamNames)
{
	
	for(i in 1:length(orderedParamNames))
	{
		isMatched<-F
		for(j in 1:length(compParamNames))
		{
			if(length(grep(orderedParamNames[i],compParamNames[j]))>0)
			{
				orderedParamNames[i]<-paste("<",orderedParamNames[i],">",sep="")
				break
			}
		}
		
	}
	orderedParamNames
}
  
## ==========================================================================
## Plot the workflow tree using Rgraphviz
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
plotWf<-function(x,...){
		
			## Find common action items for each tree level
			traverseEdge <- function(g, node=nodes(g)[1], result=NULL)
			{
				if(length(node)){
					children <- unlist(adj(g, node))
					lc <- length(children)
					if(lc){
						grps <-
								split(children, sapply(edgeData(g, from=node,
														attr="actionItem"),
												identifier))
						if(length(grps)>2)
							subGraphs[["node"]] <<- grps
						for(j in grps){
							lg <- length(j)
							result <- rbind(result, c(from=node,
											to=as.vector(j[ceiling(lg/2)])))
						}
						for(i in children)
							result <- traverseEdge(g, i, result=result)
					}
				}
				return(result)
			}
			if (!suppressWarnings(require(Rgraphviz))) 
				stop("You need to have Rgraphviz installed for this feature", 
						call.=FALSE)
						
			#Is this the call, rather than get(x@tree) ?
			tree <- get(x@tree@ID,x@env)
			
#			labels <- gsub(" ", "\n", gsub("_", "_\n", views(x)))
			labels <- sapply(views(x),function(x){ifelse(length(grep("\\.",x))>0,strsplit(x,"\\.")[[1]][2],x)})
			
			
			cmatch <- cbind(c("view", "compensateView", "transformView","gateView"), c("black", "green", "blue", "red"))
			mt <- match(sapply(views(x), function(y) class(x[[y]])),cmatch)
			col <- cmatch[mt, 2]
			nn <- views(x)
			names(labels) <- names(col) <- flowCore:::getAlias(nn, x)
			nodeRenderInfo(tree) <- list(shape="rectangler",fixedSize=FALSE,label=labels,fontsize=30, col=col,textCol=col)
			subGraphs <- list()
			tmp <- traverseEdge(tree)
			nAttrs <- list()
			if (!is.null(tmp)) {
#				elabels <- flowCore:::id2Alias(mapply(function(...)
#									identifier(edgeData(..., at="actionItem")[[1]]),
#								from=tmp[, 1], to=tmp[, 2],
#								MoreArgs=list(self=tree)), x)
#				elabels <- gsub("_", "_\n", elabels)
#				names(elabels) <- apply(tmp, 1, paste, collapse="~")
#				edgeRenderInfo(tree) <- list(label=elabels, lwd=2, 
#						fontsize=10, textCol="gray", col="gray")
				width <- rep(1.5, length(nn))
				height <- rep(0.8, length(nn))
				names(width) <- names(height) <- nn
				nAttrs <- list(width=width, height=height)
				
				g <- Rgraphviz:::layoutGraph(tree, layoutType="dot",nodeAttrs=nAttrs)
				Rgraphviz:::renderGraph(g)
				return(invisible(g))
			}
			else {
				plot(1, 1, type="n", ann=FALSE, axes=FALSE)
				text(1, 1, sprintf("baseview %s", identifier(x[[nn[1]]])))
				return(invisible(tree))
			}
		}
		
setMethod("flowWorkspace2flowCore",signature(obj="flowJoWorkspace"),function(obj,...){
	if(is.null(list(...)$path)){
		stop("You must specify a path to the fcs files via the path= argument. It must be exact.");
	}else{
		dataDir<-paste(list(...)$path,"/",sep="");
		groupId<-list(...)$groupId
	}
	
	isCompare<-ifelse(is.null(list(...)$isCompare),T,list(...)$isCompare)
	
		#get data and gateing hierarchy set by parsing xml
		G <- parseWorkspace(obj, name=groupId,execute=T,isCompare=isCompare,...)
		flowWorkspace2flowCore(G,path=dataDir)

})


setMethod("flowWorkspace2flowCore",signature(obj="GatingSet"),function(obj,...){
		if(is.null(list(...)$path)){
			stop("You must specify a path to the fcs files via the path= argument.");
		}else{
			dataDir<-list(...)$path
		}
		isCompare<-ifelse(is.null(list(...)$isCompare),T,list(...)$isCompare)

		env1<-new.env()
		assign("GS",obj,env1)
		#######################################################
		#group gatinghierarchies based on the comparison results
		#then generator one workflow for each group
		######################################################
		
		nHierarchy<-length(obj)
		sampleGroup<-list()
		if(isCompare)
		{
			message("comparing gating hierarchies")
			HierarchyIndices<-1:nHierarchy
			
			groupID<-0
			
			isGrouped<-rep(F,nHierarchy)
			while(!all(isGrouped))
			{
				sourceInd<-HierarchyIndices[!isGrouped][1]
				isGrouped[sourceInd]<-T#mark the current cource gh as grouped
				groupID<-groupID+1
				curGroup<-sourceInd
				targetIndices<-HierarchyIndices[!isGrouped]
				for(i in targetIndices)
				{
					if(.isSameGatingHierarchy(sourceInd,i,env1))
					{
						isGrouped[i]<-T#mark the matched target gh as grouped
						#add it to the current group
						curGroup<-c(curGroup,i)
					}
				}
				sampleGroup[[groupID]]<-curGroup
			}
			message("There ",ifelse(length(sampleGroup)==1,"is","are")," totally ",length(sampleGroup)," gating ",ifelse(length(sampleGroup)==1,"hierarchy","hierarchies"))
		}else
		{	#####################################################################################
			#if isCompare is False, then no comparsion and merging is carried out
			#each gating Hierarchy generates one workflow object 
			#####################################################################################
			for(i in 1:nHierarchy)
				sampleGroup[[i]]<-i
		}
		###########################################
		#apply conversion to each sample group
		#########################################
		workFlowList<-NULL
		for(k in 1:length(sampleGroup))
		{
			curGroup<-sampleGroup[[k]]
			hierarchy<-obj[[curGroup[1]]]
			sampleNames<-getSamples(obj)[curGroup]
			fcsfiles<-paste(dataDir,sampleNames,sep="/")
			message("converting from GateingHierarchy to workflow for sample group ",k)
			workFlowList<-c(workFlowList,.gatinghierarchy2workflow(hierarchy,fcsfiles))
		}
		workFlowList
})

setMethod("flowWorkspace2flowCore",signature(obj="GatingHierarchy"),function(obj,...){
			if(is.null(list(...)$path)){
				stop("You must specify a path to the fcs files via the path= argument.");
			}else{
				dataDir<-list(...)$path;
			}
			
			sampleNames<-obj@name
			fcsfiles<-paste(dataDir,sampleNames,sep="/")
			.gatinghierarchy2workflow(hierarchy=obj,fcsfiles)
		})

####fcsfiles point to multiple gatingHierarchies with the same structure,
###which can be merged to the same workflow
.gatinghierarchy2workflow<-function(hierarchy,fcsfiles){
						
			#######################################################
			#construct workflow object from the fcs files
			######################################################
			x<-hierarchy@tree
			##get node list
			nlist<-RBGL::bfs(x,nodes(x)[1]);
			
			#########create workflow object out of workframes from workspace
			
			fs<-read.flowSet(fcsfiles)
			wf1 <- workFlow(fs)
#				views(wf1)
			
			#########################################
			#convert compensation matrix to workflow
			#########################################
			message("converting compensation matrix")
			Comp.mat<-hierarchy@compensation
			
			if(!all(is.na(Comp.mat)))
			{
				Comp<-compensation(Comp.mat)
				add(wf1,Comp,name="CompensationView")
				views(wf1)
				
				#update the colnames of flowset with brackets
				compfs<-Data(wf1[["CompensationView"]])
				#add namespace of flowCore to make sure correct method of colnames is called 
				colNames<-flowCore:::colnames(compfs)
				compParams<-colnames(Comp.mat)
				updatedCompParams<-paste("<",compParams,">",sep="")
				colNames[which(colNames%in%compParams)]<-updatedCompParams
				compfs@colnames <- colNames
				flowCore:::"colnames<-"(compfs,colNames);
				
				#update the comp with the new flowset
				assign(wf1[["CompensationView"]]@data@ID,compfs,wf1[["CompensationView"]]@data@env)
				
				transParentViewName<-"CompensationView"
			}else
			{
				transParentViewName<-"base view"
			}
			
			############################################
			#extract transformation functions
			#add them to either comp view or basic view
			############################################
			message("converting transformations")
			
			transfromList<-hierarchy@transformations
			paramNames<-Data(wf1[["base view"]])@colnames
			orderedParamNames<-.getTransParamList(paramNames,transfromList)
			if(!all(is.na(Comp.mat)))
				orderedParamNames<-.addBrackets(orderedParamNames,colnames(Comp.mat))
			tl <- transformList(from=orderedParamNames,tfun=transfromList)
			
			add(wf1,tl,parent=transParentViewName,name=nlist[1])
			views(wf1)
			
			#######################################################
			#extract non-boolean gates and add them to workflow
			######################################################
			nids<-vector("character",length(nlist))
			nids[1]<-nlist[1]
			for (i in 2:length(nlist))
			{
				#get current node name
				n<-nlist[i]
				#get the gate from workspace by the node name
				g<-get("gate",envir=nodeData(x,n,"metadata")[[1]]);
				#get the parent node name and node ID
				parentName<-(setdiff(adj(ugraph(x),n)[[1]],adj(x,n)[[1]]))
				pid<-nids[which(nlist==parentName)]
				
				if(!(flowWorkspace:::.isBooleanGate.graphNEL(hierarchy,n)))
				{
					message("Gate attached to view: ", n);
					#attach the gate to the parent view
					add(wf1,g,parent=pid)	
					isNegated<-get("negated",envir=nodeData(x,n,"metadata")[[1]])
				}else #boolean gates
				{
					
					neg.Indicators<-g[[1]]$v
					gOperators<-g[[1]]$v2
					g.nodes<-g[[1]]$ref
					#				g.vector<-get("gate",env=nodeData(x,g.nodes[1],"metadata")[[1]])
					#				expr<-paste(neg.Indicators[1],"g.vector[[1]]",sep="")
					expr<-NULL
					g.vector<-NULL
					for(j in 1:length(g.nodes))
					{
						curNode<-g.nodes[j]
						curG<-get("gate",envir=nodeData(x,curNode,"metadata")[[1]])
						expr<-paste(expr,gOperators[j-1],neg.Indicators[j],"g.vector[[",j,"]]",sep="")
						g.vector<-c(g.vector,curG)
						
					}
					combineG<-eval(parse(text=expr))
					combineG@filterId<-n
					message("Gate attached to view: ", n);
					#attach the gate to the parent view
					add(wf1,combineG,parent=pid)
					#always remove negated result from boolean gate
					isNegated=F
					
				}
				
				#decide which to remove according to negated flag in workspace
				toRm<-paste(n,ifelse(isNegated,"+","-"),sep="")
				toKp<-paste(n,ifelse(isNegated,"-","+"),sep="")
				Rm(toRm,wf1)
				#fetch the node id for the unique view ID
				nid<-wf1[[toKp]]@ID
				#save the current node ID into vector for the future parent ID reference
				nids[i]<-nid
			}
			
			###################################################
			#to do:
			#comparison of gating hierarchies from one gatingSet 
			#  to decide if create mulitple workflows 
			#####################################################
			#plotWf(wf1)
		
		wf1
	}

#########################################################
#error to debug:when run .isSameGatingHierarchy(3,3,env1)
##########################################################
.isSameGatingHierarchy<-function(i,j,env1){
	
	if(!isTRUE(all.equal(env1$GS[[i]]@compensation,env1$GS[[j]]@compensation)))return(F)
	
	if(!isTRUE(all.equal(env1$GS[[i]]@transformations,env1$GS[[j]]@transformations)))return(F)
	
	#######save two trees in the new environment so that they can be access by reference
	env2 <- new.env()
	assign("gh1",env1$GS[[i]], envir=env2)
	assign("gh2",env1$GS[[j]], envir=env2)
	
	return(.isSameTree(nodeIndex1=1,nodeIndex2=1,env2))

}

###dummy version
#.isSameGatingHierarchy<-function(i,j,env1){
#	return(F)
#
#}

.isSameGate<-function(gate1,gate2)
{
	
	gate1@filterId<-""
	gate2@filterId<-""
	return(isTRUE(all.equal(gate1,gate2)))
}

.isSameGateNode<-function(nodeIndex1,nodeIndex2,env2)
{
	gateNode1<-env2$gh1@tree@nodeData@data[[nodeIndex1]]$metadata
	gateNode2<-env2$gh2@tree@nodeData@data[[nodeIndex2]]$metadata

	if(gateNode1$isBooleanGate!=gateNode1$isBooleanGate)
		return(F)
	else
	{
		gate1<-gateNode1$gate
		gate2<-gateNode2$gate
		###if boolean gate,two gating lists has to be strictly matched in order
		if(gateNode1$isBooleanGate)
		{
			if(!isTRUE(all.equal(gate1[[1]]$v,gate2[[1]]$v)))return(F)
			if(!isTRUE(all.equal(gate1[[1]]$v2,gate2[[1]]$v2)))return(F)
			##compare each gate in two list
			
			if(length(gate1[[1]]$ref)!=length(gate1[[1]]$ref))return(F)
			else
			{
				for(i in 1:length(gate1[[1]]$ref))
				{
					sourceGateNodeName<-gate1[[1]]$ref[i]
					targetGateNodeName<-gate2[[1]]$ref[i]
					g1<-getGate(env2$gh1,which(getNodes(env2$gh1)==sourceGateNodeName))
					g2<-getGate(env2$gh2,which(getNodes(env2$gh2)==targetGateNodeName))
					#when one pair of gates in the list	are different then return false	
					if(!.isSameGate(g1,g2))return(F)
				}
				return(T)##if all combined gate filters are the same,then return TRUE
				
			}
			
		}else##non-boolean gate(currently assumed as polygongate)
		{
			
			if(!is.object(gate1)|!is.object(gate1))
				return(T)###if either one is a non-gate object,then no need further comparison,consider them as the same
			else
			{
				###if both are gates then do the further comparison of gate filters
				.isSameGate(gate1,gate2)
			}	
		}
			
	}
	
}

.isSameTree<-function(nodeIndex1,nodeIndex2,env2)
{
	
	##first compare the current node gate
#	
	if(!.isSameGateNode(nodeIndex1,nodeIndex2,env2))
		return(F)#if current nodeGates are differnt,then no need to compare children
	else	
	{
		#otherwise go to children node	
		
		childrenIndexList1<-env2$gh1@tree@edgeL[[nodeIndex1]]$edges
		childrenIndexList2<-env2$gh2@tree@edgeL[[nodeIndex2]]$edges
		
		nChildren1<-length(childrenIndexList1)
		nChildren2<-length(childrenIndexList2)
		
		if(nChildren1!=nChildren2)
			return(F) 
		else
			nChildren<-nChildren1
		
		if(nChildren==0)
			return(T)	##if no both are leave node ,then return the same since gates are already the same according to the previous logic
		else
		{
			isMatchedList1<-rep(F,nChildren)
			isMatchedList2<-rep(F,nChildren)
			matchedPair<-list()
			#try to match each child node in the first list
			for(i in childrenIndexList1)
			{
#				sourceNode<-env1$tree1@nodeData@data[[i]]$metadata
				#only try to matched those unMatched node in the second list
				for(j in childrenIndexList2[!isMatchedList2])
				{
#					targetNode<-env1$tree2@nodeData@data[[j]]$metadata
					if(.isSameGateNode(i,j,env2))
					
						######################################################################
						#if found the match then mark the respective childnode flag in tree2
						#so that it won't need to be matched again,also mark the childnode in tree1
						######################################################################
						isMatchedList1[which(childrenIndexList1==i)]=T
						isMatchedList2[which(childrenIndexList2==j)]=T
						matchedPair[[length(matchedPair)+1]]<-c(i,j)
						break
						
				}
			}
		  
			#################################################################
			#After traverse the two list,if there is no unmatched left in both list 
			#,then continue check the subtree of each matched pair,otherwise automatically 
			#make the decision
			###################################################################
			
			if(all(isMatchedList1)&&all(isMatchedList2))
			{
				for(curPair in matchedPair)
						if(!.isSameTree(curPair[1],curPair[2],env2))
								return(F)##when one pair has different subtrees,then stop and return false
							
				return(T)#when all pairs passed the subtree check,return T for current node
					
			}else
				return(F)#if there is unmatched in either list,them consider them as different
		}
	}
	
	
		
}
