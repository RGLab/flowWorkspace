#' @include AllGenerics.R
#' @include AllClasses.R

setMethod("openWorkspace",signature=signature(file="character"),definition= function(file,options = 0,...){
 	#message("We do not fully support all features found in a flowJo workspace, nor do we fully support all flowJo workspaces at this time.")
	tmp<-tempfile(fileext=".xml")
	file.copy(file,tmp)
	if(inherits(file,"character")){
		x<-xmlTreeParse(tmp,useInternalNodes=TRUE,options = options, ...);
	}else{
		stop("Require a filename of a workspace, but received ",class(x)[1]);
	}
	ver<-xpathApply(x,"/Workspace",function(x)xmlGetAttr(x,"version"))[[1]]
	x<-new("flowJoWorkspace",version=ver,.cache=new.env(parent=emptyenv()),file=basename(file),path=dirname(file),doc=x, options = as.integer(options))
	x@.cache$flag=TRUE;
	return(x);
})

setAs("list", "GatingSet", function(from, to ){
	if(!all(unlist(lapply(from,function(y)class(y)=="GatingHierarchy"),use.names=FALSE))){
		stop("Can't coerce this list to class GatingSet");
	}
	new(to, set=from)
})


setMethod("show",c("flowJoWorkspace"),function(object){
	cat("FlowJo Workspace Version ",object@version,"\n");
	cat("File location: ",object@path,"\n");
	cat("File name: ",object@file,"\n");
	if(object@.cache$flag){
		cat("Workspace is open.","\n");
		cat("\nGroups in Workspace\n");
		tbl<-table(Name=getSampleGroups(object)$groupName,GroupID=getSampleGroups(object)$groupID)
		print(data.frame(Name=rownames(tbl),"Num.Samples"=diag(tbl)))
	}else{	
		cat("Workspace is closed.","\n")
	}
})


setMethod("closeWorkspace","flowJoWorkspace",function(workspace){
	free(workspace@doc);
	workspace@.cache$flag<-FALSE;
})
setOldClass("summary")

setMethod("summary",c("flowJoWorkspace"),function(object,...){
	show(object,...);
})
#options is passed to xmlTreeParse
setMethod("parseWorkspace",signature("flowJoWorkspace"),function(obj,useInternal=TRUE,name=NULL,execute=TRUE,isNcdf=FALSE,subset=NULL,nslaves=4,requiregates=TRUE,includeGates=TRUE,dMode = 0,path=obj@path,...){
	
	
			
	if(isNcdf&!TRUE){
	stop("isNcdf must be FALSE since you don't have netcdf installed");
	}
	#message("We do not fully support all versions of flowJo XML workspaces at this time.")
	#message("If your workspace is not supported or if this package throws an error, we encourage you to contact the package maintainter with a bug report. We will endeavour to add support for other features upon request.")
	##########################################################
	#path needs to be specified anyway to get full path of fcs
	#########################################################
#	if(execute){
    ##m<-match("path",names(list(...))) 
#    path=list(...)$path     
#    if(is.null(path)){ 
            ##stop("If execute=TRUE, you must specify a path to the fcs files via path= argument"); 
#            path=obj@path 
#    }
	#Check that the files exist now so we don't have to wait a long time.
	filenames<-flowWorkspace:::getFileNames(obj);
	#sapply(filenames,function(x)list.files(path=path,pattern=x,recursive=T))
	#	missingfiles<-!file.exists()
	#	if(length(which(missingfiles))/length(filenames)>=0.25){
	#		#warning(length(which(missingfiles))/length(filenames)*100,"% of the ",length(filenames)," FCS files can't be found at ",obj@path);
	#		#warning("They will be excluded from the import");
	#		#warning("Perhaps you want to specify a correct path to the files or copy the missing files over?")
	#	}
#	}
#	browser()
	x<-obj@doc;
	.hasNN(x);
	wsversion<-xpathApply(x,"/Workspace",function(z)xmlGetAttr(z,"version")[[1]])[[1]];
#	browser()
	if(!wsversion=="2.0"){
		#Windows version code
		s<-.getSamples(x,win=TRUE);
		g<-.getSampleGroups(x,win=TRUE);
	}else{
		s<-.getSamples(x);
		g<-.getSampleGroups(x);
	}
#	browser()
	sg<-merge(s,g,by="sampleID");
	#requiregates - exclude samples where there are no gates? if(requiregates==TRUE)
	if(requiregates){
		sg<-sg[sg$pop.counts>0,]
	}
	if(wsversion=="2.0"){
		##samples may have no compID if they are only log-transformed, for example.
		##Keep samples with compID = NA and set it to -2
		#If the compID = NA, check the Sample Parameter attributes for the transformation information.
		message("Version recognised. Continuing..")
	}#else if (wsversion=="1.6"){
		#Windows compensation and transformation work differently.. there is no comp id
	
#		stop("Sorry, we don't support this type of workspace (flowJo Windows) at the moment. But we are working on it!")
		
	#}
	else{
		useInternal<-TRUE
#		stop("Workspace Version not supported");
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
	if(is.factor(subset)){
		subset<-as.character(subset)
	}
	if(is.character(subset)){
	#subset s sg by file name
          #pull the file names from the keywords
          filenames<-.getKeyword(obj,"$FIL")
          #need to match filename to sampleID
          sg<-cbind(sg,filename=sapply(sg$sampleID,function(x).getKeywordsBySampleID(obj,x,"$FIL")))
          sg<-subset(sg,filename%in%subset)
	}
	if(wsversion=="2.0"){
		l<-sapply(sg[sg$groupName==groups[result],]$sampleID,function(i){
			xpathApply(x,paste("/Workspace/SampleList/Sample[@sampleID='",i,"']",sep=""))[[1]]
			})
	}else if(substr(wsversion,1,3)%in%c("1.6","1.8")){
		l<-sapply(sg[sg$groupName==groups[result],]$sampleID,function(i){
			xpathApply(x,paste("/Workspace/SampleList/Sample/DataSet[@sampleID='",i,"']",sep=""))[[1]]
			})
	}else{
		stop("Workspace Version not Supported");
	}
#	browser()
	# Allow import of a subset of samples
	if(!missing(subset)){
	if(is.numeric(subset)){
		if(max(subset)<=length(l)&min(subset)>=1)
		l<-l[subset]
	}
	}
	if(length(l)==0){
		stop("No samples in this workspace to parse!")
#		return(new("GatingSet"))
	}
	message("Parsing ",length(l)," samples");
#	browser()
	if(useInternal)
	{
		sampleIDs<-unlist(lapply(l,xmlGetAttr,"sampleID"))
		return (.parseWorkspace(xmlFileName=file.path(obj@path,obj@file),sampleIDs,execute=execute,dMode=dMode,isNcdf=isNcdf,includeGates=includeGates,path=path, xmlParserOption = obj@options,wsversion=wsversion,...))
	}
	#TODO parallelize
	if(length(grep("snowfall",loadedNamespaces()))==1){
	    if(is.null(sfGetCluster())){
	        stop("Must initialize snowfall cluster first")
	    }
	    sfLibrary(flowWorkspace)
	    for(Z in 1:length(l)){
	        l[[Z]]<-xmlGetAttr(l[[Z]],"nn")
        }
	    #I believe this breaks because the underlying C xml object is not available on the other nodes, and I'm not sure how to export it.
	    #here's what we'll do.. send the "nn" id for the top level nodes in l.
	    #convert the document to a character representation and save/send it to the nodes (x)
	    #Each node parses the document and pulls up the necessary "nn" node, then continues. 
	    tmp<-saveXML(xmlRoot(x))
	    message("exporting XML tree to nodes")
	    tf<-tempfile()
	    save(tmp,file=tf);
	    sfExport("tf","TEMP")
	    rm(tmp)
	  #  gc()
	    clusterEvalQ(sfGetCluster(),{load(tf);tmp<-xmlRoot(xmlTreeParse(tmp, asText = TRUE, useInternalNodes = TRUE));invisible()})
	    message("parsing")
		G<-sfLapply(l,function(x){
		    #parse the exported variable
		    #x<-xmlRoot(xmlTreeParse(tmp, asText = TRUE, useInternal = TRUE))
		    x<-xpathApply(tmp,paste("//node()[@nn=",x,"]",sep="\'"))[[1]]
			message("Parsing sampleID ",xmlGetAttr(x,"sampleID"));
			.getPopulations(x,env=NULL,includegates=includeGates);
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
			rm(x)
			#gc()
			tf<-tempfile()
			ret<-list(graph=graph,transformations=transformations,compensation=compensation)
			if(!file.exists(TEMP)){
			    dir.create(TEMP,recursive=TRUE)
			}
			save(ret,file=file.path(TEMP,basename(tf)));
			return(file.path(TEMP,basename(tf)));
			})
			#run through G and reconstruct the object)
			for(i in seq_along(G)){
			    load(G[[i]]);
			    G[[i]]<-ret;
			    cat(".")
			}
			#seems to work.. passing the large data back and forth is not optimal.. could save to disk and have the master node read it from disk and construct the required object.
		#stop the snowfall cluster, we're done.
		sfStop();	
	}else if(length(grep("multicore",loadedNamespaces()))==1){
	    G<-multicore::mclapply(l,function(x){
	        message("Parsing sampleID ",xmlGetAttr(x,"sampleID"));
			.getPopulations(x,env=NULL,includegates=includeGates);
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
	    })
	}else{
	G<-lapply(l,function(x){
		message("Parsing sampleID ",xmlGetAttr(x,"sampleID"));
		.getPopulations(x,env=NULL,includegates=includeGates);
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
		})
	}
	
		fn<-do.call(c,lapply(G,function(x){		
			get("fcsfile",envir=nodeDataDefaults(x$graph,"metadata"))
		}))
		names(G)<-fn
	
		excludefiles<-vector("numeric")
		for(i in 1:length(G)){
			nodeDataDefaults(G[[i]]$graph,"group")<-groups[result]
			#########################################################
			#get full path for each fcs and store in dataPath slot
			#########################################################
			file<-names(G[i])
			lastwarn<-options("warn")[[1]];
			options("warn"=-1);
			##escape "illegal" characters
			file<-gsub("\\)","\\\\)",gsub("\\(","\\\\(",file))
			absPath<-list.files(pattern=paste("^",file,"",sep=""),path=path,recursive=TRUE,full=TRUE)
			options("warn"=lastwarn)
			if(length(absPath)==0){
				warning("Can't find ",file," in directory: ",path,"\n");
				excludefiles<-c(excludefiles,i);
			}else{
				G[[i]]$dataPath<-dirname(absPath[1])
			}
		}
		#Remove samples where files don't exist.
		if(length(excludefiles)>0){
			message("Removing ",length(excludefiles)," samples from the analysis since we can't find their FCS files.");
			G<-G[-excludefiles];
		}
		
		G<-lapply(G,function(x)new("GatingHierarchy",tree=x$graph,nodes=nodes(x$graph),name=get("fcsfile",envir=nodeDataDefaults(x$graph,"metadata")),flag=FALSE,transformations=x$transformations,compensation=x$compensation,dataPath=x$dataPath,isNcdf=isNcdf))
		G<-new("GatingSet",set=G);
		##################################################
		#create ncdf file without adding matrices yet
		#################################################	
		
		if(execute){
			#comments:maybe execute should not be exposed to 
			#users for gating separately from parseWorkspace because ncdfFlow object is created during
			#executing provess and stored in gatingHierarchy after transformation
			if(isNcdf){
				ncfs1  <-read.ncdfFlowSet(files=getSamples(G,isFullPath=TRUE),flowSetId="fs1",isWriteSlice=TRUE)				
				dataEnvironment=new.env(parent=.GlobalEnv);
				assign("ncfs",ncfs1,envir=dataEnvironment)
			}else{
			    dataEnvironment<-NULL
			    ncfs1<-NULL
			}
			if(.packageLoaded("Rmpi")){	
				#nslaves=4
				mpi.spawn.Rslaves(nslaves=nslaves)
				mpi.remote.exec(library(flowWorkspace))
				if(isNcdf){
				    mpi.remote.exec(library(ncdfFlow))
			    }
				mpi.bcast.Robj2slave(ws)
				mpi.bcast.Robj2slave(G)
				mpi.remote.exec(options("flowWorkspace_mpi_communication"=TRUE));
				mpi.remote.exec(options("flowWorkspace_mpi_read_token"=FALSE    ));
				options("flowWorkspace_mpi_communication"=FALSE)
				mpi.bcast.Robj2slave(ncfs1)
				mpi.bcast.Robj2slave(dataEnvironment)
				wrapper<-function(){
					more<-1;
					while(more==1){
						#ask for a task
						mpi.send.Robj(0,0,1)
						i<-mpi.recv.Robj(0,2)
						if(!is.na(i)){
						g<-execute(G[[i]],isNcdf=isNcdf(G[[i]]),ncfs=ncfs1,dataEnvironment=dataEnvironment)
						
						mpi.send.Robj(g,0,5)
    					rm(g)
    					#gc(reset=T);	

    					#are there more tasks?
    					mpi.recv.Robj(0,mpi.any.tag())
    					sourcetag<-mpi.get.sourcetag();
    					if(sourcetag[2]==7){
    						#Nope I'm done!
    						more<-0
    						next;
    					}
					}else{
					    more<-0;
					    next;
					}
						
					}
				}
				
				#in master
				mpi.bcast.Robj2slave(wrapper);
				mpi.bcast.cmd(wrapper())
				
				#divy up the tasks
				ntasks<-length(G);
				tasks<-1:ntasks
				state<-0;
				readlock<-0;
				readqueue<-NULL;
				writequeue<-NULL
				pending<-NULL
				results<-NULL;
				while(length(tasks)>0|length(results)<ntasks){
					#message("length(tasks): ", length(tasks), " length(results): ", length(results))
					#message("checking if anyone needs to write")
					if(readlock==0&state==0&length(writequeue)>0){
						#okay, go for it
						#message("they can write")
						mpi.send.Robj(0,writequeue[1],4)
						#message("okay, they're writing")
						writequeue<-writequeue[-1L]
						state<-1;
					}
					#give permission to the next thread to read
					if(state==0&readlock==0&length(readqueue)>0){
					    #11 permission to read
					    mpi.send.Robj(0,readqueue[1],11)
					    readqueue<-readqueue[-1L]
					    readlock<-1;
					}
					#listen for a request
					#message("listening")
					obj<-mpi.recv.Robj(mpi.any.source(),mpi.any.tag())
					sourcetag<-mpi.get.sourcetag();
					who<-sourcetag[1]
					what<-sourcetag[2]
					#Asking for data
					if(what==1){
						#message("they want data")
						if(length(tasks)>0){
						    mpi.send.Robj(tasks[1],sourcetag[1],2);
						    message("Gating");
    						tasks<-tasks[-1L]
					    }else{
					        mpi.send.Robj(NA,sourcetag[1],2);
					    }
						#message("sent them data")
					}
					#Needs permission to write to NetCDF
					if(what==3){
						#queue up
						#message("they want do write to ncdf")
						writequeue<-c(writequeue,who)
					}
					#They sent me a result
					if(what==5){
						#message("they have a result")
						results<-c(results,obj)
						#Do I have any more data to hand out?
						if(length(tasks)>0){
							#message("there's more results for ",who)
							mpi.send.Robj(0,who,6)
							#message("they got more results")
						}else{
							#message("there's no more results for ", who)
							mpi.send.Robj(0,who,7)
							#message("moving on")
						}
					}
					if(what==8){
						#message("they're done writing")
						state<-0
					}
					#read lock..
					#12 done reading
					if(what==12){
					    readlock<-0;
					}
					#10 permission to read
					if(what==10){
					    #Asking Permission to Read
					   readqueue<-c(readqueue,who);
					}
				}
				#Need to clear any remaining requests;
				
                #message("done gating; Merging results")
				#results is a list. Need to merge the ncdfFlowSet metadata.
				if(isNcdf){
				    ncfses<-lapply(results,function(x)nodeData(x@tree,getNodes(x)[1],"data")[[1]][["data"]]$ncfs)
				    names(ncfses)<-unlist(lapply(results,function(x)x@name),use.names=FALSE)
				    snames<-names(ncfses)
				    message("Cloning flowset")
				    newnc<-ncdfFlow::clone.ncdfFlowSet(ncfses[[1]],isNew=FALSE,isEmpty=FALSE)
				    for(samp in snames){
        				newnc@frames[[samp]]<-ncfses[[samp]]@frames[[samp]]
        			}
        			colnames(newnc)<-colnames(as.list(newnc@frames)[[1]])
        			denv<-nodeData(results[[1]]@tree,getNodes(results[[1]])[[1]],"data")[[1]][["data"]]
        			assign("ncfs",newnc,denv)
        			for(i in 1:length(results)){
        				assign("data",denv,nodeData(results[[i]]@tree,getNodes(results[[i]])[1],"data")[[1]])
        			}            
			    }else{
			        
			    }
				G<-new("GatingSet",set=results)
				rm(results);
				#gc(reset=TRUE);
				mpi.remote.exec(options("flowWorkspace_mpi_communication"=FALSE));
				options("flowWorkspace_mpi_communication"=NULL)
			}
			else{
				G<-lapply(G,function(x)execute(hierarchy=x,isNcdf=isNcdf(x),ncfs=ncfs1,dataEnvironment=dataEnvironment))
			}
		}
		return(G);
})



getFileNames<-function(ws){
	if(class(ws)!="flowJoWorkspace"){
		stop("ws should be a flowJoWorkspace")
	}else{
		unlist(xpathApply(ws@doc,"/Workspace/SampleList/Sample/Keywords/Keyword[@name='$FIL']",function(x)xmlGetAttr(x,"value")),use.names=FALSE);
	}
}

mkformula<-function(dims2,isChar=FALSE){
	if(length(dims2)==1){
		form<-paste(c("",sapply((dims2), function(x) paste("`",x, "`", sep = ""))), collapse = "~")
	}else{
		form<-paste(sapply((dims2),function(x)paste("`",x,"`",sep="")),collapse="~")
	}
	if(!isChar)
		form<-as.formula(form)	
	return(form)
}

setAs("GatingSet","list",function(from){l<-vector("list",length(from));
for (i in seq_along(l)){
l[[i]]<-from[[i]]
}
l})


.getNcdf<-function(obj){
	if(!inherits(obj,"GatingHierarchy")){
		stop("obj must be of class GatingHierarchy")
	}
	if(!obj@flag&!obj@isNcdf){
		stop("Object doesn't hold ncdf data");
	}
	r<-nodeDataDefaults(obj@tree,"data")[["data"]];
	if(class(r)=="environment"){
		r$ncfs
	}
}

.getKeywordsBySampleID <- function(obj,sid,kw=NULL){
  kws<-xpathApply(obj@doc,sprintf("/Workspace/SampleList/Sample[@sampleID='%s']/Keywords/Keyword",sid),xmlAttrs)
  if(!is.null(kw)){
    unlist(lapply(kws,function(x)x["value"][x["name"]%in%kw]))
  }else{
    kws
  }
}


setMethod("getKeywords",c("flowJoWorkspace","character"),function(obj,y){
	w <- which(xpathApply(obj@doc,"/Workspace/SampleList/Sample/Keywords/Keyword[@name='$FIL']",function(x)xmlGetAttr(x,"value"))%in%y)
	l<-xpathApply(obj@doc,paste("/Workspace/SampleList/Sample[",w,"]/Keywords/node()",sep=""),xmlAttrs)
	names(l)<-lapply(l,function(x)x[["name"]])
	l<-lapply(l,function(x)x[["value"]])
	return(l);
})

.getKeyword<-function(ws,x){
	if(inherits(ws,"flowJoWorkspace")&class(x)=="character"){
		 unlist(xpathApply(ws@doc,paste("/Workspace/SampleList/Sample/Keywords/Keyword[@name='",x,"']",sep=""),function(x)xmlGetAttr(x,"value")),use.names=FALSE)
	}else{
		stop("No such keyword")
	}
	
}

getFJWSubsetIndices<-function(ws,key=NULL,value=NULL,group,requiregates=TRUE){
	if(!is.numeric(group)){
		stop("group must be numeric")
	}
	if(!is.character(key)&!is.null(key)){
		stop("keyword must be character")
	}
	if(!is.character(value)&!is.null(value)){
		stop("value must be character")
	}
	if(!class(ws)=="flowJoWorkspace"){
		stop("ws must be a flowJoWorkspace object")
	}
	s<-getSamples(ws);
	#TODO Use the actual value of key to name the column
	if(!is.null(key)){
	s$key<-flowWorkspace:::.getKeyword(ws,key)
	}
	g<-getSampleGroups(ws)
	sg<-merge(s,g,by="sampleID")
	if(requiregates){
	sg<-sg[sg$pop.counts>0,]
	}
	sg$groupName<-factor(sg$groupName)
	groups<-levels(sg$groupName)
	if(group>length(groups)){
		stop("group is invalid, out of range")
	}
	sg<-sg[sg$groupName%in%groups[group],]
	if(!is.null(key)&!is.null(value)){
	return(which(sg$key%in%value))
	}
	return(sg)
}

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

setMethod("getBoundaries",signature(obj="GatingHierarchy",y="character"),function(obj,y){
	g<-getGate(obj,y);
	if(length(g@parameters)==1){
		rbind(g@min,g@max)
	}else{
		g@boundaries
	}
})

#Bug here when the GatingSet has a mix of compensated and uncompensated data.. maybe need a isCompensated method..
.isCompensated<-function(x){
    flowCore:::checkClass(x,"GatingHierarchy")
	comp<-getCompensationMatrices(x)@spillover
	
    !(is.null(rownames(comp))&identical(comp%*%comp,comp))
}
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






#Taken from limma (don't want to import and create a dependency)
trimWhiteSpace<-function (x) 
{
    sub("[ \t\n\r]*$", "", sub("^[ \t\n\r]*", "", x))
}

setMethod("getSamples","flowJoWorkspace",function(x){
	.getSamples(x@doc)
})
###add support for win version
setMethod("getSampleGroups","flowJoWorkspace",function(x){
#			browser()
			win<-!x@version=="2.0"
			.getSampleGroups(x@doc,win)
		})

###add support for win version
.getSampleGroups<-function(x,win=FALSE){
	if(!win){
		do.call(rbind,xpathApply(x,"/Workspace/Groups/GroupNode",function(x){
							gid<-c(xmlGetAttr(x,"name"),xmlGetAttr(x,"groupID"));
							sid<-do.call(c,xpathApply(x,".//SampleRef",function(x){
												as.numeric(xmlGetAttr(x,"sampleID"))
											}))
							if(is.null(sid)){
								sid<-NA;
							}
							groups<-na.omit(data.frame(groupName=gid[[1]],groupID=as.numeric(gid[2]),sampleID=as.numeric(sid)));
						}))
	}else{
		#Note that groupID is from order of groupNode instead of from xml attribute 
		counter<-1
		do.call(rbind,xpathApply(x,"/Workspace/Groups/GroupNode",function(x){
#							browser()
							gid<-c(xmlGetAttr(x,"name"),xmlGetAttr(x,"groupID"));
							sid<-do.call(c,xpathApply(x,".//SampleRef",function(x){
												as.numeric(xmlGetAttr(x,"sampleID",default=NA))
											}))
							if(is.null(sid)){
								sid<-NA;
							}
#			browser()
							groups<-na.omit(data.frame(groupName=gid[[1]],groupID=counter,sampleID=as.numeric(sid)));
							counter<-counter+1
							groups
						}))
	}
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
#	browser()
	pop.counts<-as.numeric(unlist(lapply(xpathApply(top,"/Workspace/SampleList/Sample"),function(x)xpathApply(x,"count(descendant::Population)")),use.names=FALSE))
		if(!win){
			cid<-as.numeric(paste(xpathApply(top,"/Workspace/SampleList/Sample",function(x)xmlGetAttr(x,"compensationID"))))
			s<-data.frame(s,cid,pop.counts)
			colnames(s)<-c("sampleID","name","count","compID","pop.counts");
		}else{
			##Code for flowJo windows 1.6 xml
			#No compensation ID for windows. Use name
			s<-data.frame(s,pop.counts)
			colnames(s)<-c("sampleID","name","count","pop.counts");
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
	u<-as.list(unique(unlist(lapply(sapply(nms,function(y)strsplit(y,"<")),function(y).trimWhiteSpace(y[1])),use.names=FALSE)))
	T<-lapply(u,function(y).getCalibrationTableSearch(x@doc,y))
	names(T)<-u
	return(T)
})

.getCompensationMatrices<-function(z){
	top<-xmlRoot(z);
	matrices<-xpathApply(top,"/Workspace/CompensationMatrices/CompensationMatrix",function(x)xmlAttrs(x)[["name"]]);
	cmats<-lapply(matrices,function(mat){
	cmat<-as.numeric(unlist(xpathApply(top,paste("/Workspace/CompensationMatrices/CompensationMatrix[@name='",mat,"']/Channel/ChannelValue",sep=""),xmlGetAttr,"value"),use.names=FALSE))
	d<-sqrt(length(cmat));
	cmat<-matrix(cmat,d,byrow=TRUE)
	colnames(cmat)<-matrix((unlist(xpathApply(top,paste("/Workspace/CompensationMatrices/CompensationMatrix[@name='",mat,"']/Channel/ChannelValue",sep=""),xmlGetAttr,"name"),use.names=FALSE)),byrow=TRUE,d)[1,]
	rownames(cmat)<-matrix((unlist(xpathApply(top,paste("/Workspace/CompensationMatrices/CompensationMatrix[@name='",mat,"']/Channel/ChannelValue",sep=""),xmlGetAttr,"name"),use.names=FALSE)),byrow=TRUE,d)[1,]
	cmat;
	})
	names(cmats)<-matrices;
	cmats;
}

.getCalibrationTableNames<-function(x){
	top<-xmlRoot(x)
	tblnames<-xpathApply(top,"/Workspace/CalibrationTables/node()",function(x)xmlGetAttr(x,"name"))
	unlist(tblnames,use.names=FALSE);
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

	
