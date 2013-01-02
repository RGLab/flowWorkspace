setMethod("rbind2",
		signature=signature("GatingSetList","missing"),
		definition=function(x,y="missing",...)
		{
#			browser()
			isNcdfList<-lapply(x,function(gs)flowWorkspace:::isNcdf(gs[[1]]))
			if(all(duplicated(unlist(isNcdfList))[-1])){
#				browser()
				#combine flowset/ncdfFlowSet
				fsList<-lapply(x,getData)
				if(isNcdfList[[1]])
					fs<-rbind2(ncdfFlowList(fsList),...)
				else
				{
					##using original flowCore::rbind2 for flowSet
					fs<-fsList[[1]]
					for(i in 2:length(fsList))
						fs<-rbind2(fs,fsList[[i]])
				}

				#combine tree structure
				ptrlist<-lapply(x,function(gs)gs@pointer)
				sampleList<-lapply(x,getSamples)
				pointer<-.Call("R_combineGatingSet",ptrlist,sampleList)
				G<-new("GatingSetInternal")
				G@pointer<-pointer
				
				#combine R objects
				ne<-new.env();
				assign("ncfs",fs,envir=ne)
				set<-unlist(lapply(x,function(gs)gs@set))
				#deep copying of tree
				for(i in seq_along(set))
				{
					#create new local data environment that stores axis and flowData environment
					localDataEnvOld<-nodeDataDefaults(set[[i]]@tree,"data")
					localDataEnv<-new.env()
					copyEnv(localDataEnvOld,localDataEnv)
					#update flowData environment with new ncfs
					assign("data",ne,localDataEnv)
					#sync back to tree
					nodeDataDefaults(set[[i]]@tree,"data")<-localDataEnv
					#upodate pointer
					set[[i]]@pointer<-pointer
				}
				
				G@set<-set
				
			}else{
				stop("Can't combine gating sets. They should all use the same storage method. (Netcdf, or not..)")
			}
			return(G);	
			
		})
			
setMethod("show",
		signature = signature(object="GatingSetList"),
		definition = function(object) { 
			cat("A GatingSetList with", length(object@.Data),"GatingSet\n")
			cat("containing", length(unique(getSamples(object))), " unique samples.") 
			cat("\n")
		})
setMethod("getSamples", 
		signature = signature(x = "GatingSetList"),
		function(x) {
			unlist(lapply(x,getSamples))      
		})