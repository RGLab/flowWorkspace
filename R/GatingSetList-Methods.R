
setMethod("rbind2",
    signature=signature("GatingSetList","missing"),
    definition=function(x,y="missing",...)
    {
#           browser()
      isNcdfList<-lapply(x,function(gs)flowWorkspace:::isNcdf(gs[[1]]))
      if(all(duplicated(unlist(isNcdfList))[-1])){
#               browser()
        #combine flowset/ncdfFlowSet
        fsList<-lapply(x,getData)
        if(isNcdfList[[1]])
          fs<-rbind2(as(fsList,"ncdfFlowList"),...)
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
        pointer<-.Call("R_combineGatingSet",ptrlist,sampleList,package="flowWorkspace")
        G<-new("GatingSetInternal")
        G@pointer<-pointer
        G@guid<-flowWorkspace:::.uuid_gen()
        
        #combine R objects
        ne<-new.env(parent=emptyenv());
        assign("ncfs",fs,envir=ne)
        set<-unlist(lapply(x,function(gs)gs@set))
        #deep copying of tree
        for(i in seq_along(set))
        {
          #create new local data environment that stores axis and flowData environment
          localDataEnvOld<-nodeDataDefaults(set[[i]]@tree,"data")
          localDataEnv<-new.env(parent=emptyenv())
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
      cat("A GatingSetList with", length(object@data),"GatingSet\n")
      cat("containing", length(unique(getSamples(object))), " unique samples.") 
      cat("\n")
    })
setMethod("getSamples", 
    signature = signature(x = "GatingSetList"),
    function(x,...) {
      x@samples      
    })

setMethod("lapply","GatingSetList",function(X,FUN,...){
      lapply(X@data,FUN,...)
    })

setMethod("[[",c(x="GatingSetList",i="numeric"),function(x,i,j,...){
      #convert non-character indices to character
#      browser()
      this_samples <- getSamples(x)
      nSamples <- length(this_samples)
      if(i > nSamples){
        stop(i, " is larger than the number of samples: ", nSamples)
      }
        x[[this_samples[i]]]
      
    })

setMethod("[[",c(x="GatingSetList",i="logical"),function(x,i,j,...){
      #convert non-character indices to character
      
      x[[getSamples(x)[i]]]
      
    })
setMethod("[[",c(x="GatingSetList",i="character"),function(x,i,j,...){
      #convert non-character indices to character
      gh <- NULL
      for(gs in x@data){
#              browser()
            this_samples <- getSamples(gs)
            ind <- match(i,this_samples)
            if(!is.na(ind)){
              gh <- gs[[ind]]
            }
      }
      if(is.null(gh)){
        stop(i, " not found in GatingSetList!")
      }else{
        return (gh)
      }
    })
setMethod("[",c(x="GatingSetList",i="numeric"),function(x,i,j,...){
#      browser()
      x[getSamples(x)[i]]
      
    })

setMethod("[",c(x="GatingSetList",i="logical"),function(x,i,j,...){
      
      x[getSamples(x)[i]]
   
    })

setMethod("[",c(x="GatingSetList",i="character"),function(x,i,j,...){
#      browser()
      samples <- getSamples(x)
      matchInd <- match(i,samples)
      noFound <- is.na(matchInd)
      if(any(noFound)){
        stop(i(noFound), "not found in GatingSetList!")
      }
      res <- lapply(x,function(gs){
#            browser()
                  this_samples <- getSamples(gs)
                  ind <- match(i,this_samples)
                  this_subset <- i[!is.na(ind)] 
                  if(length(this_subset)>0){
                    return (gs[this_subset])
                  }else{
                    NULL
                  }
                })
      res <- res[!unlist(lapply(res,is.null))]
      res <- GatingSetList(res)
      res@samples <- samples[matchInd]
      res
    })

setMethod("getData",c(obj="GatingSetList",y="missing"),function(obj,y,...){
      stop("node index 'y' is missing!")
    })
#
setMethod("getData",signature(obj="GatingSetList",y="numeric"),function(obj,y,...){
#      browser()
      this_node <- getNodes(obj[[1]])[y]
      getData(obj,this_node)
    })
setMethod("getData",c(obj="GatingSetList",y="character"),function(obj, y, max=30, ...){
#      browser()
      if(length(getSamples(obj))>max){
        stop("You are trying to return a flowSet for more than ", max, " samples!Try to increase this limit by specifing 'max' option if you have enough memory.")
      }
#      browser()
      res <- lapply(obj,function(gs){
                NcdfFlowSetToFlowSet(getData(gs,y))
          })
      fs<-res[[1]]
      if(length(res)>1){
        for(i in 2:length(res))
          fs<-rbind2(fs,res[[i]])  
      }
      
      fs
    })
setMethod("getData",signature=c("GatingSetList","name"),function(obj, y, pop_marker_list = list(), ...){
      pop_chnl<- .getPopChnlMapping(obj[[1]],y,pop_marker_list)
      this_pops <-  as.character(pop_chnl[,"pop"])
      this_chnls <- as.character(pop_chnl[,"name"])
#      browser()
      sapply(getSamples(obj),function(this_sample){
            message(this_sample)
            gh <- obj[[this_sample]]       
            #get mask mat
#      browser()
            
            this_mat <- getIndiceMat(gh,y)[,this_pops, drop=FALSE]
            #get indices of bool gates 
            this_ind <- this_mat[,1]
            for(i in 2:ncol(this_mat)){
                    
              this_ind <- this_ind |this_mat[,i]
                      
                }
            if(sum(this_ind)==0){
              NULL
            }else{
              this_mat <- this_mat[this_ind,,drop = FALSE]
              #subset data by channels selected
              
              this_data <- getData(gh)
              this_subset <- exprs(this_data)[this_ind,this_chnls, drop=FALSE] 
              #masking the data
              this_subset <- this_subset *  this_mat
              colnames(this_subset) <- pop_chnl[,"desc"]
              this_subset  
            }
            
          })  
  
  })

setMethod("getIndices",signature=c("GatingSetList","name"),function(obj, y, ...){
    
    })
setMethod("pData","GatingSetList",function(object){

      res <- lapply(object,function(gs){
              pData(gs)      
          })
#            browser()
      res <- do.call(rbind,res)
      res[object@samples,,drop=FALSE]
    })

setReplaceMethod("pData",c("GatingSetList","data.frame"),function(object,value){
      if(!.isValidSamples(rownames(value),object))
        stop("The sample names in data.frame are not consistent with the GatingSetList!")
        
      res <- lapply(object,function(gs){
                    this_pd <- subset(value,name%in%getSamples(gs))
                    pData(gs) <- this_pd
                    gs
                  })
              
      res <- GatingSetList(res)
      res        
    })

setMethod("getGate",signature(obj="GatingSetList",y="numeric"),function(obj,y,tsort=FALSE){
      getGate(obj,getNodes(obj[[1]])[y])
    })
setMethod("getGate",signature(obj="GatingSetList",y="character"),function(obj,y,tsort=FALSE){
      res <- lapply(obj,function(gs){
            getGate(gs,y)      
          })
      unlist(res,recur=FALSE)
      
    })


setMethod("plotGate",signature(x="GatingSetList",y="numeric"),function(x,y, ...){
      selectMethod("plotGate",sig=c(x="GatingSetInternal",y="numeric"))(x=x, y=y, ...)
      
      
    })


setMethod("getPopStats","GatingSetList",function(x,...){
      res <- lapply(x,getPopStats,...)
      do.call(cbind,res)
    })

save_gslist<-function(gslist,path,...){
    
#  if(file.exists(path)){
#    path <- normalizePath(path,mustWork = TRUE)
#    if(overwrite){
#      res <- unlink(path, recursive = TRUE)
#      if(res == 1){
#        stop("failed to delete ",path)
#      }
#    }else{
#      stop(path,"' already exists!")  
#    }
#    
#  }
  if(!file.exists(path))
    dir.create(path = path)
  #do the dir normalization again after it is created
  path <- normalizePath(path,mustWork = TRUE)
  lapply(gslist,function(gs){
#        this_dir <- tempfile(pattern="gs",tmpdir=path)
#        dir.create(path = this_dir)
#        browser()
        guid <- gs@guid
        if(length(guid)==0){
          gs@guid <- flowWorkspace:::.uuid_gen()
          guid <- gs@guid
        }
        this_dir <- file.path(path,guid) 

#        invisible(flowWorkspace:::.save_gs(gs,path = this_dir, ...))
        suppressMessages(save_gs(gs,path = this_dir, ...))
      })
#  browser()
  #save sample vector
  saveRDS(gslist@samples,file=file.path(path,"samples.rds"))
  message("Done\nTo reload it, use 'load_gslist' function\n")
  
  
}


load_gslist<-function(path){
#  browser()
  path <- normalizePath(path,mustWork = TRUE)
  if(!file.exists(path))
    stop(path,"' not found!")
  dirs<-list.dirs(path,full=TRUE, recursive = FALSE)
#   browser()
  res <- lapply(dirs,function(this_dir){
#        browser()
        flowWorkspace:::.load_gs(output = this_dir, files = list.files(this_dir))$gs      
      })
  samples <- readRDS(file.path(path,"samples.rds"))
  GatingSetList(res, samples = samples)
  
}
