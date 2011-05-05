if(TRUE){
.guid <- function(len=10){
       ltrs <- c(LETTERS,letters)
       paste(c(sample(ltrs,1),sample(c(ltrs,0:9),len-1,replace=TRUE)),collapse="")
}	


.ncdfFlowSet <- function(file, sampleNames, colNames, stains, parameters, 
        description, phenoData, flowSetId) {
    
     nc <- new("ncdfFlowSet", file = file, sampleNames = sampleNames, 
             colNames = colNames, stains = stains, parameters = parameters,
             description = description, phenoData = phenoData, flowSetId = flowSetId)
     names(nc@parameters) <- names(nc@description) <- sampleNames(nc)
     nc
}


###when path is missing,files contains the fullpath of fcs files
###otherwise,files only contains the basename of the files
createNcdfFlowSet <- function(files = NULL, ncdfFile,flowSetId, 
                     path, compress = TRUE,isWriteSlice= TRUE,...) 
{

    if(missing(ncdfFile)) 
        ncdfFile = .guid() 

    if (!length(grep(".", ncdfFile, fixed = TRUE)))  
        ncdfFile <- paste(ncdfFile, "nc", sep = ".")

    phenoFrame <- NULL
    if(!is.character(files))
        stop("'files' must be a character vector.")
    file.names <- basename(files) ## strip path from names
	if(!missing(path)&&path != ".")###if path is missing,consider files as full path
        files <- file.path(path, files)    
    
 

    ## obtain event counts and  number of parameters
    bigFile <- files[which.max(file.info(files)[,"size"])]
    tmp <- read.FCSheader(bigFile)[[1]]
    maxEvents <- as.integer(tmp[["$TOT"]])
    maxChannels <- as.integer(tmp[["$PAR"]])
    maxSamples <- length(files)
    ### read the big fcs file to get the channel names 
    tmp  <- read.FCS(bigFile)
    channelNames <- as.character(as.character(parameters(tmp)@data[,"name"])) 

    msgCreate <- try(.Call("createFile", ncdfFile, as.integer(maxEvents), 
                     as.integer(maxChannels), as.integer(maxSamples),
                     as.logical(compress)),
                     silent = TRUE)
    if(is(msgCreate, "try-error")) {
            message("Creating netCDF file failed\n")
            message(msgCreate[1])
            return(NULL)
    }
     
    pars <- lapply(seq_len(maxSamples), function(i, verbose){
           tmp <- read.FCS(files[i])
			###################################################
			#when isWriteSlice is False,keep the ncdf matrix emply
			###################################################			
			if(isWriteSlice)
			{
				mat <- exprs(tmp)
				mode(mat) <- "single"
				msgWrite <- try(.Call("writeSlice", ncdfFile, mat , as.integer(i)), silent = TRUE)
            	if(is(msgWrite, "try-error")) {
                    msg <- paste("Write to netCDF failed for sample", i, sep =" ")
                    message(msg)
                    message(msgWrite[1])
                    return(NULL)
            	}
			}
            list("description" = description(tmp), "parameters" = parameters(tmp), 
                 "guids" = identifier(tmp))

        }, verbose = TRUE)
    parameters <- list()
    guids <- list()
    description <- list()
    for(i in seq_len(maxSamples)) {
        parameters[[i]] <- pars[[i]][["parameters"]]
        guids[[i]] <- pars[[i]][["guids"]]
        description[[i]] <- pars[[i]][["description"]]
    }

    
        guids <- basename(files)
        phenoData = new("AnnotatedDataFrame", data = data.frame(), 
                        varMetadata = data.frame())
    
    if(any(duplicated(guids)))
        guids <- make.unique(guids)
    names(parameters) <- guids
    ## Check to make sure the parameter values match
    #.checkPars(parameters)
    #stains <- as.character(NA)
    #suppressWarnings(
    #   if(.checkVals(parameters, "desc"))
        stains <- as.character(pData(parameters[[1]])[,"desc"])
    
    .ncdfFlowSet(file = ncdfFile, flowSetId = flowSetId, sampleNames = guids, colNames = channelNames,
           stains = stains, parameters = parameters, description = description,
           phenoData = phenoData) 
}

#setMethod("addData",
#		signature=signature(ncfs="ncdfFlowSet",object="flowFrame"),
#		definition=function(ncfs,object,sampleName)
#		{ 
#			ncdfFlow:::.addSlice(ncfs,object,sampleName)
#				
#		}
#)

.addSlice <- function(ncfs,data,sampleName)
{ 
				mat <- exprs(data)
				mode(mat) <- "single"
				
				i<-which(sampleNames(ncfs)==sampleName)
				msgWrite <- try(.Call("writeSlice", ncfs@file, mat , as.integer(i)), silent = TRUE)
				if(is(msgWrite, "try-error")) {
					msg <- paste("Write to netCDF failed for sample", i, sep =" ")
					message(msg)
					message(msgWrite[1])
					return(NULL)
				}
				msgWrite
}

.getFlowFrame <- function(ncfs,sampleName){
	val <- .ncdfExprs(ncfs, sampleName)   
	flowFrame(val,.parameters(ncfs, sampleName),description=.description(ncfs, sampleName))       
}

sampleNames<-function(ncfs)
{
	ncfs@sampleNames
}
.colnames<- function(x,prefix=prefix) {
            if(missing(prefix))
				x@colNames
			else
				x@parameters[[prefix]]@data$name
			            
        }

.parameters<-function(object, sample) {
            if(missing(sample))
                object@parameters
            else
                object@parameters[[sample]]
                
        }

.description<-function(object, sample) {
			if(missing(sample))
				object@description
			else
				object@description[[sample]]
			
		}


.retNcdfMat <- function(object, chIndx, samplePos){        
    ## chIndx is start and end positions
    mat <- try(.Call("readSlice", object@file, as.integer(chIndx),
            as.integer(samplePos)), silent = TRUE)
        if(is(mat, "try-error")) {
            msg <- paste("Reading from", object@file, "failed\n", sep =" ")
            message(msg)
            message(mat[1])
            return(NULL)
        }
    indx <- seq.int(chIndx[1], chIndx[2], 1)
	sampleName<-sampleNames(object)[samplePos]
    clNames <- .colnames(object,sampleName)[indx] ##FIXME
    colnames(mat) <- clNames
    mat
}

.ncdfExprs <- function(object, sample, channel) {
    chNames <- .colnames(object,sample)  ## FIXME
    sampNames <- sampleNames(object)
    if(!missing(channel)) {
        if(!all(channel %in% chNames))
            stop("All channels specified could not be found in the ncdfFlowSet")
        chPos <- which(chNames %in% channel)
        
    }else {
        channel <- chNames
        chPos <- 1:length(chNames)
    }
    chIndx <- c(min(chPos), max(chPos))
    if(!missing(sample)) {
        if(!all(sample %in% sampNames))
            stop("All channels specified could not be found in the ncdfFlowSet")
        sampIndx <- which(sampNames %in% sample)
       
        #   matRead <- .retNcdfMat(object, chIndx, sampIndx)[,channel,drop=FALSE]
     
    } else {
        message("Sample names is missing, data for all samples in ncdfFlowSet will be returned")
        sampIndx <- 1:length(sampNames)
    }
        matRead<- lapply(sampIndx, function(x){
                      .retNcdfMat(object, chIndx, x)[,channel, drop= FALSE]
                  })
    
    names(matRead) <- sampNames[sampIndx]
    if(length(sampIndx) == 1)
        return(matRead[[1]])
    else 
        return(matRead)
}





}
