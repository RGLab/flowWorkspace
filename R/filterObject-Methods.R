#' @include GatingSet-Methods.R
NULL

# convert flowCore filter to a list (filterObject) 
# that can be understood by c++ code
setMethod("filterObject",signature=c("rectangleGate"),function(x,...){
      params<-parameters(x)
      
      if(length(params)==1)
      {
        #convert to rangeGate
        filterObject<-list(type=as.integer(2)
            ,params=params
            ,range=c(x@min,x@max) 
            ,filterId=x@filterId
        )
        
      }else
      {
        #convert to rectGate gate
        mat<-rbind(x@min,x@max)
        filterObject<-list(type=as.integer(5)
            ,params=params
            ,boundaries=mat
            ,filterId=x@filterId)  
      }
      filterObject
    })


setMethod("filterObject",signature=c("polygonGate"),function(x,...){
      params<-parameters(x)
      
      list(type=as.integer(1)
          ,params=params
          ,boundaries=x@boundaries
          ,filterId=x@filterId)  
    })

setMethod("filterObject",signature=c("booleanFilter"),function(x,...){
      expr <- x@deparse
      
      pattern <- "&&|\\|\\|"
      if(grepl(pattern=pattern,expr))
        stop("double operater ('&&' or '||') found in the expression of booleanFilter!")
      
      pattern <- "&|\\|"
      #get the position of logical operators
      op_ind <- unlist(gregexpr(pattern=pattern,expr))
      #extract these operators
      op <- flowWorkspace:::trimWhiteSpace(substring(expr,op_ind,op_ind))
      ##append & for the first node element(as C parser convention requires)
      if(length(op)==1){
        if(nchar(op)==0){
          op <- "&"
        }else{
          op <- c("&",op)
        }
      }else{
        op <- c("&",op)        
      }

      #split into node elements by operators
      refs <- unlist(strsplit(expr,split=pattern)) 
      refs <- trimWhiteSpace(refs)
      #extract the leading ! operator from each ref
      isNot <- as.logical(regexpr("!",refs) + 1) 
      #strip ! symbol from node elements
      refs <- sub("!","",refs)
      
      #check if there is still illegal ! symbol left
      nNodes <- length(refs)
      if(any(grepl("!", refs)))
        stop("extra '!' symbol found in the reference node names of boolean fitler!")
      if(length(op)!=nNodes)
        stop("the number of logical operators are inconsistent with nodes!")
      list(type=as.integer(3)
          ,refs=refs
          ,isNot=isNot
          ,op=op
          ,filterId=x@filterId)  
    })

setMethod("filterObject",signature=c("ellipsoidGate"),function(x,...){
      params<-parameters(x)
      
      list(type=as.integer(4)
          , params = params
          , mu = x@mean
          , cov = x@cov
          , dist = x@distance
          ,filterId=x@filterId)  
    })