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
        #convert to polygon gate
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
      expr<-x@deparse
      pattern<-"&|\\|"
      #get the position of logical operators
      op_ind<-unlist(gregexpr(pattern=pattern,expr))
      #extract these operators
      op<-flowWorkspace:::trimWhiteSpace(substring(expr,op_ind,op_ind))
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
      refs<-unlist(strsplit(expr,split=pattern)) 
      refs<-trimWhiteSpace(refs)
      #extract is not operator
      isNot <- as.logical(regexpr("!",refs)+1) 
      #strip is not operator from node elements
      refs <- gsub("!","",refs)
      
      nNodes<-length(refs)
      if(length(isNot)!=nNodes)
        stop("the number of ! operators are inconsistent with nodes!")
      if(length(op)!=nNodes)
        stop("the number of logical operators are inconsistent with nodes!")
      list(type=as.integer(3)
          ,refs=refs
          ,isNot=isNot
          ,op=op
          ,filterId=x@filterId)  
    })

