validitycheck <- function(gs_list){
  for(gs in gs_list)
  {
    stopifnot(is(gs, "GatingSet"))
  }
  
  #check overlapping samples
  gs_samples <- unlist(lapply(gs_list, sampleNames))
  if(any(duplicated(gs_samples))){
    stop ("There are overlapping samples across GatingSets!")
  }
  
  
  gs1 <- gs_list[[1]]
  
  #compare GatingSets
  
  res <- sapply(gs_list[-1],function(this_gs){
    
    .compareGatingSet(this_gs,gs1)
  })
  
  
  is_error <- sapply(res,function(this_res){
    is.na(as.logical(this_res))
  })
  if(any(is_error)){
    this_error_ind <- which(is_error)[1]
    stop (paste("GatingSet 1 and",this_error_ind+1,":",res[this_error_ind]))
  }
  
  
}

.getNodes_removeHidden <- function(gh){
  complete <- gs_get_pop_paths(gh, showHidden = TRUE)
  sub <- gs_get_pop_paths(gh, showHidden = FALSE)
  hiddenInd <- which(!complete%in%sub)
  #remove hidden node from paths
  for(i in hiddenInd){
    thisHidden <- complete[i]
    hiddenPopName <- basename(thisHidden)
    parent <- gs_pop_get_parent(gh, thisHidden)
    sub <- gsub(thisHidden, parent, sub)
  }
  sub
}        
#TODO:gating tree comparison needs to be improved 
# can't use RBGL::isomorphism since it will not take care of the hidden nodes
.compareGatingHierarchy<-function(gh1,gh2){
  tree1 <- .getNodes_removeHidden(gh1)
  tree2 <- .getNodes_removeHidden(gh2)
  
  if(setequal(tree1, tree2)){
    return (TRUE)
  }else{
    return (paste("gating structure doesn't match:",sampleNames(gh1),sampleNames(gh2)))
  }
}
.compareFlowData <- function(fs1,fs2){
  #it is strange that colnames doesn't dispatch properly without namespace prefix
  col1 <- flowCore::colnames(fs1)
  col2 <- flowCore::colnames(fs2)
  if(!setequal(col1,col2)){
    msg <- paste("colnames of cyto data don't match!")
    return (msg)
  }
  if(!setequal(colnames(pData(fs1)),colnames(pData(fs2)))){
    return ("pData of cyto data doesn't match!")
  }
  return  (TRUE)
  
}
.compareGatingSet<-function(gs1,gs2){
  gh1 <- gs1[[1]]
  gh2 <- gs2[[1]]
  res <- .compareGatingHierarchy(gh1,gh2)
  if(class(res) == "character"){
    return (res)
  }
  fs1 <- gs_pop_get_data(gs1)
  fs2 <- gs_pop_get_data(gs2)
  .compareFlowData(fs1,fs2)
}



#' Merge a list of GatingSets into a single GatingSet
#' 
#' It also checks the consistency of the cyto data and gates.
#' 
#' @param x a list of GatingSets
#' @param ... other arguments (not used)
#' @export 
merge_list_to_gs <- function(x,...){
  if(is(x, "GatingSetList"))
    x <- x@data
  stopifnot(is(x, "list"))
  validitycheck(x)
  
  # make sure the column names of flow data are in the same order
  cols <- flowCore::colnames(gs_cyto_data(x[[1]]))
  for(gs in x)
  {
    gs_cyto_data(gs) <- gs_cyto_data(gs)[,cols]
  }
  
  #combine tree structure
  ptrlist <- lapply(x,function(gs)gs@pointer)
  sampleList <- lapply(x, sampleNames)
  new("GatingSet", pointer = cpp_combineGatingSet(ptrlist,sampleList))
}
