##Methods to add a gate to a GatingSet or GatingHierarchy below a certain node in the tree (or at the root).
#
#setMethod("addGate",signature=c("GatingHierarchy","polygonGate","character"),definition=function(obj,gate=NULL,parent=NULL,nname=NULL,negated=FALSE,...){
#	if(!parent%in%c(getNodes(obj),"ROOT")){
#		stop("parent ",parent," not in gating hierarchy ",obj@name);
#	}
#	return(.addGate(obj,gate=gate,parent=parent,nname=nname,negated=negated))
#})
#.addGate<-function(obj,gate=NULL,nname=NULL,parent=NULL,negated=FALSE,...){
#	if(is.null(nname)){
#		nname<-gate@filterId
#		nname<-paste(nname,c("+"),sep="")
#	}
#	t<-graph:::addNode(nname[1],obj@tree)
#	if(parent!="ROOT"){
#		t<-graph:::addEdge(parent,nname[1],t)
#	}
#	obj@tree<-t
#	setGate(obj,nname[1])<-gate
#	obj@nodes<-c(obj@nodes,nname[1])
#	#set metadata
#	m<-new.env(parent=emptyenv())
#	mpar<-obj@tree@nodeData@data[[parent]]$metadata
#	copyEnv(mpar,m)
#	ptot<-mpar[["thisTot"]]
#	fjName<-paste(mpar[["fjName"]],nname[1],collapse="/")
#	namvals<-list(count=NA_character_,thisIndices=list(),parentTot=ptot,thisTot=NA_character_,isGated=FALSE,isBooleanGate=FALSE,fjName=fjName,negated=negated)
#	multiassign(names(namvals),namvals,m)
#	nodeData(obj@tree,nname[1],"metadata")<-m
#	return(obj)
#}
#
##setMethod("addGate",signature=c("GatingHierarchy","rangeGate","character"),definition=function(obj,gate=NULL,parent=NULL){
##	if(!parent%in%c(getNodes(obj),"ROOT")){
##		stop("parent ",parent," not in gating hierarchy ",obj@name);
##	}
##})
#
#setMethod("addGate",signature=c("GatingHierarchy","rectangleGate","character"),definition=function(obj,gate=NULL,parent=NULL,nname=NULL,negated=FALSE,...){
#	if(!parent%in%c(getNodes(obj),"ROOT")){
#		stop("parent ",parent," not in gating hierarchy ",obj@name);
#	}
#	return(.addGate(obj,gate=gate,parent=parent,nname=nname,negated=negated))
#})
#
##setMethod("addGate",signature=c("GatingHierarchy","quadrantGate","character"),definition=function(obj,gate=NULL,parent=NULL){
##	if(!parent%in%c(getNodes(obj),"ROOT")){
##		stop("parent ",parent," not in gating hierarchy ",obj@name);
##	}
##})
#
#setMethod("addGate",signature=c("GatingHierarchy","quadGate","character"),definition=function(obj,gate=NULL,parent=NULL,nname=NULL,negated=FALSE,...){
#	if(!parent%in%c(getNodes(obj),"ROOT")){
#		stop("parent ",parent," not in gating hierarchy ",obj@name);
#	}
#	return(.addGate(obj,gate=gate,parent=parent,nname=nname,negated=negated,...))
#})
#
#setMethod("addGate",signature=c("GatingSet","polygonGate","numeric"),definition=function(obj,gate=NULL,parent=NULL,negated=FALSE,...){
#	p<-gsub("^*\\.","",unlist(lapply(obj,function(x)getNodes(x)[parent]),use.names=FALSE))
#	stopifnot(all(p[1]==p))
#	
#	#TODO finish method
#})
#
##setMethod("addGate",signature=c("GatingSet","rangeGate","numeric"),definition=function(obj,gate=NULL,parent=NULL){
##	p<-gsub("^*\\.","",unlist(lapply(obj,function(x)getNodes(x)[parent]),use.names=FALSE))
##	stopifnot(all(p[1]==p))
##	
##})
#
#setMethod("addGate",signature=c("GatingSet","rectangleGate","numeric"),definition=function(obj,gate=NULL,parent=NULL,negated=FALSE,...){
#	p<-gsub("^*\\.","",unlist(lapply(obj,function(x)getNodes(x)[parent]),use.names=FALSE))
#	stopifnot(all(p[1]==p))
#	#TODO finish method
#})
#
##setMethod("addGate",signature=c("GatingSet","quadrantGate","numeric"),definition=function(obj,gate=NULL,parent=NULL){
##	p<-gsub("^*\\.","",unlist(lapply(obj,function(x)getNodes(x)[parent]),use.names=FALSE))
##	stopifnot(all(p[1]==p))
##	
##})
#
#setMethod("addGate",signature=c("GatingSet","quadGate","numeric"),definition=function(obj,gate=NULL,parent=NULL,negated=FALSE,...){
#	p<-gsub("^*\\.","",unlist(lapply(obj,function(x)getNodes(x)[parent]),use.names=FALSE))
#	stopifnot(all(p[1]==p))
#	#TODO finish method
#})
#
#
#
#setGeneric("setGate<-",function(this,node,value)standardGeneric("setGate<-"))
#setReplaceMethod("setGate",c("GatingHierarchy","character"),function(this,node,value){
#	env<-nodeData(this@tree,node,"metadata")[[1]]
#	stopifnot(inherits(value,"polygonGate")|inherits(value,"rectangleGate"))
#	assign("gate",value,env=env)
#	this
#})
#
##Copy the gating hierarchy in GatingSet a to GatingSet b
##Useful right before a combine
#
##This function doesn't verify that the assigned environment is valid. Should not be called by the user.
##You could seriously break the data structure
#setGeneric("setDataEnv",function(this,value)standardGeneric("setDataEnv"))
#setMethod("setDataEnv",c("GatingHierarchy","environment"),function(this,value){
#	assign("data",value,env=nodeDataDefaults(this@tree,"data"))
#})
#
#setGeneric("getDataEnv",function(this)standardGeneric("getDataEnv"))
#setMethod("getDataEnv",c("GatingHierarchy"),function(this){
#	nodeDataDefaults(this@tree,"data")[["data"]]
#})
#
#setMethod("setData",c("GatingHierarchy","flowSet"),function(this,value){
#	if(inherits(value,"ncdfFlowSet")){
#		assign("ncfs",value,nodeDataDefaults(this@tree,"data")[["data"]])
#	}else{
#		stop("error: wrong class")
#	}
#})
#setMethod("setData",c("GatingHierarchy","flowFrame"),function(this,value){
#		assign("data",value,nodeDataDefaults(this@tree,"data"))
#})
#
