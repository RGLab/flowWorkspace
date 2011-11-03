#All class definitions
setOldClass("XMLInternalDocument")
setClass("flowJoWorkspace",representation(version="character",file="character",.cache="environment",path="character",doc="XMLInternalDocument"))

setClass("GatingHierarchy",representation(tree="graphNEL",nodes="character",name="character",flag="logical",transformations="list",compensation="matrix",dataPath="character",isNcdf="logical"))

setMethod("initialize","GatingHierarchy",function(.Object,tree=new("graphNEL",edgemode="directed"),nodes=character(),name="New Sample",flag=FALSE,transformations=rep(list({f<-function(x){x};attr(f,"type")<-"identity";f}),8),compensation=diag(8),dataPath=".",isNcdf=FALSE,fcsfile=NULL){
	#callNextMethod(.Object,tree,nodes,name,flag,transformations,compensation,dataPath,isNcdf)
	m<-new.env()
	
	multiassign(c("compID","fcsfile","fjName","gate","negated","isBooleanGate","thisIndices","parentTot","thisTot","isGated","keywords"),list(NULL,fcsfile,fcsfile,NA,FALSE,FALSE,list(),NA,0,FALSE,NULL),env=m)
	nodeDataDefaults(tree,"metadata")<-m
	d<-new.env();
	data<-new.env();
	assign("data",data,env=d);
	if(!isNcdf){
		assign("data",new("flowSet"),env=data)
	}else{
		assign("ncfs",new("ncdfFlowSet"),env=data);
	}
	nodeDataDefaults(tree,"data")<-d
	.Object@nodes<-nodes
	.Object@tree<-tree
	.Object@name<-name
	if (!is.null(fcsfile)&name=="New Sample"){
		.Object@name<-fcsfile
	}
	.Object@flag<-flag
	.Object@transformations<-transformations
	.Object@compensation<-compensation
	.Object@dataPath<-dataPath
	.Object@isNcdf<-isNcdf
	validObject(.Object)
	return(.Object)
})
setClass("GatingSet",representation(set="list",metadata="AnnotatedDataFrame"),validity=function(object){
	all(unlist(lapply(object@set,function(y)inherits(y,"GatingHierarchy"))))
})
setMethod("initialize","GatingSet",function(.Object,set=list(new("GatingHierarchy")),metadata=new("AnnotatedDataFrame"),files=NULL,isNcdf=FALSE,flowSetId=NULL){
	if(!is.null(files)){
		if(isNcdf){
			stopifnot(length(grep("ncdfFlow",loadedNamespaces()))!=0)
			fs<-read.ncdfFlowSet(files,flowSetId=ifelse(is.null(flowSetId),"New FlowSet",flowSetId))
		}else{
			fs<-read.flowSet(files)
		}
		set<-vector("list",length(fs))
		for(i in seq_along(fs)){
			set[[i]]<-new("GatingHierarchy",fcsfile=files[i],dataPath=dirname(files[i]),name=basename(files[i]),isNcdf=isNcdf)
			setData(set[[i]],fs)
		}
	}
	.Object@set<-set;
	.Object@metadata<-metadata
	validObject(.Object)
	return(.Object)
})


