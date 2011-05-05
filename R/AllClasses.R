#All class definitions
setOldClass("XMLInternalDocument")
setClass("flowJoWorkspace",representation(version="character",file="character",.cache="environment",path="character",doc="XMLInternalDocument"))

setClass("GatingHierarchy",representation(tree="graphNEL",nodes="character",name="character",flag="logical",transformations="list",compensation="matrix",dataPath="character"))

setClass("GatingSet",representation(set="list",metadata="AnnotatedDataFrame"),validity=function(object){
	all(unlist(lapply(object@set,function(y)inherits(y,"GatingHierarchy"))))
})


setClass("ncdfFlowSet",
		representation = representation(
				file = "character",
				sampleNames = "character",
				colNames = "character",
				stains = "character",
				parameters = "list",
				description = "list",
				phenoData = "AnnotatedDataFrame",
				flowSetId = "character"),
		prototype = list(phenoData = new("AnnotatedDataFrame", data = data.frame(), 
						varMetadata = data.frame()),
				sampleNames = character(0),
				colNames = character(0),
				stains = character(0),
				parameters = list(),
				description = list(),
				flowSetId = character(0)),
		validity = function(object){
			
		})

