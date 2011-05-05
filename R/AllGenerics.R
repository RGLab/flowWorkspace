setGeneric("openWorkspace", function(file){
	standardGeneric("openWorkspace");
})
setGeneric("closeWorkspace",function(workspace){
	standardGeneric("closeWorkspace")
})
setGeneric("parseWorkspace",function(obj,...){
	standardGeneric("parseWorkspace")
})
setGeneric("getNodes",function(x,...){
	standardGeneric("getNodes");
})
setGeneric("flowWorkspace2flowCore",function(obj,...){
	standardGeneric("flowWorkspace2flowCore");
})

setGeneric("ellipsoidGate2FlowJoVertices",function(gate,...){
	standardGeneric("ellipsoidGate2FlowJoVertices");
})

setGeneric("execute",function(hierarchy,...){
	standardGeneric("execute")
})
setGeneric("plotGate",function(x,y,...){
	standardGeneric("plotGate")
})
# setGeneric("plotWf",function(x,...){
# 			standardGeneric("plotWf");
# 		})

setGeneric("getPopStats",function(x,...){
	standardGeneric("getPopStats");
})
setGeneric("plotPopCV",function(x,...){
	standardGeneric("plotPopCV");
})
setGeneric("getData",function(obj,...){
	standardGeneric("getData")
})
setGeneric("getGate",function(obj,y,...){
	standardGeneric("getGate");
})
setGeneric("getParent",function(obj,y,...){
	standardGeneric("getParent")
})
setGeneric("getAxisLabels",function(obj,y,...){
	standardGeneric("getAxisLabels")
})
setGeneric("getBoundaries",function(obj,y,...){
	standardGeneric("getBoundaries")
})

setGeneric("getDimensions",function(obj,y,...){
	standardGeneric("getDimensions");
})

setGeneric("getChildren",function(obj,y,...){
	standardGeneric("getChildren");
})
setGeneric("getIndices",function(obj,y,...){
	standardGeneric("getIndices");
})
setGeneric("getProp",function(x,y,...){
	standardGeneric("getProp");
})
setGeneric("getTotal",function(x,y,...){
	standardGeneric("getTotal");
})
setGeneric("getSamples",function(x,...){
	standardGeneric("getSamples");
})
setGeneric("getSample",function(x,...){
			standardGeneric("getSample");
		})
setGeneric("getSampleGroups",function(x){
	standardGeneric("getSampleGroups")
})
setGeneric("getCompensationMatrices",function(x){
	standardGeneric("getCompensationMatrices")
})
setGeneric("getTransformations",function(x){
	standardGeneric("getTransformations")
})
setGeneric("getKeywords",function(obj,y){
	standardGeneric("getKeywords")
})
setGeneric("exportAsFlowJoXML", function(obj, ...){
	standardGeneric("exportAsFlowJoXML")
})


