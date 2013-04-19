setGeneric("openWorkspace", function(file,...){
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
setGeneric("setNode",function(x,y,value,...){
      standardGeneric("setNode"); 
    })
setGeneric("flowWorkspace2flowCore",function(obj,...){
	standardGeneric("flowWorkspace2flowCore");
})

setGeneric("ellipsoidGate2FlowJoVertices",function(gate,...){
	standardGeneric("ellipsoidGate2FlowJoVertices");
})
setGeneric("haveSameGatingHierarchy",function(object1,object2){
	standardGeneric("haveSameGatingHierarchy");
})
setGeneric("addGate",function(obj,gate,parent,...){
	standardGeneric("addGate");	
})


#to deprecated by "flowSet"
setGeneric("getNcdf",function(obj){
	standardGeneric("getNcdf")
})

#to  be  deprecated by "flowSet"
setGeneric("ncFlowSet", function(x) standardGeneric("ncFlowSet"))
#to  be  deprecated by "flowSet"
setGeneric("ncFlowSet<-", function(x,value) standardGeneric("ncFlowSet<-"))

setGeneric("flowSet")
setGeneric("flowSet<-", function(x,value) standardGeneric("flowSet<-"))

setGeneric("getIndiceFile",function(obj){
			standardGeneric("getIndiceFile")
		})

setGeneric("execute",function(hierarchy,...){
	standardGeneric("execute")
})
setGeneric("plotGate",function(x,y,...){
	standardGeneric("plotGate")
})

setGeneric("getPopStats",function(x,...){
	standardGeneric("getPopStats");
})
setGeneric("plotPopCV",function(x,...){
	standardGeneric("plotPopCV");
})
setGeneric("getData",function(obj,y,...){
	standardGeneric("getData")
})
setGeneric("getGate",function(obj,y,...){
	standardGeneric("getGate");
})
setGeneric("setGate",function(obj,y,value,...){
      standardGeneric("setGate");
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

setGeneric("copyGatingHierarchyFromTo",function(a,b,...){
	standardGeneric("copyGatingHierarchyFromTo");
})

setGeneric("writeIndice",function(obj,y,z,...){
			standardGeneric("writeIndice");
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

setGeneric("setData",function(this,value)standardGeneric("setData"))



