# TODO: Add comment
# 
# Author: wjiang2
###############################################################################



setClass("GatingSetInternal",contains="GatingSet"
		,representation(set="list"
				,metadata="AnnotatedDataFrame")
		,validity=function(object){
			all(unlist(lapply(object@set
									,function(y)inherits(y,"GatingHierarchy"))))
		})

setClass("GatingHierarchyInternal",contains="GatingHierarchy"
		,representation(tree="graphNEL"
				,nodes="character"
				,name="character"
				,flag="logical"
				,transformations="list"
				,compensation="matrix"
				,dataPath="character"
				,isNcdf="logical"))

