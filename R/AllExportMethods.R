# Public methods

setMethod("exportAsFlowJoXML",signature("workFlow"),function(obj, file = NULL, ...){
			filled_template <- populate_multiworkflow_workspace_template(list(obj))
			write_workspace_file(filled_template, file = file)
		})

setMethod("exportAsFlowJoXML",signature("list"),function(obj, file = NULL, ...){
			filled_template <- populate_multiworkflow_workspace_template(obj)
			write_workspace_file(filled_template, file = file)
		})




# Package private methods


setMethod("exportAsFlowJoXML",signature("ellipsoidGate"),function(obj, transforms,...){
			xchannel <- obj@parameters[[1]]@parameters
			ychannel <- obj@parameters[[2]]@parameters
			x_transform <- transforms[[xchannel]]
			y_transform <- transforms[[ychannel]]
			if(is.null(x_transform)){
				x_transform <- function(x){x} # identity transform		
			} else {
				x_transform <- x_transform@f
			}
			if(is.null(y_transform)){
				y_transform <- function(x){x}
			} else {
				y_transform <- y_transform@f
			}
			gate_node <- NULL
			vertices <- ellipsoidGate2FlowJoVertices(obj)
			
			if(length(obj@parameters) == 2){
				makeVertexNode <- function(x){
					newXMLNode(name = "Vertex", attrs = list(x = invert_transform(x[1], x_transform), y = invert_transform(x[2], y_transform)))
				}
				vertices <- apply(vertices, 1, makeVertexNode)
				polygon <- newXMLNode(name = "Polygon", .children = list(vertices))
				polygon_parent_name <- get_gate_name(obj)
				parent_polygon <- newXMLNode(name = "Ellipse", .children = polygon, attrs = list(name = polygon_parent_name, xAxisName = xchannel, yAxisName = ychannel))				
				strings <- vector(mode = "list", length = 2)
				strings[[1]] <- newXMLNode(name = "String", text = xchannel)
				strings[[2]] <- newXMLNode(name = "String", text = ychannel)
				string_array <- newXMLNode(name = "StringArray", .children = strings)
				parameter_names <- newXMLNode(name = "ParameterNames", .children = string_array)
				graph_node <- get_graph_node(obj)
				gate_node <- newXMLNode(name = "PolygonGate", .children = list(parameter_names, parent_polygon, graph_node))				
			} else {
				stop('Only 2-d ellipsoid gates are currently supported for export.')
			}
			
			
		})


setMethod("exportAsFlowJoXML",signature("rectangleGate"),function(obj, transforms,...){
			gate_node <- NULL
			if(length(obj@min) == 1){
				channel <- obj@parameters[[1]]@parameters #wrapped in a "unitytransform". what.
				transform <- transforms[[channel]]@f
				if(is.null(transform)){
					transform <- function(x){x}; # identity transform
				}
				# building the "Range" node from the bottom up
				vertices <- vector(mode = "list", length = 2)
				# the "as.character" conversion will produce 15 significant digits.
				# Old FlowJo XML appears to have "y" coordinates on these "Range" gates.  They are equal for both vertices.
				
				vertices[[1]] <- newXMLNode(name = "Vertex", attrs = list(x = as.character(invert_transform(obj@min, transform)), y = "0.0"))
				vertices[[2]] <- newXMLNode(name = "Vertex", attrs = list(x = as.character(invert_transform(obj@max, transform)), y = "0.0"))
				polygon <- newXMLNode(name = "Polygon", .children = list(vertices))
				range_name <- get_gate_name(obj)
				
				range <- newXMLNode(name = "Range", .children = polygon, attrs = list(name = range_name, xAxisName = channel))
				
				# the "PolygonGate" will be comprised of the Range node above, and a ParameterNames.
				# Working backwards again...
				strings <- vector(mode = "list", length = 2)
				strings[[1]] <- newXMLNode(name = "String", text = channel)
				strings[[2]] <- newXMLNode(name = "String", text = "")
				string_array <- newXMLNode(name = "StringArray", .children = strings)
				parameter_names <- newXMLNode(name = "ParameterNames", .children = string_array)
				graph_node <- get_graph_node(obj)
				gate_node <- newXMLNode(name = "PolygonGate", .children = list(parameter_names, range, graph_node))
				
			} else {
				stop('Only 1-d "Range" gates are currently supported.')
			}
			return(gate_node)	
		})

setMethod("exportAsFlowJoXML",signature("polygonGate"),function(obj, transforms,...){
			xchannel <- obj@parameters[[1]]@parameters
			ychannel <- obj@parameters[[2]]@parameters
			x_transform <- transforms[[xchannel]]
			y_transform <- transforms[[ychannel]]
			if(is.null(x_transform)){
				x_transform <- function(x){x} # identity transform		
			} else {
				x_transform <- x_transform@f
			}
			if(is.null(y_transform)){
				y_transform <- function(x){x}
			} else {
				y_transform <- y_transform@f
			}
			gate_node <- NULL
			if(length(obj@parameters) == 2){
				makeVertexNode <- function(x){
					newXMLNode(name = "Vertex", attrs = list(x = invert_transform(x[1], x_transform), y = invert_transform(x[2], y_transform)))
				}
				vertices <- apply(obj@boundaries, 1, makeVertexNode)
				polygon <- newXMLNode(name = "Polygon", .children = list(vertices))
				polygon_parent_name <- get_gate_name(obj)
				parent_polygon <- newXMLNode(name = "Polygon", .children = polygon, attrs = list(name = polygon_parent_name, xAxisName = xchannel, yAxisName = ychannel))
				
				strings <- vector(mode = "list", length = 2)
				strings[[1]] <- newXMLNode(name = "String", text = xchannel)
				strings[[2]] <- newXMLNode(name = "String", text = ychannel)
				string_array <- newXMLNode(name = "StringArray", .children = strings)
				parameter_names <- newXMLNode(name = "ParameterNames", .children = string_array)
				graph_node <- get_graph_node(obj)
				gate_node <- newXMLNode(name = "PolygonGate", .children = list(parameter_names, parent_polygon, graph_node))
				
			} else {
				stop('Only 2-d polygons are currently supported.')
			}
			return(gate_node)		
		})

setMethod("exportAsFlowJoXML",signature("intersectFilter"),function(obj, transforms, gate_view, workflow){
			gate_node <- NULL
			
			boolean_formula <- get_boolean_formula(obj)
			
			string_array <- newXMLNode(name = "StringArray")
			parameter_names <- newXMLNode(name = "ParameterNames", .children = string_array)
			
			gate_paths <- vector(mode = "list", length = length(obj@filters))
			for(i in 1:length(obj@filters)){
				gate_path_node <- newXMLNode(name = "String", text = get_gate_path(workflow, obj@filters[[i]]@filterId)) 
				gate_paths[[i]] <- gate_path_node
			}
			string_array <- newXMLNode(name = "StringArray", .children = gate_paths)			
			gate_paths_node <- newXMLNode(name = "GatePaths", .children = string_array)
			#graph_node <- get_graph_node(obj) -- flowJo doesn't like Graphs in BooleanGates.  great.  makes sense.
			gate_node <- newXMLNode(name = "BooleanGate", .children = list(parameter_names, gate_paths_node), attrs = list(specification = boolean_formula))						
			return(gate_node)		
		})


get_gate_path <- function(workflow, filterId){
	# filterId here should be the positive filter, so if it's a complement we remove the 'not '
	if(substr(filterId, 0, 4)=="not "){
		filterId = substr(filterId, 5, nchar(filterId))
	}
	action <- workflow[[paste(c("action_", filterId), collapse="")]] # if an intersectFilter has a component filter which is not present in the workflow this will not be found.
                                                                     # since FlowJo is expecting a hierarchical gate path as a part of the BooleanGate, this is not optional.
	if(is.null(action)){
		stop("Can't export a workflow with intersectFilter gates composed of filters not present in the workflow.")
	}
	path <- get_gate_name(action)
	parent_action <- parent(action)
	parent_class = class(parent_action)
	while(parent_class == "gateView"){
	 	path <- c(path, get_gate_name(parent_action))
		parent_action <- parent(parent_action)
		parent_class <- class(parent_action)
	}
	path <- paste(path[length(path):1], collapse = "/")
	return(path)
}

get_boolean_formula <- function(intersection){
	filters <- intersection@filters
	Gs <- rep("G", length(filters))
	suffixes <- 0:(length(filters)-1)
	
	# is it important to know more than complementFilter vs non-complement in constructing this formula?
	# for an intersection filter, we do know that it will always be "and" connecting the filters, so 
	# we just need to know if it's negated.
	is_complement <- sapply(filters, class)=="complementFilter"
	prefixes <- rep("", length(filters))
	prefixes[is_complement] <- "!"
    #
	
	names <- apply(cbind(prefixes, Gs, suffixes), 1, paste, collapse = "")
	formula <- paste(names, collapse = " & ")
	return(formula)
}	


invert_nonlog_transform <- function(y_value, transform_function, interval = c(-10^3, 10^6)){
	fr <- function(x){abs(transform_function(x)-y_value)}
	o <- optimize(fr, interval)
	return(o$min)
}

invert_transform <- function(y_value, transform_function){
	transform_type <- attr(transform_function, "type") 
	if(is.null(transform_type)){		
		return(invert_nonlog_transform(y_value, transform_function))
	} else if(transform_type == "log"){
		return(10^y_value)
	} else if (transform_type == "identity"){
		return(y_value)
	} else if (transform_type == "gateOnly"){
		return(y_value)
	}
	else {
		return(invert_nonlog_transform(y_value, transform_function))
	}
}


get_graph_node <- function(gate){

    prev_graph_type = "Pseudocolor"

	# in this case we choose the axes from the first gate in the filter
	if(class(gate) == "intersectFilter"){
		gate <- gate@filters[[1]]
	}
	
	if(class(gate) == "rectangleGate"){
        graph_type = "Histogram"
		yaxis = ""
	} else {		
		yaxis <- gate@parameters[[2]]@parameters
		graph_type = "Pseudocolor"
	}
	
	axes_nodes <- vector("list", 2)	
	
	## FlowJo prefers "dimemsion" to "dimension".  Do not 'fix' this spelling.	
	axes_nodes[[1]] <- newXMLNode(name = "Axis", attrs = list(name = gate@parameters[[1]]@parameters, dimemsion = 1))   
	axes_nodes[[2]] <- newXMLNode(name = "Axis", attrs = list(name = yaxis, dimemsion = 2))
	
	graph_node <- newXMLNode(name = "Graph", .children = axes_nodes, attrs = list(type = graph_type, highResolution = 1, prevGraphType = prev_graph_type, prevHighResolution = 1))
	
	
	return(graph_node)
}


get_gate_name <- function(gate){
	if(class(gate)=="gateView"){
		raw <- gate@name
		raw <- substr(raw, 1, nchar(raw)-1)
		return(gsub("\\d+\\.", "", raw))
	} else if(class(gate)=="gateActionItem"){
		raw <- gate@name
		return(gsub("action_\\d+\\.", "", raw))		
	} else {
	    raw <- gate@filterId
	    return(gsub("filter_\\d+\\.", "", raw))
	}
}

read_workspace_template <- function(template_file = NULL){
	if(is.null(template_file )){
		template_file = dir(system.file("extdata", package = "flowWorkspace"), full.names = TRUE, pattern = "empty_workspace_9.2.xml")
	}   
	workspace_template <- xmlTreeParse(template_file)
	return(workspace_template)
}


get_workflow_comp_and_trans <- function(workflow){
	
	comp_view <- NULL
	trans_view <- NULL
	
	# if there's a compensation, we expect it to be the second node in the workflow and then a transform 
	if("compensateView" %in% class(get(nodes(workflow)[2], workflow@env))){
		comp_view <- get(nodes(workflow)[2], workflow@env)
		
		if("transformView" %in% class(get(nodes(workflow)[3], workflow@env))){
			trans_view <- get(nodes(workflow)[3], workflow@env)									
		}
	} else {# if there not a compensation, we expect the transformation to be the second node 
		if("transformView" %in% class(get(nodes(workflow)[2], workflow@env))){
			trans_view <- get(nodes(workflow)[2], workflow@env)
		}
	}
	
	# we need the node above all gates (which themselves may then be nested)
	if(is.null(trans_view) & is.null(comp_view)){
		gates_parent_node <- nodes(workflow)[1]
	} else if(!is.null(trans_view) & !is.null(comp_view)){
		gates_parent_node <- nodes(workflow)[3]		
	} else {
		gates_parent_node <- nodes(workflow)[2]
	}
	
	transforms <- NULL
	if(!is.null(trans_view)){		
		trans_action <- get(trans_view@action@ID, trans_view@env)
		trans_list <- get(trans_action@transform@ID, trans_action@env)
		transforms <- trans_list@transforms		
	}
	return(list(compensation_view = comp_view, transformations = transforms, gates_parent_node = gates_parent_node))
}


is_workflow_exportable <- function(workflow){
	if(length(nodes(workflow)) < 3){
		stop("Expecting a workFlow with at least three items.")
	}		
}

populate_multiworkflow_workspace_template <- function(workflows, template = read_workspace_template()){
	
	canonical_workflow <- workflows[[1]]
	
	comp_and_trans <- get_workflow_comp_and_trans(canonical_workflow)
	compensation_view <- comp_and_trans$compensation_view
	transforms <- comp_and_trans$transformations
	gates_parent_node <- comp_and_trans$gates_parent_node
	
	# we will replace children of Workspace
	workspace_node <- template$doc$children[["Workspace"]]
	
	# retrieve the flowSet or flowFrame associated with the root of the canonical workflow.
	workflow_data <- Data(flowCore:::get(nodes(canonical_workflow)[1], canonical_workflow))
	
	workspace_node[["Keywords"]] <- get_keywords_node(workflow_data)    
	workspace_node[["StainChannelList"]] <- get_stainchannellist_node(workflow_data)
	workspace_node[["Groups"]] <- get_groups_node(workflows)
	
	message("Converting gates.")
	workspace_node[["SampleList"]] <- get_samplelist_node(workflows, transforms)
	
	if(!is.null(compensation_view)){
		message("Converting compensation matrices.")
		workspace_node[["CompensationMatrices"]] <- get_compensationmatrices_node(canonical_workflow, compensation_view)
	} else {
		message("Skipping compensation matrices (none found).")
	}
	
	message("Converting calibration tables.")			
	if(!is.null(transforms)){
		workspace_node[["CalibrationTables"]] <- get_calibrationtables_node(canonical_workflow, transforms)
	}
	
	# put our workspace back in the template
	template$doc$children[["Workspace"]] <- workspace_node
	return(template)
	
	
}

get_calibrationtables_node <- function(workflow, transforms){
	table_nodes <- vector("list")
	
	# make each table node
	for(i in 1:length(transforms)){
		if(attr(transforms[[i]]@f, "type") == "flowJo" | is.null(attr(transforms[[i]]@f, "type"))){						
			s<-seq(-10^3,10^6,l=1e6) # precision of the inverse computation 
			transform_matrix <- cbind(0:4096, splinefun(transforms[[i]]@f(s),s, method = "natural")(0:4096))
			attrs <- list(name = paste(c("calibration_tables", names(transforms)[i]), collapse=""), biexponentialDecades="0",biexponentialNegDecades="0", biexponentialWidth="0")	 # sometimes these will be real?  flowjo crashes if not present.
			table_node <- newXMLNode(name = "Table", attrs = attrs, value = paste(apply(transform_matrix, 1, paste, collapse= "," ), collapse=","))
			table_nodes <- c(table_nodes, table_node)
		}
		# above in the attrs assignment, the calibration table name will match to the compensation matrix name 
	}

	calibrationtables_node <- newXMLNode(name = "CalibrationTables", .children = table_nodes)
}


get_compensationmatrices_node <- function(workflow, comp_view){
	
	comp_action <- get(comp_view@action@ID, comp_view@env)
	comp_matrix <- get(comp_action@compensate@ID, comp_action@env)@spillover
	channels <- vector("list", ncol(comp_matrix))
	
	for(i in 1:nrow(comp_matrix)){
		channelvalues <- vector("list", ncol(comp_matrix))
		for(j in 1:ncol(comp_matrix)){
			channelvalue_node <- newXMLNode(name = "ChannelValue", attrs = list(name = colnames(comp_matrix)[j], value = comp_matrix[i, j]))
			channelvalues[[j]] <- channelvalue_node
		}		  
		channel_node <- newXMLNode(name = "Channel", .children = channelvalues, attrs = list(name = rownames(comp_matrix)[i]))
		channels[[i]] <- channel_node
	}
	
	
	compensationmatrix_node <- newXMLNode(name = "CompensationMatrix", .children = channels, attrs = list(name = "calibration_tables ", prefix = "<", suffix = ">", usageFlag = 1))
	compensationmatrices_node <- newXMLNode(name = "CompensationMatrices", .children = compensationmatrix_node)
	return(compensationmatrices_node)
}

## This currently creates only one group -- "All Samples"
get_groups_node <- function(workflows){    
	sample_id = 1
	sampleref_nodes <- list()
	for(workflow in workflows){
		flow_data <- Data(flowCore:::get(nodes(workflow)[1], workflow))
		if(class(flow_data)=="flowSet"){ 
			for(i in 1:length(flow_data)){
				sampleref_nodes <- c(sampleref_nodes, newXMLNode(name = "SampleRef", attrs = list(sampleId = as.character(sample_id))))
				sample_id = sample_id + 1    
			}        
		} else {
			sampleref_nodes <-  newXMLNode(name = "SampleRef", attrs = list(sampleId = as.character(sample_id)))
			sample_id = sample_id + 1
		}
	}
	samplerefs_node <- newXMLNode(name = "SampleRefs", .children = sampleref_nodes)
	group_node <- newXMLNode(name = "Group", .children = samplerefs_node, attrs = list(groupID = "0"))
	groupnode_node <- newXMLNode(name = "GroupNode", .children = group_node, attrs = list(groupID = "0", name = "All Samples", owningGroup="All Samples"))
	groups_node <- newXMLNode(name = "Groups", .children = groupnode_node, attrs = list(nextGroupId = "1", sortOrder="ByName"))
}

get_keywords_node <- function(flow_data){    
	all_keywords <- NULL
	if(class(flow_data)=="flowSet"){
		for(i in 1:length(flow_data)){
			flow_frame <- flow_data[[i]]
			keywords <- keyword(flow_frame)
			new_keywords <- setdiff(names(keywords), names(all_keywords))
			all_keywords <- c(all_keywords, keywords[new_keywords])
		}    
	} else {
		all_keywords <- keyword(flow_frame)        
	}     
	keyword_nodes <- vector("list", length(all_keywords))
	for(i in 1:length(all_keywords)){
		keyword_nodes[[i]] = newXMLNode(name = "Keyword", attrs = list(name = names(all_keywords)[i]))
	}       
	keywords_node <- newXMLNode(name = "Keywords", .children = keyword_nodes)
	return(keywords_node)
}

## TODO: This is taking stains from the first frame if
##       this is a flowSet.  Do some unioning like for 
##       global keywords?
get_stainchannellist_node <- function(flow_data){
	if(class(flow_data)=="flowSet"){
		flow_frame <- flow_data[[1]]
	}
	stain_channels <- colnames(exprs(flow_frame))  	
	stain_channels <- subset(stain_channels, is_channel_stain(stain_channels))
	strings <- vector(mode = "list", length = length(stain_channels))
	strings <- sapply(stain_channels, function(x)newXMLNode(name = "String", text = x[[1]])) 
	string_array_node <- newXMLNode(name = "StringArray", .children = strings)
	stainchannellist_node <- newXMLNode(name = "StainChannelList", .children = string_array_node);
	return(stainchannellist_node)
}

is_channel_stain <- function(channel_name){
	nonstain_channels <- c("SSC-W","SSC-H", "SSC-A", "FSC-W", "FSC-H", "FSC-A", "Time")
	return(!(channel_name %in% nonstain_channels))
}

## fill the <Parameter> attributes under a <Sample>
get_sample_parameter_nodes <- function(flow_frame, transforms){	
	
	channels <- flow_frame@parameters@data[["name"]]
	channel_aliases <- row.names(flow_frame@parameters@data)
	keywords <- keyword(flow_frame)
	parameter_nodes <- NULL
	for(i in 1:length(channels)){
		name <- channels[[i]]
		
		label <- keywords[[paste(c(channel_aliases[i], "S"), collapse="")]]
		if(is.null(label)){
			label <- ""
		}
		
		bits <- keywords[[paste(c(channel_aliases[i], "B"), collapse="")]]
		
		display_scale_keyword_name = paste(c("P", i, "DISPLAY"), collapse="")
		
		if(!is.null(keywords[[display_scale_keyword_name]]) && keywords[[display_scale_keyword_name]]=="LOG") display_scale_is_log <- "1" else display_scale_is_log <- "0" 
		
		# TODO: these won't be right if R has linearized the FCS data but FlowJo will be reading the original
		min_range <- parameters(flow_frame)@data[i,"minRange"]
		max_range <- parameters(flow_frame)@data[i,"maxRange"] + 1 
		
		keyword_gain <- as.numeric(keywords[[paste(c(channel_aliases[i], "G"), collapse="")]])
		if(length(keyword_gain)==0){
			keyword_gain <- 1
		}
		
		if(display_scale_is_log=="0"){
			gain <- as.numeric(keyword_gain)/(max_range/4096)   #TODO: this is wrong for Time, at least
			decades <- 0
		} else {
			gain <- "1"
			decades <- log(max_range,  10) + 1
		}
		
		
		attrs <- list(name = name, label = label, bits = bits, log = display_scale_is_log, range = 4096, lowValue = min_range, highValue = max_range, decades = decades, gain = gain)

		if(is_channel_stain(name)){			
			attrs <- c(attrs, calibrationIndex = "1")
		}
		
		parameter_nodes <- c(parameter_nodes, newXMLNode(name = "Parameter", attrs = attrs))
	}    
	
	return(parameter_nodes)
}

get_samplelist_node <- function(workflows, transforms){
	children <- vector("list")
	
	total_sample_id = 1
	for(workflow in workflows){
		workflow_sample_id = 1
		flow_data <- Data(flowCore:::get(nodes(workflow)[1], workflow))		
		comp_and_trans <- get_workflow_comp_and_trans(workflow)
		gates_parent_node <- comp_and_trans$gates_parent_node # we need the node just above gates for putting graphs in the Population children of a SampleList\Sample		
		gates_parent_view <- get(gates_parent_node, workflow@env)		
		toplevel_gate_nodes <- unlist(tree(workflow)@edgeL[[gates_parent_view@ID]])
		
		if(class(flow_data)=="flowSet"){
			for(i in 1:length(flow_data)){			
				children <- c(children, get_sample_node(flow_data[[i]], workflow, workflow_sample_id, total_sample_id, transforms, toplevel_gate_nodes))				
				workflow_sample_id = workflow_sample_id + 1
				total_sample_id = total_sample_id + 1
			}
		} else {
			children <- c(children, get_sample_node(flow_data, workflow_sample_id, total_sample_id, toplevel_gate_nodes))
			total_sample_id = total_sample_id + 1
		}
	}
	return(newXMLNode(name = "SampleList", .children = children, attrs = list(nextSampleID=total_sample_id+1)))
}

get_sample_keywords_node <- function(flow_frame){	
	# we're going to take the keywords from the FCS files rather than from the R objects, which
	# may have altered the FCS keywords. We see this with "P7E" or generally "PNE" type keywords
	# when R has linearized the data.
	
	#keywords <- keyword(flow_frame)
	fcs_file <- keyword(flow_frame, "FILENAME")[[1]]
	keywords <- read.FCSheader(fcs_file)[[1]]
	
	keyword_nodes <- vector("list", length(keywords))	
	for(i in 1:length(keywords)){
		keyword_nodes[[i]] = newXMLNode(name = "Keyword", attrs = list(name = names(keywords)[i], value = keywords[[i]]))
	}       
	
	keywords_node <- newXMLNode(name = "Keywords", .children = keyword_nodes)
	return(keywords_node)
}

get_population_node <- function(sample_id, gate_view_ref, workflow, transforms){
	
	gate_view <- get(gate_view_ref, workflow@env)
	gate_action <- get(gate_view@action@ID, gate_view@env)
	gate <- get(gate_action@gate@ID, gate_action@env)
	gate_node <- exportAsFlowJoXML(gate, transforms, gate_view, workflow)			
	
	children_indexes <- unlist(tree(workflow)@edgeL[[gate_view@ID]])
	
	children <- list(gate_node)
	for(i in children_indexes){
		children <- c(children, get_population_node(sample_id, nodes(workflow)[i], workflow, transforms)) #recursion
	}
	
	count <- flowCore:::summary(gate_view)[,"true"][sample_id]
	
	# we need to add one more child, namely a graph node which specifies axes from a child gate
	# this is for the "up down" tree traversal in flowjo gui
	if(length(children_indexes)>0){
		child_gate_view <- get(nodes(workflow)[children_indexes[1]], workflow@env)
		child_gate_action <- get(child_gate_view@action@ID, child_gate_view@env)
		child_gate <- get(child_gate_action@gate@ID, child_gate_action@env)
		graph_node <- get_graph_node(child_gate)				
	} else { # if there are no children we still need the graph but with the current gate's axes 
		graph_node <- get_graph_node(gate)
		
	}
	children <- c(graph_node, children)
	attrs <- list(count = count, name = get_gate_name(gate), owningGroup="All Samples")
	
	if(length(children_indexes)==0){
		attrs <- c(attrs, hasTwistDown = "0")
	}
			
	pop_node <- newXMLNode(name = "Population", .children = children, attrs = attrs)
	return(pop_node)
}

get_population_gate_hierarchy_node <- function(sample_id, workflow, transforms, toplevel_gate_nodes){
		
	population_nodes <- NULL
	for(j in 1:length(toplevel_gate_nodes)){
		population_nodes <- c(population_nodes, get_population_node(sample_id, nodes(workflow)[toplevel_gate_nodes[[j]]], workflow, transforms))		
		
	}
	return(population_nodes)
}

get_sample_node <- function(flow_frame, workflow, workflow_sample_id, total_sample_id, transforms, toplevel_gate_nodes){
	sample_keywords_node <- get_sample_keywords_node(flow_frame) # <Keywords>
	sample_parameter_nodes <- get_sample_parameter_nodes(flow_frame, transforms) # <Parameter> list    
	populations <- get_population_gate_hierarchy_node(workflow_sample_id, workflow, transforms, toplevel_gate_nodes)
	
	event_count <- nrow(flow_frame)
	samplenode_node <- newXMLNode(name = "SampleNode", .children = populations, attrs = list(sampleID = total_sample_id, name = flow_frame@description[["SAMPLE ID"]]))
	sample_node <- newXMLNode(name = "Sample", .children = c(sample_keywords_node, sample_parameter_nodes, samplenode_node), attrs = list(eventCount = event_count, sampleNumber="", sampleID=total_sample_id, compensationID="1"))
	
	return(sample_node);    
}

# Note that, when a file is not used, the user will be warned about 
# closing a NULL connection.  This is because XML::saveXML doesn't
# close the connection it makes.
write_workspace_file <- function(filled_template, file){
	# this prefix should be programatically available via the doctype during save,
	# but there was some funky spacing doing it and no way to adjust
	prefix <- '<?xml version="1.0" encoding="ISO-8859-1"?>\n<!DOCTYPE Workspace>\n'
	root <- xmlRoot(filled_template)
	return(saveXML(doc = root, file = file, prefix = prefix))	
}
