#' @include filterObject_Methods.R
NULL

#' @templateVar old setGate
#' @templateVar new gs(/gh)_pop_set_gate
#' @template template-depr_pkg
NULL
#' @export
setGeneric("setGate",function(obj,y,value,...)standardGeneric("setGate"))

#' @export
setMethod("setGate"
    ,signature(obj="GatingHierarchy",y="character",value="filter")
    ,function(obj,y,value,...){
      .Deprecated("gh_pop_set_gate")
	  gh_pop_set_gate(obj,y,value,...)
	   
    })

#' update the gate
#' 
#' update the population node with a flowCore-compatible gate object
#' 
#' Usually \link{recompute} is followed by this call since updating a gate
#' doesn't re-calculating the cell events within the gate automatically.
#' see \link{filterObject} for the gate types that are currently supported.
#' 
#' @name gs_pop_set_gate
#' @aliases gh_pop_set_gate setGate setGate,GatingHierarchy,character,filter-method
#' setGate,GatingSet,character,ANY-method
#' @param obj \code{GatingHierarchy} or \code{GatingSet}
#' @param y \code{character} node name or path
#' @param value \code{filter} or \code{filterList} or \code{list} of \code{filter} objects
#' @param negated \code{logical} see \link{add}
#' @param ... other aguments
#' @examples
#' \dontrun{  
#' rg1 <- rectangleGate("FSC-H"=c(200,400), "SSC-H"=c(250, 400), filterId="rectangle")
#' rg2 <- rectangleGate("FSC-H"=c(200,400), "SSC-H"=c(250, 400), filterId="rectangle")
#' flist <- list(rg1,rg2)
#' names(flist) <- sampleNames(gs[1:2])
#' gs_pop_set_gate(gs[1:2], "lymph", flist)
#' recompute(gs[1:2], "lymph") 
#' }
#' @export
gh_pop_set_gate <- function(obj,y,value, negated = FALSE,...){
			if(is(value, "quadGate"))
			{
			  if(negated)
			    stop("'negated' flag can't be TRUE for quadGate")
			  g <- gh_pop_get_gate(obj, y)
			  quadintersection <- attr(g, "quadintersection")
			  if(is.null(quadintersection))
			    stop(y, " is not a quadGate and can't be updated by a new quadGate!")
			  newintersection <- value@boundary
			  if(!all(names(quadintersection) == names(newintersection)))
			    stop("The channel names of the new quadGate do not match to the original quadGate")
			  quadrants <- attr(g, "quadrants")
			  if(length(quadrants)!=4)
			    warning("The existing quadGate just has ", length(quadrants), " quadrants and only these quadrants will be updated!")
			  set_quadgate(obj@pointer,sampleNames(obj), y, newintersection)
			}else
			{
			  this_fobj <- filter_to_list(value)
			  this_fobj$negated<-negated
			  cpp_setGate(obj@pointer,sampleNames(obj), y, this_fobj)
			  
			}
			
		}

#' @export 
setMethod("setGate",
    signature=c(obj="GatingSet",y="character", value = "ANY"),
    definition=function(obj, y, value,...)
    {
		.Deprecated("gs_pop_set_gate")
		 gs_pop_set_gate(obj, y, value)
      
    })

#' @rdname gs_pop_set_gate
#' @export
gs_pop_set_gate <- function(obj, y, value,...)
    {
		if(is(value, "filterList"))
		{
	      samples<-sampleNames(obj)
	      
	      if(!setequal(names(value),samples))
	        stop("names of filterList do not match with the sample names in the gating set!")           
	      
	      lapply(samples,function(sample){
	            curFilter<-value[[sample]]
	            gh<-obj[[sample]]
	            gh_pop_set_gate(obj=gh,y,value=curFilter,...)
	          })
  		}else if(is(value, "list")){
			flist<-filterList(value)
			gs_pop_set_gate(obj,y,flist,...)
			
		}else
			stop(class(value), " not supported!")
      
    }

#' Simplified geometric transformations of gates associated with nodes
#' 
#' Perform geometric transformations of a gate associated with a node of a \code{\linkS4class{GatingHierarchy}} or
#' \code{\linkS4class{GatingSet}}. This method is a wrapper for \code{\link[flowCore]{transform_gate}} that enables 
#' updating of the gate associated with a node of a \code{GatingHierarchy} or \code{GatingSet}.
#'  
#' \code{transform_gate} calls \code{\link{gs_pop_set_gate}} to modify the provided \code{GatingHierarchy} or \code{GatingSet} 
#' directly so there is no need to re-assign its output. The arguments will be essentially identical to the 
#' \code{flowCore} method, except for the specification of the target gate. Rather than being called on an 
#' object of type \code{flowCore::filter}, here it is called on a \code{GatingHierarchy} or \code{GatingSet} 
#' object with an additional character argument for specifying the node whose gate should be transformed. 
#' The rest of the details below are taken from the \code{flowCore} documentation.
#' 
#' This method allows changes to the four filter types defined by simple geometric gates (\code{\linkS4class{quadGate}},
#' \code{\linkS4class{rectangleGate}}, \code{\linkS4class{ellipsoidGate}}, and \code{\linkS4class{polygonGate}}) using
#' equally simple geometric transformations (shifting/translation, scaling/dilation, and rotation). The method also
#' allows for directly re-setting the slots of each Gate-type object. Note that these methods are for manually altering
#' the geometric definition of a gate. To easily transform the definition of a gate with an accompanyging scale 
#' transformation applied to its underlying data, see ?ggcyto::rescale_gate.
#' 
#' First, \code{transform_gate} will apply any direct alterations to the slots of the supplied Gate-type filter object.
#' For example, if "\code{mean = c(1,3)}" is present in the argument list when \code{transform_gate} is called on a
#' \code{ellipsoidGate} object, the first change applied will be to shift the \code{mean} slot to \code{(1,3)}. The method
#' will carry over the dimension names from the gate, so there is no need to provide column or row names with arguments
#' such as \code{mean} or \code{cov} for \code{ellipsoidGate} or \code{boundaries} for \code{polygonGate}.
#' 
#' \code{transform_gate} then passes the geometric arguments (\code{dx}, \code{dy}, \code{deg}, \code{rot_center}, \code{scale}, 
#' and \code{center}) to the methods which perform each respective type of transformation:  
#' \code{\link[flowCore]{shift_gate}}, \code{\link[flowCore]{scale_gate}}, or \code{\link[flowCore]{rotate_gate}}. 
#' The order of operations is to first scale, then rotate, then shift. The default behavior of each operation follows 
#' that of its corresponding method but for the most part these are what the user would expect. A few quick notes:
#' \itemize{
#' \item \code{rotate_gate} is not defined for \code{rectangleGate} or \code{quadGate} objects, due to their definition as
#' having 1-dimensional boundaries.
#' \item The default center for both rotation and scaling of a \code{polygonGate} is the centroid of the polygon. This
#' results in the sort of scaling most users expect, with a uniform scale factor not distorting the shape of the original polygon.
#' }
#' 
#' @name transform_gate
#' @aliases transform_gate,GatingHierarchy-method transform_gate,GatingSet-method
#' @param obj A \code{GatingHierarchy} or \code{GatingSet} object
#' @param y A character specifying the node whose gate should be modified
#' @param scale Either a numeric scalar (for uniform scaling in all dimensions) or numeric vector specifying the factor by 
#' which each dimension of the gate should be expanded (absolute value > 1) or contracted (absolute value < 1). Negative values 
#' will result in a reflection in that dimension. 
#' 
#' For \code{rectangleGate} and \code{quadGate} objects, this amounts to simply
#' scaling the values of the 1-dimensional boundaries. For \code{polygonGate} objects, the values of \code{scale} will be used
#' to determine scale factors in the direction of each of the 2 dimensions of the gate (\code{scale_gate} is not yet defined
#' for higher-dimensional \code{polytopeGate} objects). \strong{Important: } For \code{ellipsoidGate} objects, \code{scale}
#' determines scale factors for the major and minor axes of the ellipse, in that order.
#' 
#' @param deg An angle in degrees by which the gate should be rotated in the counter-clockwise direction.
#' @param rot_center A separate 2-dimensional center of rotation for the gate, if desired. By default, this will
#' be the center for \code{ellipsoidGate} objects or the centroid for \code{polygonGate} objects. The \code{rot_center} argument 
#' is currently only supported for \code{polygonGate} objects. It is also usually simpler to perform a rotation and a translation 
#' individually than to manually specify the composition as a rotation around a shifted center.
#' 
#' @param dx Either a numeric scalar or numeric vector. If it is scalar, this is just the desired shift of the gate in 
#' its first dimension. If it is a vector, it specifies both \code{dx} and \code{dy} as \code{(dx,dy)}.
#' This provides an alternate syntax for shifting gates, as well as allowing shifts of \code{ellipsoidGate} objects
#' in more than 2 dimensions.
#' @param dy A numeric scalar specifying the desired shift of the gate in its second dimension.
#' @param center A numeric vector specifying where the center or centroid should be moved (rather than specifiying \code{dx} 
#' and/or \code{dy})
#' 
#' @param \dots Assignments made to the slots of the particular Gate-type filter object in the form "<slot_name> = <value>"
#' 
#' @examples
#' \dontrun{
#' # Scale the original gate non-uniformly, rotate it 15 degrees, and shift it
#' transform_gate(gs, node, scale = c(2,3), deg = 15, dx = 500, dy = -700)
#' 
#' # Scale the original gate (in this case an ellipsoidGate) after moving its center to (1500, 2000)
#' transform_gate(gs, node, scale = c(2,3), mean = c(1500, 2000))
#' }
#'
#' @seealso \code{\link[flowCore:transform_gate]{flowCore::transform_gate}}
#'
#' @export
transform_gate.GatingHierarchy <- function(obj, y, scale = NULL, deg = NULL, rot_center = NULL, dx = NULL, dy = NULL, center = NULL, ...){
  gate <- gh_pop_get_gate(obj, y)
  gate <- transform_gate(gate, scale = scale, deg = deg, 
                         rot_center = rot_center, dx = dx,
                         dy = dy, center = center, ...)
  gh_pop_set_gate(obj, y, gate)
}

#' @export
transform_gate.GatingSet <- function(obj, y, scale = NULL, deg = NULL, rot_center = NULL, dx = NULL, dy = NULL, center = NULL, ...){
  gates <- gs_pop_get_gate(obj, y)
  gates <- lapply(gates, function(gate) transform_gate(gate, scale = scale, deg = deg, 
                                                       rot_center = rot_center, dx = dx,
                                                       dy = dy, center = center, ...)) 
  gs_pop_set_gate(obj, y, gates)
}

#' Simplified geometric scaling of gates associated with nodes
#' 
#' Scale a gate associated with a node of a \code{GatingHierarchy} or
#' \code{\linkS4class{GatingSet}}. This method is a wrapper for \code{\link[flowCore]{scale_gate}} that enables 
#' updating of the gate associated with a node of a \code{GatingHierarchy} or \code{GatingSet}.
#'  
#' \code{scale_gate} calls \code{\link{gs_pop_set_gate}} to modify the provided \code{GatingHierarchy} or \code{GatingSet} 
#' directly so there is no need to re-assign its output. The arguments will be essentially identical to the 
#' \code{flowCore} method, except for the specification of the target gate. Rather than being called on an 
#' object of type \code{\link[flowCore]{filter}}, here it is called on a \code{GatingHierarchy} or \code{GatingSet} 
#' object with an additional character argument for specifying the node whose gate should be transformed. 
#' The rest of the details below are taken from the \code{flowCore} documentation.
#' 
#' This method allows uniform or non-uniform geometric scaling of filter types defined by simple geometric gates 
#' (\code{\linkS4class{quadGate}}, \code{\linkS4class{rectangleGate}}, \code{\linkS4class{ellipsoidGate}}, and 
#' \code{\linkS4class{polygonGate}}) Note that these methods are for manually altering
#' the geometric definition of a gate. To easily transform the definition of a gate with an accompanyging scale 
#' transformation applied to its underlying data, see ?ggcyto::rescale_gate.
#' 
#' The \code{scale} argument passed to \code{scale_gate} should be either a scalar or a vector of the same length
#' as the number of dimensions of the gate. If it is scalar, all dimensions will be multiplicatively scaled uniformly
#' by the scalar factor provided. If it is a vector, each dimension will be scaled by its corresponding entry in the vector.
#' 
#' The scaling behavior of \code{scale_gate} depends on the type of gate passed to it. For \code{rectangleGate} 
#' and \code{quadGate} objects, this amounts to simply scaling the values of the 1-dimensional boundaries. 
#' For \code{polygonGate} objects, the values of \code{scale} will be used to determine scale factors 
#' in the direction of each of the 2 dimensions of the gate (\code{scale_gate} is not yet defined
#' for higher-dimensional \code{polytopeGate} objects). \strong{Important: } For \code{ellipsoidGate} objects, \code{scale}
#' determines scale factors for the major and minor axes of the ellipse, \emph{in that order}. Scaling by a negative factor 
#' will result in a reflection in the corresponding dimension.
#' 
#' @name scale_gate
#' @aliases scale_gate,GatingHierarchy-method scale_gate,GatingSet-method
#' @param obj A \code{GatingHierarchy} or \code{GatingSet} object
#' @param y A character specifying the node whose gate should be modified
#' 
#' @param scale Either a numeric scalar (for uniform scaling in all dimensions) or numeric vector specifying the factor by 
#' which each dimension of the gate should be expanded (absolute value > 1) or contracted (absolute value < 1). Negative values 
#' will result in a reflection in that dimension. 
#' @param ... not used
#' @examples
#' \dontrun{
#' # Scales both dimensions by a factor of 5
#' scale_gate(gs, node, 5)
#' 
#' # Shrinks the gate in the first dimension by factor of 1/2
#' # and expands it in the other dimension by factor of 3
#' scale_gate(gs, node, c(0.5,3))
#' }
#' 
#' @seealso transform_gate \code{\link[flowCore:scale_gate]{flowCore::scale_gate}}
#' @export
scale_gate.GatingHierarchy <- function(obj, y, scale = NULL, ...){
  gate <- gh_pop_get_gate(obj, y)
  gate <- scale_gate(gate, scale = scale)
  gh_pop_set_gate(obj, y, gate)
}

#' @export
scale_gate.GatingSet <- function(obj, y, scale = NULL, ...){
  gates <- gs_pop_get_gate(obj, y)
  gates <- lapply(gates, function(gate) scale_gate(gate, scale = scale)) 
  gs_pop_set_gate(obj, y, gates)
}

#' Simplified geometric rotation of gates associated with nodes
#' 
#' Rotate a gate associated with a node of a \code{GatingHierarchy} or
#' \code{GatingSet}. This method is a wrapper for \code{\link[flowCore]{rotate_gate}} that enables 
#' updating of the gate associated with a node of a \code{GatingHierarchy} or \code{GatingSet}.
#'  
#' \code{rotate_gate} calls \code{\link{gs_pop_set_gate}} to modify the provided \code{GatingHierarchy} or \code{GatingSet} 
#' directly so there is no need to re-assign its output. The arguments will be essentially identical to the 
#' \code{flowCore} method, except for the specification of the target gate. Rather than being called on an 
#' object of type \code{flowCore:filter}, here it is called on a \code{GatingHierarchy} or \code{GatingSet} 
#' object with an additional character argument for specifying the node whose gate should be transformed. 
#' The rest of the details below are taken from the \code{flowCore} documentation.
#' 
#' This method allows for geometric rotation of filter types defined by simple geometric gates 
#' (\code{\linkS4class{ellipsoidGate}}, and \code{\linkS4class{polygonGate}}). The method is not defined 
#' for \code{rectangleGate} or \code{quadGate} objects, due to their definition as having 1-dimensional boundaries.
#' 
#' The angle provided in the \code{deg} argument should be in degrees rather than radians. By default, the rotation
#' will be performed around the center of an \code{ellipsoidGate} or the centroid of the area encompassed by
#' a \code{polygonGate}. The \code{rot_center} argument allows for specification of a different center of rotation
#' for \code{polygonGate} objects (it is not yet implemented for \code{ellipsoidGate} objects) but
#' it is usually simpler to perform a rotation and a translation individually than to manually specify 
#' the composition as a rotation around a shifted center.
#' 
#' @name rotate_gate
#' @aliases rotate_gate,GatingHierarchy-method rotate_gate,GatingSet-method
#' @param obj A \code{GatingHierarchy} or \code{GatingSet} object
#' @param y A character specifying the node whose gate should be modified
#' 
#' @param deg An angle in degrees by which the gate should be rotated in the counter-clockwise direction
#' @param rot_center A separate 2-dimensional center of rotation for the gate, if desired. By default, this will
#' be the center for \code{ellipsoidGate} objects or the centroid for \code{polygonGate} objects. The \code{rot_center} argument 
#' is currently only supported for \code{polygonGate} objects.
#' @param ... not used
#' @examples
#' \dontrun{
#' #' # Rotates the original gate 15 degrees counter-clockwise
#' rotate_gate(gs, node, deg = 15)
#' # Rotates the original gate 270 degrees counter-clockwise
#' rotate_gate(gs, node, 270)
#' }
#' 
#' @seealso transform_gate \code{\link[flowCore:rotate_gate]{flowCore::rotate_gate}}
#' @export
rotate_gate.GatingHierarchy <- function(obj, y, deg = NULL, rot_center = NULL, ...){
  gate <- gh_pop_get_gate(obj, y)
  gate <- rotate_gate(gate, deg = deg, rot_center = rot_center)
  gh_pop_set_gate(obj, y, gate)
}

#' @export
rotate_gate.GatingSet <- function(obj, y, deg = NULL, rot_center = NULL, ...){
  gates <- gs_pop_get_gate(obj, y)
  gates <- lapply(gates, function(gate) rotate_gate(gate, deg = deg, rot_center = rot_center)) 
  gs_pop_set_gate(obj, y, gates)
}

#' Simplified geometric translation of gates associated with nodes
#' 
#' Shift the location of a gate associated with a node of a \code{GatingHierarchy} or
#' \code{GatingSet}. This method is a wrapper for \code{\link[flowCore]{shift_gate}} that enables 
#' updating of the gate associated with a node of a \code{GatingHierarchy} or \code{GatingSet}.
#'  
#' \code{shift_gate} calls \code{\link{gs_pop_set_gate}} to modify the provided \code{GatingHierarchy} or \code{GatingSet} 
#' directly so there is no need to re-assign its output. The arguments will be essentially identical to the 
#' \code{flowCore} method, except for the specification of the target gate. Rather than being called on an 
#' object of type \code{flowCore::filter}, here it is called on a \code{GatingHierarchy} or \code{GatingSet} 
#' object with an additional character argument for specifying the node whose gate should be transformed. 
#' The rest of the details below are taken from the \code{flowCore} documentation.
#' 
#' This method allows for geometric translation of filter types defined by simple geometric gates 
#' (\code{rectangleGate}, \code{quadGate}, \code{\linkS4class{ellipsoidGate}}, or \code{\linkS4class{polygonGate}}).
#' The method provides two approaches to specify a translation. For \code{rectangleGate} objects, this will
#' shift the \code{min} and \code{max} bounds by the same amount in each specified dimension. For \code{quadGate}
#' objects, this will simply shift the divinding boundary in each dimension. For \code{ellipsoidGate} objects, this
#' will shift the center (and therefore all points of the ellipse). For \code{polgonGate} objects, this will simply
#' shift all of the points defining the polygon.
#' 
#' The method allows two different approaches to shifting a gate. Through the \code{dx} and/or \code{dy} arguments,
#' a direct shift in each dimension can be provided. Alternatively, through the \code{center} argument, the gate
#' can be directly moved to a new location in relation to the old center of the gate. For \code{quadGate} objects, 
#' this center is the intersection of the two dividing boundaries (so the value of the \code{boundary} slot). For
#' \code{rectangleGate} objects, this is the center of the rectangle defined by the intersections of the centers
#' of each interval. For \code{ellipsoidGate} objects, it is the center of the ellipsoid, given by the \code{mean}
#' slot. For \code{polygonGate} objects, the centroid of the old polygon will be calculated and shifted to the new
#' location provided by \code{center} and all other points on the polygon will be shifted by relation to the centroid.
#' 
#' @name shift_gate
#' @aliases shift_gate,GatingHierarchy-method shift_gate,GatingSet-method
#' @param obj A \code{GatingHierarchy} or \code{GatingSet} object
#' @param y A character specifying the node whose gate should be modified
#' 
#' @param dx Either a numeric scalar or numeric vector. If it is scalar, this is just the desired shift of the gate in 
#' its first dimension. If it is a vector, it specifies both \code{dx} and \code{dy} as \code{(dx,dy)}.
#' This provides an alternate syntax for shifting gates, as well as allowing shifts of \code{ellipsoidGate} objects
#' in more than 2 dimensions.
#' @param dy A numeric scalar specifying the desired shift of the gate in its second dimension.
#' @param center A numeric vector specifying where the center or centroid should be moved (rather than specifiying \code{dx} 
#' and/or \code{dy})
#' @param ... not used
#' @examples
#' \dontrun{
#' # Moves the entire gate +500 in its first dimension and 0 in its second dimension
#' shift_gate(gs, node, dx = 500)
#' 
#' #Moves the entire gate +250 in its first dimension and +700 in its second dimension
#' shift_gate(gs, node, dx = 500, dy = 700)
#' 
#' # Same as previous
#' shift_gate(gs, node, c(500,700))
#' 
#' # Move the gate based on shifting its center to (700, 1000)
#' shift_gate(gs, node, center = c(700, 1000))
#' }
#' 
#' @seealso transform_gate \code{\link[flowCore:shift_gate]{flowCore::shift_gate}}
#' @export
shift_gate.GatingHierarchy <- function(obj, y, dx=NULL, dy=NULL, center=NULL, ...){
  gate <- gh_pop_get_gate(obj, y)
  gate <- shift_gate(gate, dx = dx, dy = dy, center = center)
  gh_pop_set_gate(obj, y, gate)
}

#' @export
shift_gate.GatingSet <- function(obj, y, dx=NULL, dy=NULL, center=NULL, ...){
  gates <- gs_pop_get_gate(obj, y)
  gates <- lapply(gates, function(gate) shift_gate(gate, dx = dx, dy = dy, center = center)) 
  gs_pop_set_gate(obj, y, gates)
}
  
  
