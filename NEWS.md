# flowWorkspace 4.1.5 (Bioconductor release 3.12)
+ switching the [[ subsetting behavior for cytoset to returning a cytoframe by default (breaking change)

# flowWorkspace 3.35.x (Bioconductor release 3.11)

+ Allow cytoset constructor to take a list of cytoframes
+ Add cf_rename_channel and cf_rename_marker
+ Add "select" option to load_gs
+ Remove shallow_copy method for cytoframe/cytoset
+ Allow negative subsetting of columns in cytoframe/cytoset
+ add save_cytoset and load_cytoset
+ Add methods for setting cytoframe keywords (cf_keyword_delete, cf_keyword_insert, cf_keyword_rename)
+ GatingSetList deprecated for merge_gslist
+ Make gs_pop_get_stats_tfilter columns/format match gh_pop_get_count_fast
+ Make gh_pop_get_indices take a vector of nodes rather than collapsed node string
+ Check for duplicate markers in markernames<- replacement
+ Add cs_set_cytoframe to change cytoframe reference within cytoset
+ Add cleanup_temp functions for manually cleaning up temp files associated with GatingSets
+ Remove automatic scaling of time channel from  gh_apply_to_new_fcs
+ Add inverse.transform option for gs_get_singlecell_expansion
+ write.FCS support added for cytoframe
+ Update GatingSet archive to use one protobuf file per sample and use `.gs` extension for protobuf files RGLab/cytolib #32
+ Modify `keyword<-` replacement method for `cytoframe` to allow partial replacement using named list, like `flowFrame` #311

# flowWorkspace 3.33.10 (Bioconductor release 3.10)

+ MAJOR CHANGE: Added cytoframe and cytoset classes (and supporting methdos) which are now used by GatingSet
  -- see section "The cytoframe and cytoset classes" in introduction vignette for summary of changes
+ Add cf_scale_time_channel for scaling time channel in a cytoframe
+ Add gh_apply_to_new_fcs to use a GatingHierarchy as template for gating new FCS files
+ Add inverse.transform option for gh_pop_get_data, gs_pop_get_data
+ Rename flowjo_flog -> flowjo_log_trans
+ Add gs_pop_get_stats_tfilter to prefilter by time range for getting pop stats #287
+ More api renaming according to #274: flowJo_biexp_trans -> flowjo_biexp_trans

# flowWorkspace 3.31.17 (Bioconductor release 3.9)

+ Deprecate many method names to change over to new naming standard
  -- see #274 and help(`flowWorkspace-deprecated`) for summary
+ Add additional.sampleID option to parseWorkspace to help uniquely identify samples
+ Add parseWorkspace option to disable transformation of gates and data
+ Add fcs_file_extension optional argument to parseWorkspace
+ normalizePath for parseWorkspace path and h5_dir arguments
+ Move parseWorkspace to CytoML package
+ normalizePath for load_gs
+ Defunct plotGate and remove some flowViz dependendencies
+ Parallelizion and efficient improvements for getPopStats (gh_pop_get_stats/gs_pop_get_stats)
+ Add get_leaf_nodes
+ Add copyNode method for copying node (and its decscendants) to another point in gating tree
+ Add getFullNodePath to convert partial node paths to full
+ Add transform_gate methods for geometric transformations of gates from GatingSet nodes

# flowWorkspace 3.11.32

## Enhancements
+ stores the axis information in 'axis' slot of 'GatingSet' class to be used by plotGate
+ allow different orders of colnames of flow data when merging multiple 'GatingSet's into 'GatingSetList'
+ add hidden nodes support
+ annotate hidden and boolean nodes in 'gating tree'
+ wrap 'getNodes' logic into c++ to speed it up
+ add 'flowJoTrans' function to construct the flowJo-type biexponentioal transformation function
+ add multiple overlays support to plotGate method
+ add raw.scale argument to plotGate
+ add 'subset' S3 function to subset the GatingSet/GatingSetList based on 'pData'
+ add 'long' format output from 'getPopStats` method
+ add 'transform' method for 'GatingSet' to transform the flow data associated with the GatingSet and save the transformation functions within 'GatingSet'
+ add some internal functions ('merge-GatingSet.R') to handle merging 'GatingSets'	

# flowWorkspace 0.6.0

This version supports importing of flowJo XML workspaces generated using, up to version 9.2 of the Mac OS X version of flowJo. We do not yet support workpsaces generated using the windows version of flowJo. 

## FEATURES:

* import flowJo XML workspace from the Mac version. (Greg Finak)
* convert GatingHierarchy objects to workflows. (Mike Jiang)
* export workflows to flowJo XML workspace readable by Mac version of flowJo (Mose Andre)
* netCDF support for large data sets. Based on code contributed by Nisat Gopalakrishnan.

## KNOWN ISSUES:
	
* export to flowJo - flowWorkspace will export the correct compensation matrices, but they will need to apply manually via the menu items in flowJo.
	* Navigating between graphs of populations is buggy.
	* export expects to receive a workflow that contains a compensation view and a transformation view as the first two actions in the workflow. 
	* import from flowJ - we may not support all gating configurations. The parsing code was written based on examples. If your workspace is not being imported correctly, contact the authors. If your workspace is not being imported correctly because it's generated by flowJo for windows, it is not supported. Are you sure you're trying to import the XML workspace and not the .jo or .wsp file?
	* flowJo 8.2 for MAC. - XML has some differences and there may be issues importing workspaces into R. We have resolved those bugs that we've found but we have not had the opportunity for extensive testing with this version.
	* speed - import can slow for very large workspaces with many gates.
