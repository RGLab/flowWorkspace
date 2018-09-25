/*
 * R_GatingSet.cpp
 *
 *these are R APIs
 *
 *  Created on: Mar 30, 2012
 *      Author: wjiang2
 */
#include "flowWorkspace/pairVectorRcppWrap.h"
#include "flowWorkspace/openWorkspace.hpp"
using namespace Rcpp;
using namespace cytolib;
using namespace flowWorkspace;
CYTOLIB_INIT()
WS_INIT()

GatingSet * getGsPtr(SEXP _gsPtr){

	if(R_ExternalPtrAddr(_gsPtr)==0)
			throw(domain_error("Null GatingSet pointer!"));
	XPtr<GatingSet>gs(_gsPtr);

	return gs;
}
/*
 * can't use module for exposing overloaded methods
 */


//[[Rcpp::export]]
XPtr<flowJoWorkspace> open_workspace(string filename, int sample_name_location, int xmlParserOption)
{

  unique_ptr<flowJoWorkspace> ws = openWorkspace(filename, static_cast<SAMPLE_NAME_LOCATION>(sample_name_location),xmlParserOption);

  return XPtr<flowJoWorkspace>(ws.release());
}

//[[Rcpp::export]]
XPtr<GatingSet> parse_workspace(XPtr<flowJoWorkspace> ws
                                  , int group_id
                                  , List subset
                                  , bool execute
                                  , string path
                                  , string h5_dir
                                  , bool includeGates
                                  , vector<string> additional_keys
                                  , vector<string> keywords
                                  , bool is_pheno_data_from_FCS
                                  , bool keyword_ignore_case
                                  , float extend_val
                                  , float extend_to
                                  , bool channel_ignore_case
                                  , bool leaf_bool
                                  , IntegerVector which_lines = IntegerVector::create() //argument from this line and below are for fcs parser
                                  , string transformation="linearize"
                                  , float decades=0
                                  , bool truncate_min_val = false
                                  , float min_limit=-111
                                  , bool truncate_max_range = true
                                  , int dataset = 1
                                  , bool emptyValue= true
                                  , bool ignoreTextOffset = true
                                  , bool onlyTxt = false
                                  , int num_threads = 1
)
{
  ParseWorkspaceParameters config;
  //ws parser config
  config.data_dir = path;
  config.h5_dir = h5_dir;
  config.is_gating = execute;
  config.is_parse_gate = includeGates;
  config.is_pheno_data_from_FCS = is_pheno_data_from_FCS;
  config.keyword_ignore_case = keyword_ignore_case;
  config.keywords_for_pheno_data = keywords;
  config.keywords_for_uid = additional_keys;
  config.gate_extend_trigger_value = extend_val;
  config.gate_extend_to = extend_to;
  config.channel_ignore_case = channel_ignore_case;
  config.compute_leaf_bool_node = leaf_bool;

  SEXP nm = subset.names();
  if(!Rf_isNull(nm))//without NULL checking, the following line will fail
  {
	  vector<string> filter_names = as<vector<string> >(nm);

	  for(unsigned i = 0; i < filter_names.size(); i++)
	  {
		string filter_name = filter_names[i];
		config.sample_filters[filter_name] = as<vector<string>>(subset[filter_name]);
	  }
  }
  //fcs parser config
  config.fcs_read_param.header.ignoreTextOffset = ignoreTextOffset;
  config.fcs_read_param.header.nDataset = dataset;
  config.fcs_read_param.header.isEmptyKeyValue = emptyValue;

  config.fcs_read_param.data.which_lines = as<vector<int>>(which_lines);
  config.fcs_read_param.data.decades = decades;
  config.fcs_read_param.data.truncate_min_val = truncate_min_val;
  config.fcs_read_param.data.min_limit = min_limit;
  config.fcs_read_param.data.truncate_max_range = truncate_max_range;
  config.fcs_read_param.data.num_threads = num_threads;
  if(transformation=="linearize")
    config.fcs_read_param.data.transform = TransformType::linearize;
  else if(transformation=="none")
    config.fcs_read_param.data.transform = TransformType::none;
  else if(transformation=="linearize_with_PnG_scaling")
    config.fcs_read_param.data.transform = TransformType::linearize_with_PnG_scaling;
  else if(transformation=="scale")
    config.fcs_read_param.data.transform = TransformType::scale;
  else
    stop("unkown transformation type :" + transformation);

  unique_ptr<GatingSet> gs = ws->to_GatingSet(group_id, config);
  return XPtr<GatingSet>(gs.release());
}

//[[Rcpp::export]]
KW_PAIR get_keywords_by_id(XPtr<flowJoWorkspace> ws, int sample_id)
{
  return ws->get_keywords(sample_id).getPairs();
}

//[[Rcpp::export]]
KW_PAIR get_keywords_by_name(XPtr<flowJoWorkspace> ws, string sample_name)
{
  wsSampleNode node = ws->get_sample_node(sample_name);
  return ws->get_keywords(node).getPairs();
}

//[[Rcpp::export]]
List get_sample_groups(XPtr<flowJoWorkspace> ws)
{

  vector<SampleGroup> groups = ws->get_sample_groups(ParseWorkspaceParameters());
  unsigned nGroup = groups.size();
  IntegerVector group_ids(nGroup);
  StringVector group_names(nGroup);
  List sample_ids(nGroup);
  for(unsigned i = 0; i < nGroup; i++)
  {
    group_ids[i] = i;
    group_names[i] = groups[i].group_name;
    unsigned nSample = groups[i].sample_info_vec.size();
    IntegerVector sample_id_vec(nSample);
    for(unsigned j = 0; j < nSample; j++)
      sample_id_vec[j] = groups[i].sample_info_vec[j].sample_id;
    sample_ids[i] = sample_id_vec;
  }

  return List::create(Named("groupID") = group_ids
                    , Named("groupName") = group_names
                     , Named("sampleID") = sample_ids
                    );
}

//[[Rcpp::export]]
List get_samples(XPtr<flowJoWorkspace> ws)
{

  vector<SampleGroup> groups = ws->get_sample_groups(ParseWorkspaceParameters());
  unsigned nGroup = groups.size();
  List grouplist(nGroup);
  for(unsigned i = 0; i < nGroup; i++)
  {
    const vector<SampleInfo> & sample_info_vec = groups[i].sample_info_vec;
    unsigned nSample = sample_info_vec.size();
    List samples(nSample);

    for(unsigned j = 0; j < nSample; j++)
    {
      const SampleInfo & sample_info = sample_info_vec[j];
      samples[j] = List::create(Named("sampleID") = sample_info.sample_id
                                  , Named("name") = sample_info.sample_name
                                  , Named("count") = sample_info.total_event_count
                                  , Named("pop.counts") = sample_info.population_count
                                  );

    }

    grouplist[i] = samples;
  }

  return grouplist;
}


// //[[Rcpp::export]]
// string get_version(XPtr<flowJoWorkspace> ws)
// {
//   return ws->parseVersionList();
// }

//[[Rcpp::export]]
string get_xml_file_path(XPtr<flowJoWorkspace> ws)
{
  return ws->get_xml_file_path();
}


