/*
 * flowJoWorkspace.hpp
 *
 *  Created on: Mar 15, 2012
 *      Author: wjiang2
 */

#ifndef FLOWJOWORKSPACE_HPP_
#define FLOWJOWORKSPACE_HPP_

#include "workspace.hpp"
#include "cytolib/trans_group.hpp"
#include <sstream>
#include <boost/lexical_cast.hpp>
#include <boost/tokenizer.hpp>

namespace flowWorkspace
{

struct SampleInfo{
	int sample_id;
	string sample_name;
	int total_event_count;
	int population_count;
	compensation comp;
	wsSampleNode sample_node;
	KEY_WORDS keywords;
};
struct SampleGroup{
	string group_name;
	vector<SampleInfo> sample_info_vec;
};
struct ParseWorkspaceParameters
{
	 bool is_gating = true;; // whether to load FCS data and perform gating
	 bool is_parse_gate = true;; // whether to parse the gates, can be turned off for parsing pop stats only
	 bool is_pheno_data_from_FCS = false;; // whether to extract keywords for pdata from FCS or workspace
	 vector<string> keywords_for_pheno_data = {}; // the keywords to be extracted as pdata for cytoframe
	 vector<string> keywords_for_uid= {"$TOT"};  // keywords to be used along with sample name for constructing sample uid
	 bool keyword_ignore_case = false;; // whether to ignore letter case when extract keywords for pdata (can be turned off when the letter case are not consistent across samples)
	 bool channel_ignore_case = false;; //  whether the colnames(channel names) matching needs to be case sensitive (e.g. compensation, gating..)
	 float gate_extend_trigger_value = 0; //the threshold that determine wether the gates need to be extended. default is 0. It is triggered when gate coordinates are below this value.
	 float gate_extend_to = -4000;// the value that gate coordinates are extended to. Default is -4000. Usually this value will be automatically detected according to the real data range.
	 unordered_map<string, vector<string>> sample_filters;
	 string data_dir = ""; //path for FCS directory
	 bool is_h5 = true;;
	 bool compute_leaf_bool_node = true;;
	 string h5_dir = generate_unique_dir(fs::temp_directory_path(), "gs");// output path for generating the h5 files
	 FCS_READ_PARAM fcs_read_param;
	 unordered_map<string, compensation> compensation_map;//optional customized sample-specific compensations
	 compensation global_comp;
	 string fcs_file_extension = ".fcs";
//	ParseWorkspaceParameters()
//	 {
//		is_gating = true;
//		is_parse_gate = true;
//		is_pheno_data_from_FCS = false;
//		keywords_for_pheno_data= {};
//		keywords_for_uid = {"$TOT"};
//		keyword_ignore_case = false;
//		channel_ignore_case = false;
//		gate_extend_trigger_value = 0;
//		gate_extend_to = -4000;
//		data_dir = "";
//		is_h5 = true;
//		compute_leaf_bool_node = true;
//		h5_dir = fs::temp_directory_path().string();
//
//	 }
};

class flowJoWorkspace:public workspace{
private:
	string versionList;//used for legacy mac ws
public:

	flowJoWorkspace(xmlDoc * doc){

		nodePath.group="/Workspace/Groups/GroupNode";// abs path
		nodePath.sampleRef=".//SampleRef";//relative GroupNode
		nodePath.sample="/Workspace/SampleList/Sample";//abs path
		nodePath.sampleNode="./SampleNode";//relative to sample

		nodePath.attrName = "name";
		nodePath.compMatName = "name";
		nodePath.compMatChName = "name";
		nodePath.compMatVal = "value";

		this->doc=doc;
		doc_root = wsNode(xmlDocGetRootElement(doc));

	}

	string get_xml_file_path(){
		return (const char*)(doc->URL);
	}
	/*
	  * Constructor that starts from a particular sampleNode from workspace to build a tree
	  */
	GatingHierarchyPtr to_GatingHierarchy(wsSampleNode curSampleNode,bool is_parse_gate,const trans_global_vec & _gTrans)
	 {
		GatingHierarchyPtr gh(new GatingHierarchy());
	 	wsRootNode root=getRoot(curSampleNode);
	 	if(is_parse_gate)
	 	{

	 		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
	 			COUT<<endl<<"parsing compensation..."<<endl;
	 		compensation comp=getCompensation(curSampleNode);

	 		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
	 			COUT<<endl<<"parsing trans flags..."<<endl;
	 		PARAM_VEC transFlag=getTransFlag(curSampleNode);

	 		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
	 			COUT<<endl<<"parsing transformation..."<<endl;
	 		//prefixed version
	 		trans_local trans = getTransformation(root,comp,transFlag,_gTrans, true);

	 		/*
	 		 * unprefixed version. Both version of trans are added (sometime they are identical)
	 		 * so that the trans defined on uncompensated channel (e.g. SSC-A) can still be valid
	 		 * without being necessarily adding comp prefix.
	 		 * It is mainly to patch the legacy workspace of mac or win where the implicit trans is added for channel
	 		 * when its 'log' keyword is 1.
	 		 * vX doesn't have this issue since trans for each parameter/channel
	 		 * is explicitly defined in transform node.
	 		 */
	 		trans_local trans_raw=getTransformation(root,comp,transFlag,_gTrans, false);
	 		//merge raw version of trans map to theprefixed version
	 		trans_map tp = trans_raw.getTransMap();
	 		for(trans_map::iterator it=tp.begin();it!=tp.end();it++)
	 		{
	 			trans.addTrans(it->first, it->second);
	 		}
	 		gh.reset(new GatingHierarchy(comp, transFlag, trans));

	 	}
	 	if(g_loglevel>=GATING_HIERARCHY_LEVEL)
			COUT<<endl<<"parsing DerivedParameters..."<<endl;
	 	unordered_set<string> derived_params = get_derivedparameters(curSampleNode);

	 	if(g_loglevel>=POPULATION_LEVEL)
	 		COUT<<endl<<"parsing populations..."<<endl;

	 	populationTree &tree = gh->getTree();
	 	VertexID pVerID=addRoot(tree, root);
	 	addPopulation(tree, pVerID,&root,is_parse_gate, derived_params);
	 	return gh;
	 }

	unique_ptr<GatingSet> to_GatingSet(unsigned group_id, ParseWorkspaceParameters config)
	{

		vector<SampleInfo> sample_infos = get_sample_info(group_id, config);

		auto it_filter = config.sample_filters.find("name");
		if(it_filter != config.sample_filters.end())
			config.sample_filters.erase(it_filter);								// remove name filter from filters

		unique_ptr<GatingSet> gs(new GatingSet());
		 /*
		  * parsing global calibration tables
		  */
		 trans_global_vec gTrans;
		 if(config.is_parse_gate)
		 {
			 if(g_loglevel>=GATING_SET_LEVEL)
				 COUT<<"... start parsing global transformations... "<<endl;
			 gTrans=getGlobalTrans();

		 }


		 string data_dir = config.data_dir;
		 fs::path h5_dir = fs::path(config.h5_dir);
		 if(config.is_gating)
		 {

			if(data_dir == "")
			{
				//use xml dir
				string xml_filepath = get_xml_file_path();
				data_dir = path_dir_name(xml_filepath);
			}
			h5_dir = gs->generate_h5_folder(h5_dir);
		 }

		 GatingSet cytoset;
		/*
		 * try to parse each sample
		 */
		for(auto & p : sample_infos)
		{

			if(g_loglevel>=GATING_HIERARCHY_LEVEL)
				COUT<<endl<<"... start parsing sample: "<< p.sample_name <<"... "<<endl;
			//generate uid
			string uid = p.sample_name + concatenate_keywords(p.keywords, config.keywords_for_uid);


			if(config.is_gating)
			{
				//match FCS
				search_for_fcs(data_dir, p, config, cytoset);

			}

			auto it = cytoset.find(uid);

			//proceed gate parsing when data is available or gate-parsing only
			if(it!= cytoset.end() || !config.is_gating)
			{

				CytoFrameView frv;

				if(config.is_gating)
				{
					frv = it->second->get_cytoframe_view();
					if(g_loglevel>=GATING_HIERARCHY_LEVEL)
						cout<<endl<<"Extracting pheno data from keywords for sample: " + uid<<endl;


					//set pdata
					frv.set_pheno_data("name", p.sample_name);

					KEY_WORDS & keys = p.keywords;
					if(config.is_pheno_data_from_FCS)
						keys = frv.get_keywords();

					for(const string & k : config.keywords_for_pheno_data)
					{
						//todo::case insensitive search
						if(config.keyword_ignore_case)
							throw(domain_error("keyword_ignore_case not supported yet!"));
						auto it_k = keys.find(k);
						if(it_k == keys.end())
							throw(domain_error("keyword '" + k + "' not found in sample: " + uid));

						frv.set_pheno_data(it_k->first, it_k->second);
					}

					//filter sample by FCS keyword
					if(config.is_pheno_data_from_FCS && !check_sample_filter(config.sample_filters, frv.get_pheno_data()))//skip parsing this sample if not passed filter
					{
						if(g_loglevel>=GATING_HIERARCHY_LEVEL)

						cout<<endl<<"Skipping sample: " + uid + " by filter"<<endl;

						continue;
					}
				}

				//parse gating tree
				GatingHierarchyPtr gh = to_GatingHierarchy(p.sample_node,config.is_parse_gate,gTrans);

				if(g_loglevel>=GATING_HIERARCHY_LEVEL)
					COUT<<"Gating hierarchy created: "<<uid<<endl;

				if(config.is_gating)
				{
					if(g_loglevel>=GATING_HIERARCHY_LEVEL)
						cout<<endl<<"Gating ..."<<endl;
					//we know we created Mem version of cytoFrame during the parsing
					//thus it is valid cast
					CytoFramePtr frptr = frv.get_cytoframe_ptr();
					MemCytoFrame & fr = dynamic_cast<MemCytoFrame &>(*frptr);

					//load the data into the header-only version of cytoframe
					fr.read_fcs_data();

					compensation comp;
					if(!config.global_comp.empty())
					{
						comp = config.global_comp;
					}
					else
					{
						auto it_comp = config.compensation_map.find(uid);
						if(it_comp != config.compensation_map.end())
						{
							comp = it_comp->second;

						}
					}
					comp.cid = "1";
					gh->set_compensation(comp, true);
					gh->compensate(fr);
					gh->transform_gate();
					gh->transform_data(fr);
					gh->extendGate(fr, config.gate_extend_trigger_value);
					gh->gating(fr, 0,false, config.compute_leaf_bool_node);

					unique_ptr<CytoFrame> frame_ptr;
					if(config.is_h5)
					{
						string h5_filename = (h5_dir/uid).string() + ".h5";
						fr.write_h5(h5_filename);
						gh->set_cytoFrame_view(CytoFrameView(CytoFramePtr(new H5CytoFrame(h5_filename))));
					}

				}
				else
				{
					gh->transform_gate();
					gh->extendGate(config.gate_extend_trigger_value, config.gate_extend_to);

				}
				gs->add_GatingHierarchy(gh, uid);
			}

		}
		if(gs->size() == 0)
			throw(domain_error("No samples in this workspace to parse!"));


		return gs;
	}
	/**
	 * Search for the FCS file
	 * First try to search by file name, if failed, use FCS keyword $FIL + additional keywords for further searching and pruning
	 * cytoframe is preloaded with header-only.
	 */
	void search_for_fcs(const string & data_dir, const SampleInfo  & sample_info, const ParseWorkspaceParameters & config, GatingSet & cytoset)
	{
		FCS_READ_PARAM fcs_read_param = config.fcs_read_param;
		fcs_read_param.header.is_fix_slash_in_channel_name = is_fix_slash_in_channel_name();

		string ws_key_seq = concatenate_keywords(sample_info.keywords, config.keywords_for_uid);
		string uid = sample_info.sample_name + ws_key_seq;

		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
			COUT<<endl<<"searching for FCS for sample: " + uid <<endl;
		//try to search by file name first
		vector<string> all_file_paths = list_files(data_dir, config.fcs_file_extension);
		vector<string> file_paths;
		for(auto i : all_file_paths)
		{
			if(path_base_name(i) == sample_info.sample_name)
				file_paths.push_back(i);
		}
		if(file_paths.size() == 0)
		{
			//search by keyword by traversing the data dir recursively until the target is found
			for(const string & file_path : all_file_paths)
			{

				shared_ptr<MemCytoFrame> fr (new MemCytoFrame(file_path, fcs_read_param));
				fr->read_fcs_header();
				if(fr->get_keyword("$FIL") == sample_info.sample_name &&
						ws_key_seq == concatenate_keywords(fr->get_keywords(), config.keywords_for_uid))
				{
					cytoset.add_cytoframe_view(uid, CytoFrameView(fr));
					return;//to avoid scanning entire folder, terminate the search immediately on the first hit assuming the uid is unique
				}
			}


		}
		else
		{//further verify/pruning the matched files by looking at the keywords

			for(const string & file_path : file_paths)
			{
				shared_ptr<MemCytoFrame> fr (new MemCytoFrame(file_path, fcs_read_param));
				fr->read_fcs_header();
				string fcs_key_seq = concatenate_keywords(fr->get_keywords(), config.keywords_for_uid);

				if(fcs_key_seq == ws_key_seq)//found matched one
				{
					if(cytoset.find(uid)!=cytoset.end())
						throw(domain_error("Duplicated FCS found for sample " + uid));

					cytoset.add_cytoframe_view(uid, CytoFrameView(fr));
				}
			}

		}
		if(cytoset.find(uid) == cytoset.end())
			PRINT("FCS not found for sample " + uid + "\n");

	}
	/**
	 * Generate the uniquely identifiable id for each sample
	 * by concatenate sample name with some other keywords
	 * @param node
	 * @param keywords_for_uid
	 * @return
	 */
	string concatenate_keywords(const KEY_WORDS & keywords, const vector<string> & keywords_for_uid)
	{
		string uid = "";
		for(const string & key : keywords_for_uid)
		{
			auto it = keywords.find(key);
			if(it == keywords.end())
				throw(domain_error("Keyword not found in workspace: " + key + " for sample " + uid));
			uid += "_" + it->second;
		}
		return uid;
	}

	vector<wsGroupNode> get_group_nodes()
	{
		xmlXPathContextPtr context = xmlXPathNewContext(doc);
		//parse group info
		xmlXPathObjectPtr grp_result = xmlXPathEval((xmlChar *)nodePath.group.c_str(), context);
		xmlXPathFreeContext(context);

		if(xmlXPathNodeSetIsEmpty(grp_result->nodesetval)){
			xmlXPathFreeObject(grp_result);
			 throw(domain_error("No Groups infomation!"));
		}
		int nGroup = grp_result->nodesetval->nodeNr;
		vector<wsGroupNode> res(nGroup);
		for(int i = 0; i < nGroup; i++)
		{
			xmlNodePtr cur=grp_result->nodesetval->nodeTab[i];
			res[i] = wsGroupNode(cur);
		}
		xmlXPathFreeObject(grp_result);

		return res;
	}
	vector<SampleGroup> get_sample_groups(const ParseWorkspaceParameters & config)
	{

		vector<wsGroupNode> nodes = get_group_nodes();
		unsigned nGroup = nodes.size();
		vector<SampleGroup> res(nGroup);
		for(unsigned i = 0; i < nGroup; i++)
		{
			res[i].group_name = nodes[i].getProperty("name");
			res[i].sample_info_vec = get_sample_info(i, config);
		}

		return res;

	}
	vector<SampleInfo> get_sample_info(unsigned group_id, const ParseWorkspaceParameters & config)
	{
		if(g_loglevel>=GATING_SET_LEVEL)
				cout<<endl<<"Parsing sample information for group: " + to_string(group_id) <<endl;
		vector<wsGroupNode> nodes = get_group_nodes();
		unsigned nGroup = nodes.size();

		if(group_id >= nGroup){
			 throw(domain_error("Invalid group id: " + to_string(group_id) + "\n"));
		}
		wsGroupNode grp_node = nodes[group_id];
		//parse sample ref node
		xmlXPathObjectPtr sids=grp_node.xpathInNode(nodePath.sampleRef.c_str());
		xmlNodeSetPtr nodeSet=sids->nodesetval;
		int nSample = nodeSet->nodeNr;
		vector<SampleInfo> sample_info_vec;
		for(int j = 0; j < nSample; j++)
		{
			wsNode cur_sample_ref_node(nodeSet->nodeTab[j]);
			string sampleID=cur_sample_ref_node.getProperty("sampleID");
			int sample_id = boost::lexical_cast<int>(sampleID);
//			if(g_loglevel>=GATING_SET_LEVEL)
//				cout<<endl<<"Search sample for sampleID: " + sampleID <<endl;
			//parse sample node

			auto sample_vec = get_sample_node(sample_id);
			if(sample_vec.size() > 0)
			{
				if(sample_vec.size() > 1)
					throw(domain_error("multiple sample nodes matched to the sample id: " + sampleID + "!"));

				if(g_loglevel>=GATING_HIERARCHY_LEVEL)
					cout<<endl<<"Parsing sample information for sampleID: " + sampleID <<endl;
				SampleInfo sample_info;
				sample_info.sample_node = sample_vec[0];
				sample_info.sample_id = sample_id;
				sample_info.population_count = get_population_count(sample_info.sample_node);

				// filter sample by pop.count
				if(sample_info.population_count == 0)
					continue;

				sample_info.total_event_count = get_event_count(getRoot(sample_info.sample_node));
				//cache sample_name for the same reason
				sample_info.sample_name = get_sample_name(sample_info.sample_node);
				//filter by name
				unordered_map<string, vector<string>> sample_filter = config.sample_filters;
				auto it_filter = sample_filter.find("name");



				if(it_filter != sample_filter.end())
				{

					if(g_loglevel>=GATING_SET_LEVEL)
						cout<<endl<<"filter by sample name: " + sample_info.sample_name <<endl;
					if(!check_sample_filter<PDATA>(*it_filter,{{"name",sample_info.sample_name}}))
					{
						// remove name filter from filters
						sample_filter.erase(it_filter);
						continue;
					}

				}

				//pre-parse and cache the keywords from ws to avoid parsing it multiple times later
				sample_info.keywords = get_keywords(sample_info.sample_node);
				//filter by other pheno data
				if(config.is_pheno_data_from_FCS)
				{
					if(g_loglevel>=GATING_SET_LEVEL)
						cout<<endl<<"filter by workspace keyword: " <<endl;
					if(!check_sample_filter<KEY_WORDS>(sample_filter, sample_info.keywords))
						continue;
				}
				//save the sample info that passed the filter
				sample_info_vec.push_back(sample_info);

			}
		}
		xmlXPathFreeObject(sids);
		return sample_info_vec;
	}

	template<typename T>
	bool check_sample_filter(const pair<string, vector<string>> & sample_filter, const T & pheno_data)
	{

		//apply each filter
		string filter_name = sample_filter.first;
		const auto & it_pdata = pheno_data.find(filter_name);
		if(it_pdata == pheno_data.end())
			throw(domain_error("filter '" + filter_name + "' not found in sample pheno data: "));

		const vector<string> & filter_values = sample_filter.second;
		return find(filter_values.begin(), filter_values.end(), it_pdata->second) != filter_values.end();

	}

	template<typename T>
	bool check_sample_filter(const unordered_map<string, vector<string>> & sample_filter, const T & pheno_data)
	{
		for(auto & it_filter : sample_filter)
		{
			if(!check_sample_filter(it_filter, pheno_data))
				return false;
		}
		return true;
	}
	KEY_WORDS get_keywords(int sample_id)
	{
		auto sample_vec = get_sample_node(sample_id);
		if(sample_vec.size() == 0)
			throw(domain_error("sample id: " + to_string(sample_id) + " not found!"));
		if(sample_vec.size() > 1)
			throw(domain_error("multiple sample nodes matched to the sample id: " + to_string(sample_id) + "!"));

		return get_keywords(sample_vec[0]);
	}
	KEY_WORDS get_keywords(wsSampleNode & node)
	{
		KEY_WORDS keys;
		xmlXPathObjectPtr res=node.xpathInNode("Keywords/Keyword");
		xmlNodeSetPtr nodeSet=res->nodesetval;
		for(int j = 0; j < nodeSet->nodeNr; j++)
		{
			wsNode key_node(nodeSet->nodeTab[j]);
			string val = key_node.getProperty("value");
			boost::trim(val);
			keys[key_node.getProperty("name")] = val;
		}
		return keys;
	}

	vector<wsSampleNode> get_sample_node(int sample_id){
		string s_sample_id = to_string(sample_id);
		string path = xPathSample(s_sample_id);
		xmlXPathObjectPtr res = doc_root.xpath(path, false);

		vector<wsSampleNode> node_vec;
		if(res&&res->nodesetval)
		{
			int nSample = res->nodesetval->nodeNr;
			node_vec.resize(nSample);

			for(int i = 0; i < nSample; i++)
				node_vec[i] = wsSampleNode(res->nodesetval->nodeTab[i]);
		}
		xmlXPathFreeObject(res);
		return node_vec;
	}
	/**
	 * Search for sample node by name
	 * Currently it is used by Rcpp API
	 * @param sample_name
	 * @return
	 */
	wsSampleNode get_sample_node(string sample_name)
	{
		xmlXPathObjectPtr res;
		switch(nodePath.sample_name_location)
		{
			case SAMPLE_NAME_LOCATION::KEY_WORD:
			{
				res = doc_root.xpath(nodePath.sample + "/Keywords/Keyword[@name='$FIL' and @value='" + sample_name + "']/../..");

				break;

			}
			case SAMPLE_NAME_LOCATION::SAMPLE_NODE:
			{
				res = doc_root.xpath(nodePath.sample + "/SampleNode[@name='" + sample_name + "']/..");
				break;
			}

			default:
				throw(domain_error("unknown sampleName Location!It should be either 'keyword' or 'sampleNode'."));
		}
		if(res->nodesetval->nodeNr == 0)
		{
			xmlXPathFreeObject(res);
			throw(domain_error("sample not found: " + sample_name));
		}
		if(res->nodesetval->nodeNr > 1)
		{
			xmlXPathFreeObject(res);
			throw(domain_error("Multiple sample nodes found for : " + sample_name));
		}
		wsSampleNode node(res->nodesetval->nodeTab[0]);
		xmlXPathFreeObject(res);
		return node;
	}
	/*
	 * .
	 * By default sampleName is fetched from keywords/$FIL
	 *  However the $FIL may not be consistent with the sampleNode "name" attribute
	 *   and the physical FCS filename,so optionally we allow parsing it
	 *   from name attribute from SampleNode
	 *
	 */
	string get_sample_name(wsSampleNode & node){
		string filename;
		switch(nodePath.sample_name_location)
		{
			case SAMPLE_NAME_LOCATION::KEY_WORD:
			{
				xmlXPathObjectPtr res=node.xpathInNode("Keywords/Keyword[@name='$FIL']");
				if(res->nodesetval->nodeNr!=1){
					xmlXPathFreeObject(res);
					throw(domain_error("$FIL keyword not found!"));
				}

				wsNode kwNode(res->nodesetval->nodeTab[0]);
				xmlXPathFreeObject(res);
				filename=kwNode.getProperty("value");
				break;
			}
			case SAMPLE_NAME_LOCATION::SAMPLE_NODE:
			{
				xmlXPathObjectPtr res=node.xpathInNode("SampleNode");//get sampleNode
				wsNode sampleNode(res->nodesetval->nodeTab[0]);
				xmlXPathFreeObject(res);

				filename=sampleNode.getProperty(nodePath.attrName);//get property name from sampleNode
				break;
			}

			default:
				throw(domain_error("unknown sampleName Location!It should be either 'keyword' or 'sampleNode'."));
		}

		if(filename.empty())
			throw(domain_error("$FIL value is empty!"));
		//fs:: make_preferred not working yet, have to manually handle windows path
		boost::replace_all(filename, "\\", "/");
		return path_base_name(filename);
	}

	/*
	 * get transformation for one particular sample node
	 */



	//xquery the "SampleNode" within "sample" context
	wsRootNode getRoot(wsSampleNode sample)
	{
	//	COUT<<"parsing root node"<<endl;
		xmlXPathObjectPtr res=sample.xpathInNode(nodePath.sampleNode);
		wsRootNode node(res->nodesetval->nodeTab[0]);
		xmlXPathFreeObject(res);
	//	COUT<<nodePath.sampleNode<<endl;
		return node;
	}


	wsPopNodeSet getSubPop(wsNode * node)
	{

		xmlXPathObjectPtr res=node->xpathInNode(nodePath.popNode);
		int nChildren=res->nodesetval->nodeNr;
	//	wsPopNodeSet childenNodes(res->nodesetval->nodeTab,nChildren);
		wsPopNodeSet childenNodes;
		for(int i=0;i<nChildren;i++)
		{
			childenNodes.push_back(wsPopNode(res->nodesetval->nodeTab[i]));
		}

		xmlXPathFreeObject(res);

		return childenNodes;

	}

	/*
	 * this is for windows version currently because windows version does not have parameter nodes,
	 * Not sure whether to get "Range" info for windows though
	 *
	 *
	 */
	PARAM_VEC getTransFlag(wsSampleNode sampleNode){
		PARAM_VEC res;

		/*
		 * get total number of channels
		 */
		string path="Keywords/*[@name='$PAR']";
		xmlXPathObjectPtr parRes=sampleNode.xpathInNode(path);
		wsNode parNode(parRes->nodesetval->nodeTab[0]);
		xmlXPathFreeObject(parRes);
		unsigned short nPar=atoi(parNode.getProperty("value").c_str());

		/*
		 * get info about whether channel should be transformed
		 */

		for(unsigned i=1;i<=nPar;i++)
		{
			PARAM curParam;

			/*
			 * get curernt param name
			 */
			stringstream ss(stringstream::in | stringstream::out);
			ss << "Keywords/*[@name='$P"<< i<<"N']";
			path=ss.str();
			xmlXPathObjectPtr parN=sampleNode.xpathInNode(path);
			wsNode curPNode(parN->nodesetval->nodeTab[0]);
			xmlXPathFreeObject(parN);
			string pName=curPNode.getProperty("value");

			/*
			 * get current display flag
			 */
			stringstream ss1(stringstream::in | stringstream::out);
			ss1 << "Keywords/*[@name='P"<<i<<"DISPLAY']";
			path=ss1.str();
			xmlXPathObjectPtr parDisplay=sampleNode.xpathInNode(path);
			wsNode curDisplayNode(parDisplay->nodesetval->nodeTab[0]);
			xmlXPathFreeObject(parDisplay);
			string curFlag=curDisplayNode.getProperty("value");

			/**
			 * get PnR (mainly used to init T value in flog)
			 */
			stringstream ss2(stringstream::in | stringstream::out);
			ss2 << "Keywords/*[@name='$P"<<i<<"R']";
			path=ss2.str();
			xmlXPathObjectPtr parR=sampleNode.xpathInNode(path);
			wsNode curRNode(parR->nodesetval->nodeTab[0]);
			xmlXPathFreeObject(parR);
			string curR=curRNode.getProperty("value");
			/*
			 * check if data is already stored in log scale
			 * so that pnR may need to be calculated by PnE
			 */
			float f1 = 0, f2 = 0;//init with default values in case PnE is missing
			stringstream ss3(stringstream::in | stringstream::out);
			ss3 << "Keywords/*[@name='$P"<<i<<"E']";
			path=ss3.str();
			xmlXPathObjectPtr parE=sampleNode.xpathInNode(path);
			if(parE->nodesetval->nodeNr > 0)
			{
				wsNode curENode(parE->nodesetval->nodeTab[0]);

				string curE=curENode.getProperty("value");
				vector<string> tokens;
				boost::split(tokens, curE, boost::is_any_of(","));
				f1 = stof(tokens[0]);
				f2 = stof(tokens[1]);
				if(f1 > 0 && f2 == 0)//correct f2 for legacy FCS 2.0
					f2 = 1;
			}
			xmlXPathFreeObject(parE);
			if(f1 > 0)
				curParam.range = pow(10, f1) * f2;
			else
				curParam.range=atoi(curR.c_str());
			curParam.param=pName;
			curParam.log=curFlag.compare("LOG")==0;


			if(g_loglevel>=GATING_SET_LEVEL)
				COUT<<pName<<":"<<curFlag;
			/*
			 * We can't determine '$TIMESTEP' soly from workspace since the this keyword value in xml is not as reliable as the one in FCS TEXT
			 */
	//		if(pName.compare("Time") == 0||pName.compare("time") == 0){
	//			path="Keywords/*[@name='$TIMESTEP']";
	//			xmlXPathObjectPtr parRes=sampleNode.xpathInNode(path);
	//			wsNode parNode(parRes->nodesetval->nodeTab[0]);
	//			xmlXPathFreeObject(parRes);
	//			string sTimestep = parNode.getProperty("value");
	//			curParam.timestep=strtod(sTimestep.c_str(), NULL);
	//			if(g_loglevel>=GATING_SET_LEVEL)
	//				COUT<<sTimestep;
	//		}
			if(g_loglevel>=GATING_SET_LEVEL)
						COUT<<endl;

			res.push_back(curParam);
		}
		return res;
	}


	int get_event_count(const wsRootNode & node)
	{

		return atoi(node.getProperty("count").c_str());
	}

	int get_population_count(const wsSampleNode & node)
	{
		//libxml doesn't seem to support count() function in xpath
		xmlXPathObjectPtr res = node.xpathInNode(".//Population");
		int n = res->nodesetval->nodeNr;
		xmlXPathFreeObject(res);
		return n;
	}


	/*
	 *Note: nodeProperties is dynamically allocated and up to caller to free it
	 */
	void to_popNode(wsRootNode & node, nodeProperties & np){



		/*
		 * in order to make the pop names comparable accross samples
		 * force the root node name as "root" (it was stored as fcs filenames in some fj ws)
		 */

		np.setName("root");

		POPSTATS fjStats;
		fjStats["count"]= get_event_count(node);
		np.setStats(fjStats,false);


	}

	void to_popNode(wsPopNode &node,nodeProperties & np,bool is_parse_gate=false){



		//add pop name
		np.setName(node.getProperty(nodePath.attrName).c_str());

		if(g_loglevel>=POPULATION_LEVEL)
				COUT<<"parse the population Node:"<<np.getName()<<endl;
		//add pop counts
		POPSTATS fjStats;
		string sCount = node.getProperty("count");
		//set the empty stats to -1
		fjStats["count"] = sCount.empty()?-1:atoi(sCount.c_str());
		np.setStats(fjStats,false);

		try
		{
			if(is_parse_gate)np.setGate(getGate(node));
		}
		catch (logic_error & e) {

			throw(logic_error("extracting gate failed:" + np.getName() + "--" + e.what()));
		}


	}

	//used for legacy mac ws
	void parseVersionList(){
		wsNode root(this->doc->children);
		xmlXPathObjectPtr res = root.xpath("/Workspace");
		wsNode curNode(res->nodesetval->nodeTab[0]);
		xmlXPathFreeObject(res);
		this->versionList=curNode.getProperty("versionList");

	}
	/*
	 * get the minimum initial digit from the version list string
	 * //used for legacy mac ws
	 */
	unsigned short getVersionMin(){
		int res=numeric_limits<int>::max();
		vector<string> vlist;
		boost::split(vlist,versionList,boost::is_any_of(";"));
		for(vector<string>::iterator it=vlist.begin();it!=vlist.end();it++)
		{
			string curVer=*it;
			boost::erase_all(curVer,"Pre");
			vector<string> digits;
			boost::split(digits,curVer,boost::is_any_of("."));
			curVer=digits.at(0).c_str();
			boost::algorithm::trim((curVer));
			if(!curVer.empty())
			{
				int toCompare=boost::lexical_cast<int>(curVer);
				res=min(res,toCompare);
			}

		}
		return res;
	}

	vector<BOOL_GATE_OP> parseBooleanSpec(string specs,vector<string> gPaths){

		vector<BOOL_GATE_OP> res;

		/*
		 * parse the spec strings to get logical operations among populations
		 */

		boost::replace_all(specs," ","");

		//tokenize by boolean operator: & or |

		boost::char_separator<char> sep("", "&|"); // first arg specify dropped seperator and secoond for the kept separators
		boost::tokenizer<boost::char_separator<char>> tokens(specs, sep);
		unsigned short i = 0;
		vector<string> popTokens, opTokens;

		for(string thisToken : tokens)
		{

			if(i%2 == 0)
				popTokens.push_back(thisToken);//like G0, G1...
			else
				opTokens.push_back(thisToken);//operators: |, &
			i++;

		}
		unsigned short nPopulations=popTokens.size();
		if(nPopulations!=gPaths.size())
		{
			cout << specs << endl;
			for(auto p:gPaths)
				cout << p << " ";
			cout << endl;
			for(auto p:popTokens)
						cout << p << " ";
			cout << endl;
			throw(domain_error("the logical operators and the gating paths do not pair correctly!"));
		}


		for(unsigned j=0;j<nPopulations;j++)
		{

			BOOL_GATE_OP gOpObj;


			string curPopToken=popTokens.at(j);
			string curOpToken;
			if(j==0)
				curOpToken="&";//assign and operation to the first token and actually it is not used
			else
				curOpToken=opTokens.at(j-1);
			/*
			 * extract number from token
			 */
			string sIndex=boost::erase_all_copy(curPopToken,"!");
			boost::erase_all(sIndex,"G");
			unsigned short index = boost::lexical_cast<unsigned short>(sIndex);
			/*
			 * select respective gating path string and split it into vector
			 */
			string curPath=gPaths.at(index);
			boost::split(gOpObj.path,curPath,boost::is_any_of("/"));
			/*
			 * the path could either one of the two types:
			 * 1) /pop1
			 * 2) /pop1/pop2
			 * the first case indicate it is a single reference node name,which requires nearest ancestor search
			 * the second is a full path or partial path of the nodes,requires a path matching
			 * EDIT:turns out the second case is also partial path.
			 */
			if(gOpObj.path.at(0).empty())
				gOpObj.path.erase(gOpObj.path.begin());//remove the first empty string


			// parse negate operator
			gOpObj.isNot=curPopToken.find("!")!=string::npos;


			/*
			 * if not |,we assume it as & by skipping the actual matching with "&"
			 * since it stores as &amp; in xml
			 */
			gOpObj.op=boost::iequals(curOpToken,"|")?'|':'&';

			/*
			 * push the parsed gating path vector and operator into result vector
			 */
			res.push_back(gOpObj);

		}
		return res;

	}
};



};

#endif /* FLOWJOWORKSPACE_HPP_ */
