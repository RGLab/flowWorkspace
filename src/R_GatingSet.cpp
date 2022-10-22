/*
 * R_GatingSet.cpp
 *
 *these are R APIs
 *
 *  Created on: Mar 30, 2012
 *      Author: wjiang2
 */

#include "flowWorkspace_types.h"
#include <cpp11.hpp>
GatingSet * getGsPtr(SEXP _gsPtr){

        if(R_ExternalPtrAddr(_gsPtr)==0)
                        throw(domain_error("Null GatingSet pointer!"));
        cpp11::external_pointer<GatingSet>gs(_gsPtr);

        return gs.get();
}


[[cpp11::register]]
void gs_transform_data(cpp11::external_pointer<GatingSet> gsPtr) {
	for(auto sn : gsPtr->get_sample_uids())
	{
		GatingHierarchyPtr gh = gsPtr->getGatingHierarchy(sn);
		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
			cout<<"transforming: "<<sn<<endl;
		CytoFramePtr cf = gh->get_cytoframe_view().get_cytoframe_ptr();

		MemCytoFrame fr(*cf);

		gh->transform_data(fr);
		cf->set_data(fr.get_data());
		cf->set_params(fr.get_params());
		cf->set_keywords(fr.get_keywords());
	}
}
[[cpp11::register]]
void cpp_gating(cpp11::external_pointer<GatingSet> gsPtr, vector<string> nodes, bool alwaysLoadData, bool verbose, bool leafbool) {
  if(nodes[0] == "root")
    alwaysLoadData = true; //skip the checking to save time when start from root

  VertexID_vec nodeIDs(nodes.size());
  GatingSet cs = gsPtr->get_cytoset();
  for(const string & sid : gsPtr->get_sample_uids())
  {
    if(verbose)
      cout << "gating " << sid << endl;
    GatingHierarchyPtr gh = gsPtr->getGatingHierarchy(sid);
    for(unsigned i = 0; i < nodes.size(); i++)
      nodeIDs[i] = gh->getNodeID(nodes[i]);
  
    // Ideally, we want to track back to all ancesters and references to check if they are already gated
    //   in order to determine whether the raw data is needed
    //   but for the sake of speed, we only check the parent and reference node
    //     of the boolGate at the moment
    //     if the further upstream ancester nodes are not gated yet, which will fail the gating
    //       since we are passing the empty dummy data, we will simply throw the error and prompt user
    //       to recompute these upstream gates explicitly
    bool isloadData;
    if(alwaysLoadData)
      isloadData = true;
    else
    {
      bool isAllBoolGate = true;
      for(auto i : nodeIDs)
      {
        auto type = gh->getNodeProperty(i).getGate()->getType();
        if(type!=BOOLGATE&&type!=LOGICALGATE&&type!=CLUSTERGATE)
        {
          isAllBoolGate = false;
          break;
        }
      }
      if(isAllBoolGate)
      {
        isloadData = false;
        for(auto i : nodeIDs)
        {
          //check if parent is gated
          if(!gh->getNodeProperty(gh->getParent(i)).isGated())
          {
            isloadData = true;
            break;
          }
          
          //if check if reference is gated
          bool allrefgated = true;
          boolGate & g = dynamic_cast<boolGate &>(*(gh->getNodeProperty(i).getGate()));
          for(auto j : g.getBoolSpec())
          {
            if(!gh->getNodeProperty(gh->getParent(gh->getNodeID(j.path))).isGated())
            {
              allrefgated = false;
              break;
            }
          }
          if(!allrefgated)
          {
            isloadData = true;
            break;
          }
        }
        
      }
      else
        isloadData = true;
    }
    
    //actual gating
    shared_ptr<MemCytoFrame> fr (new MemCytoFrame());
    if(isloadData)
      fr = cs.get_cytoframe_view(sid).get_realized_memcytoframe();
    for(auto nodeID : nodeIDs)
    {
      try{
        gh->gating(*fr, nodeID, true, leafbool);  
      }
      catch(const std::exception & e)
      {
        string strerr = e.what();
        if(!isloadData&&strerr.find("not found")!=string::npos)
         throw(domain_error("Found ungated upstream population. Set 'alwaysLoadData = TRUE' for 'recompute' method, and try again!"));
        else
          throw(domain_error(strerr));
        
      }
      
    }
      
  }
}

[[cpp11::register]]
cpp11::external_pointer<GatingSet> subset_gs_by_sample(cpp11::external_pointer<GatingSet> gsPtr, vector<string> samples) {

  return cpp11::external_pointer<GatingSet>(new GatingSet(gsPtr->sub_samples(samples)));
}

[[cpp11::register]]
cpp11::external_pointer<GatingSet> get_cytoset(cpp11::external_pointer<GatingSet> gsPtr) {

  return cpp11::external_pointer<GatingSet>(new GatingSet(gsPtr->get_cytoset()));
}

[[cpp11::register]]
cpp11::external_pointer<GatingSet> get_cytoset_from_node(cpp11::external_pointer<GatingSet> gsPtr, string node) {

  return cpp11::external_pointer<GatingSet>(new GatingSet(gsPtr->get_cytoset(node)));
}

[[cpp11::register]]
void set_cytoset(cpp11::external_pointer<GatingSet> gsPtr, cpp11::external_pointer<GatingSet> cs) {

  gsPtr->set_cytoset(*cs);
}

[[cpp11::register]]
StringVec cpp_getSamples(cpp11::external_pointer<GatingSet> gsPtr) {

	return gsPtr->get_sample_uids();
}

/*
 * constructing GatingSet from existing gating hierarchy and new data
 */
[[cpp11::register]]
cpp11::external_pointer<GatingSet> cpp_NewGatingSet(cpp11::external_pointer<GatingSet> gsPtr
               ,string src_sample_uid
			   , cpp11::external_pointer<GatingSet> cs
			   , bool execute
         , string comp_source)
  {

		GatingHierarchy & gh=*gsPtr->getGatingHierarchy(src_sample_uid);

		/*
		 * used gh as the template to clone multiple ghs in the new gs
		 */
		GatingSet * newGS=new GatingSet(gh, *cs, execute, comp_source);

		/*
		 * using default finalizer to delete gs,which is triggered by gc() when
		 * xptr is out of scope
		 */

		return cpp11::external_pointer<GatingSet>(newGS);

}

[[cpp11::register]]
string get_gatingset_id(cpp11::external_pointer<GatingSet> gsPtr) {

	return gsPtr->get_uid();
}
[[cpp11::register]]
void set_gatingset_id(cpp11::external_pointer<GatingSet> gsPtr, string id) {

	 gsPtr->set_uid(id);
}

[[cpp11::register]]
void cpp_saveGatingSet(cpp11::external_pointer<GatingSet> gs, string path, string backend_opt) {
      CytoFileOption cf_opt;
      bool skip_data = false;
      if(backend_opt == "copy")
        cf_opt = CytoFileOption::copy;
      else if(backend_opt == "move")
        cf_opt = CytoFileOption::move;
      else if(backend_opt == "link")
        cf_opt = CytoFileOption::link;
      else if(backend_opt == "symlink")
        cf_opt = CytoFileOption::symlink;
      else if(backend_opt == "skip")
      {
          cf_opt = CytoFileOption::skip;
          skip_data = true;
      }
      else
        stop("invalid backend_opt option!");
			gs->serialize_pb(path, cf_opt, skip_data);
}

[[cpp11::register]]
cpp11::external_pointer<GatingSet> cpp_loadGatingSet(string path, bool readonly, vector<string> select_samples, bool verbose) {


	return cpp11::external_pointer<GatingSet>(new GatingSet(path, false, readonly, select_samples, verbose));

}

[[cpp11::register]]
cpp11::external_pointer<GatingSet> load_legacy_gs(string pbfile, cpp11::external_pointer<GatingSet> cs) {
		return cpp11::external_pointer<GatingSet>(new GatingSet(pbfile, *cs));

}

[[cpp11::register]]
cpp11::external_pointer<GatingSet> cpp_CloneGatingSet(cpp11::external_pointer<GatingSet> gs, string h5_dir, bool is_copy_data) {



		return cpp11::external_pointer<GatingSet>(new GatingSet(gs->copy(is_copy_data, true, h5_dir)));

}

[[cpp11::register]]
cpp11::external_pointer<GatingSet> cpp_combineGatingSet(cpp11::list gsList,cpp11::list sampleList) {

	cpp11::external_pointer<GatingSet> newGS(new GatingSet());
//	GatingSet newCS;

		for(int i=0;i<gsList.size();i++)
		{
			GatingSet *	gs=getGsPtr((SEXP)gsList[i]);
			cpp11::strings samples(sampleList[i]);
//			const GatingSet & cs = gs->get_cytoset();
			for(auto sn : samples)
			{
//				newCS.add_cytoframe_view(sn, gss.get_cytoframe_view(sn));
				newGS->add_GatingHierarchy(gs->getGatingHierarchy(sn),sn);
			}
//			newGS->set_cytoset(newCS);
		}


		return newGS;

}

/**
 * change sample name
 */
[[cpp11::register]]
void cpp_setSample(cpp11::external_pointer<GatingSet> gs,string oldName, string newName) {
	
		gs->set_sample_uid(oldName,newName);

}

//' check whether cytolib is build with tiledb support
//' @return TRUE or FALSE
//' @export
[[cpp11::register]]
bool is_tiledb_support() {

		return false;

}
[[cpp11::register]]
unsigned short cpp_getLogLevel() {

		return(g_loglevel);

}

[[cpp11::register]]
void cpp_setLogLevel(unsigned short loglevel) {

		g_loglevel = loglevel;

}

[[cpp11::register]]
void cpp_togleErrorFlag(){
	my_throw_on_error = !my_throw_on_error;
}
