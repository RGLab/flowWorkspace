/*
 * R_GatingSet.cpp
 *
 *these are R APIs
 *
 *  Created on: Mar 30, 2012
 *      Author: wjiang2
 */

#include "flowWorkspace.h"
#include <Rcpp.h>
using namespace Rcpp;
GatingSet * getGsPtr(SEXP _gsPtr){

        if(R_ExternalPtrAddr(_gsPtr)==0)
                        throw(domain_error("Null GatingSet pointer!"));
        XPtr<GatingSet>gs(_gsPtr);

        return gs;
}


//[[Rcpp::export]]
void gs_transform_data(XPtr<GatingSet> gsPtr) {
	for(auto sn : gsPtr->get_sample_uids())
	{
		GatingHierarchyPtr gh = gsPtr->getGatingHierarchy(sn);
		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
			Rcout<<"transforming: "<<sn<<endl;
		CytoFramePtr cf = gh->get_cytoframe_view().get_cytoframe_ptr();

		MemCytoFrame fr(*cf);

		gh->transform_data(fr);
		cf->set_data(fr.get_data());
		cf->set_params(fr.get_params());
		cf->set_keywords(fr.get_keywords());
	}
}
//[[Rcpp::export]]
void cpp_gating(XPtr<GatingSet> gsPtr, vector<string> nodes, bool alwaysLoadData, bool verbose, bool leafbool) {
  if(nodes[0] == "root")
    alwaysLoadData = true; //skip the checking to save time when start from root

  VertexID_vec nodeIDs(nodes.size());
  GatingSet cs = gsPtr->get_cytoset();
  for(const string & sid : gsPtr->get_sample_uids())
  {
    if(verbose)
      Rcout << "gating " << sid << endl;
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
          boolGate * g = dynamic_cast<boolGate *>(gh->getNodeProperty(i).getGate());
          for(auto j : g->getBoolSpec())
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
    unique_ptr<MemCytoFrame> fr (new MemCytoFrame());
    if(isloadData)
      fr.reset(new MemCytoFrame(*cs.get_cytoframe_view(sid).get_cytoframe_ptr()));
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

//[[Rcpp::export]]
XPtr<GatingSet> subset_gs_by_sample(XPtr<GatingSet> gsPtr, vector<string> samples) {

  return XPtr<GatingSet>(new GatingSet(gsPtr->sub_samples(samples)));
}

//[[Rcpp::export]]
XPtr<GatingSet> get_cytoset(XPtr<GatingSet> gsPtr) {

  return XPtr<GatingSet>(new GatingSet(gsPtr->get_cytoset()));
}

//[[Rcpp::export]]
XPtr<GatingSet> get_cytoset_from_node(XPtr<GatingSet> gsPtr, string node) {

  return XPtr<GatingSet>(new GatingSet(gsPtr->get_cytoset(node)));
}

//[[Rcpp::export]]
void set_cytoset(XPtr<GatingSet> gsPtr, XPtr<GatingSet> cs) {

  gsPtr->set_cytoset(*cs);
}
//[[Rcpp::export(name=".cpp_getSamples")]]
StringVec get_sample_uids(XPtr<GatingSet> gsPtr) {

	return gsPtr->get_sample_uids();
}

/*
 * constructing GatingSet from existing gating hierarchy and new data
 */
//[[Rcpp::export(name=".cpp_NewGatingSet")]]
XPtr<GatingSet> NewGatingSet(XPtr<GatingSet> gsPtr
               ,string src_sample_uid
			   , XPtr<GatingSet> cs
			   , bool execute)
  {

		GatingHierarchy & gh=*gsPtr->getGatingHierarchy(src_sample_uid);

		/*
		 * used gh as the template to clone multiple ghs in the new gs
		 */
		GatingSet * newGS=new GatingSet(gh, *cs, execute);

		/*
		 * using default finalizer to delete gs,which is triggered by gc() when
		 * xptr is out of scope
		 */

		return XPtr<GatingSet>(newGS);

}

//[[Rcpp::export]]
string get_gatingset_id(XPtr<GatingSet> gsPtr) {

	return gsPtr->get_uid();
}
//[[Rcpp::export]]
void set_gatingset_id(XPtr<GatingSet> gsPtr, string id) {

	 gsPtr->set_uid(id);
}

/*
 * save/load GatingSet
 */
//[[Rcpp::export(name=".cpp_saveGatingSet")]]
void save_gatingset(XPtr<GatingSet> gs, string path, string cdf) {
      H5Option h5_opt;
      if(cdf == "copy")
        h5_opt = H5Option::copy;
      else if(cdf == "move")
        h5_opt = H5Option::move;
      else if(cdf == "link")
        h5_opt = H5Option::link;
      else if(cdf == "symlink")
        h5_opt = H5Option::symlink;
      else if(cdf == "skip")
        h5_opt = H5Option::skip;
      else
        stop("invalid cdf option!");
			gs->serialize_pb(path, h5_opt);
}

//[[Rcpp::export(name=".cpp_loadGatingSet")]]
XPtr<GatingSet> load_gatingset(string path, unsigned int h5_acc_flags) {
		return XPtr<GatingSet>(new GatingSet(path, false, h5_acc_flags));

}

//[[Rcpp::export]]
XPtr<GatingSet> load_legacy_gs(string pbfile, XPtr<GatingSet> cs) {
		return XPtr<GatingSet>(new GatingSet(pbfile, *cs));

}

//[[Rcpp::export(name=".cpp_CloneGatingSet")]]
XPtr<GatingSet> CloneGatingSet(XPtr<GatingSet> gs, string h5_dir, bool is_copy_data) {



		return XPtr<GatingSet>(new GatingSet(gs->copy(is_copy_data, true, h5_dir)));

}

//[[Rcpp::export(name=".cpp_combineGatingSet")]]
XPtr<GatingSet> combineGatingSet(Rcpp::List gsList,Rcpp::List sampleList) {

	XPtr<GatingSet> newGS(new GatingSet());
//	GatingSet newCS;

		for(int i=0;i<gsList.size();i++)
		{
			GatingSet *	gs=getGsPtr((SEXP)gsList[i]);
			StringVec samples=as<StringVec>(sampleList[i]);
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
//[[Rcpp::export(name=".cpp_setSample")]]
void set_sample_uid(XPtr<GatingSet> gs,string oldName, string newName) {
	
		gs->set_sample_uid(oldName,newName);

}


//[[Rcpp::export(name=".cpp_getLogLevel")]]
unsigned short getLogLevel() {

		return(g_loglevel);

}

//[[Rcpp::export(name=".cpp_setLogLevel")]]
void setLogLevel(unsigned short loglevel) {

		g_loglevel = loglevel;

}

//[[Rcpp::export(name=".cpp_togleErrorFlag")]]
void toggleErrorFlag(){
	my_throw_on_error = !my_throw_on_error;
}
