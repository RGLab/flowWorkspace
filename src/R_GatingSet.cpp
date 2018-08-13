/*
 * R_GatingSet.cpp
 *
 *these are R APIs
 *
 *  Created on: Mar 30, 2012
 *      Author: wjiang2
 */

#include "flowWorkspace.h"

using namespace Rcpp;
using namespace cytolib;

//[[Rcpp::export]]
XPtr<CytoSet> subset_gs_by_sample(XPtr<GatingSet> gsPtr, vector<string> samples) {

  return XPtr<CytoSet>(new CytoSet(gsPtr->get_cytoset().sub_samples(samples)));
}

//[[Rcpp::export]]
XPtr<CytoSet> get_cytoset(XPtr<GatingSet> gsPtr) {

  return XPtr<CytoSet>(new CytoSet(gsPtr->get_cytoset()));
}

//[[Rcpp::export]]
XPtr<CytoSet> get_cytoset_from_node(XPtr<GatingSet> gsPtr, string node) {

  return XPtr<CytoSet>(new CytoSet(gsPtr->get_cytoset(node)));
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
               ,StringVec new_sample_uids)
  {

		GatingHierarchy & gh=*gsPtr->getGatingHierarchy(src_sample_uid);

		/*
		 * used gh as the template to clone multiple ghs in the new gs
		 */
		GatingSet * newGS=new GatingSet(gh,new_sample_uids);

		/*
		 * using default finalizer to delete gs,which is triggered by gc() when
		 * xptr is out of scope
		 */

		return XPtr<GatingSet>(newGS);

}

/*
 * save/load GatingSet
 */
//[[Rcpp::export(name=".cpp_saveGatingSet")]]
void save_gatingset(XPtr<GatingSet> gs, string path, bool overwrite, string cdf) {
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
			gs->serialize_pb(path, overwrite, h5_opt);
}

//[[Rcpp::export(name=".cpp_loadGatingSet")]]
XPtr<GatingSet> load_gatingset(string path) {
		GatingSet * gs=new GatingSet(path);
		return XPtr<GatingSet>(gs);

}

//[[Rcpp::export(name=".cpp_CloneGatingSet")]]
XPtr<GatingSet> CloneGatingSet(XPtr<GatingSet> gs,StringVec new_sample_uids) {



		return XPtr<GatingSet>(new GatingSet(gs->sub_samples(new_sample_uids).copy()));

}

//[[Rcpp::export(name=".cpp_combineGatingSet")]]
XPtr<GatingSet> combineGatingSet(Rcpp::List gsList,Rcpp::List sampleList) {

		GatingSet * newGS=new GatingSet();

		for(int i=0;i<gsList.size();i++)
		{
			GatingSet *	gs=getGsPtr((SEXP)gsList[i]);
			StringVec samples=as<StringVec>(sampleList[i]);
			for(auto sn : samples)
				newGS->add_GatingHierarchy(gs->getGatingHierarchy(sn),sn);
		}


		return XPtr<GatingSet>(newGS);

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
