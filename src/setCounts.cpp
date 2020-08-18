#include <cytolib/GatingSet.hpp>
#include <Rcpp.h>
using namespace cytolib;

using namespace Rcpp;

/*
 * the wrapper function that exposes to R
 */
//' set the event counts for a given node
//'
//'
//' @param gsPtr external pointer that points to the C data structure of GatingSet
//' @param sampleName sample name
//' @param node node name
//' @param count the event count to be stored
//' @noRd
//[[Rcpp::export(".set.count.xml")]]
void setCounts(Rcpp::XPtr<GatingSet> gsPtr, string sampleName, string node, int count){
  // Rcpp::Rcout << sampleName << std::endl;
  GatingHierarchy & gh = *gsPtr->getGatingHierarchy(sampleName);
  VertexID nodeID = gh.getNodeID(node);
  nodeProperties & np = gh.getNodeProperty(nodeID);
  np.set_stats("Count", count, false);
}

