#include <cytolib/GatingSet.hpp>
#include <cpp11.hpp>
using namespace cytolib;


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
[[cpp11::register]]
void setCounts_cpp(cpp11::external_pointer<GatingSet> gsPtr, string sampleName, string node, int count){
  GatingHierarchy & gh = *gsPtr->getGatingHierarchy(sampleName);
  VertexID nodeID = gh.getNodeID(node);
  nodeProperties & np = gh.getNodeProperty(nodeID);
  POPSTATS fjStats;
  fjStats["count"]= count;
  np.setStats(fjStats, false);
}

