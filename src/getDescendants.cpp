#include <cytolib/GatingSet.hpp>
#include <cpp11.hpp>
using namespace cytolib;
/*
 * the routine that deals with the core c++ class and its member functions
 */
void getDescendants_gh(GatingHierarchy & gh, VertexID u, VertexID_vec & output){
  VertexID_vec children = gh.getChildren(u);

  for(VertexID_vec::iterator it= children.begin();it!=children.end();it++)
  {
    u=*it;
    output.push_back(u);
    getDescendants_gh(gh, u, output);
  }
}

/*
 * the wrapper function that exposes to R
 */
//' grab all the descendant nodes for a given node
//'
//' This is a faster version of flowIncubator:::getDescendants
//'
//' @param gsPtr external pointer that points to the C data structure of GatingSet
//' @param sampleName sample name
//' @param node node name
//' @noRd
[[cpp11::register]]
VertexID_vec getDescendants_cpp(cpp11::external_pointer<GatingSet> gsPtr, string sampleName, string node){
  GatingHierarchy &gh = *gsPtr->getGatingHierarchy(sampleName);
  VertexID_vec output;


  getDescendants_gh(gh, gh.getNodeID(node), output);
  return(output);

}

