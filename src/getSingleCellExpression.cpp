#include "cytolib/GatingSet.hpp"
#include <cpp11.hpp>

#include <stdexcept>
using namespace std;
using namespace cytolib;


/**
 * mask and subset the input data matrix based on indexList
 */
cpp11::writable::doubles_matrix<> maskMatrix(const vector<BoolVec> & indexList, cpp11::doubles_matrix<> data, bool threshold){
  
  // or operation among these indices
  int nMarkers = indexList.size();
  
  BoolVec ind = indexList.front();
  if(nMarkers > 1)
    for(int i = 1; i < nMarkers; ++i)
      transform (ind.begin(), ind.end(), indexList.at(i).begin(), ind.begin(),logical_or<bool>());
  
  // grab and mask those rows
  int lgl_n = count(ind.begin(),ind.end(),true);
  int n = data.nrow();
  int k = data.ncol();
  if(k!=  nMarkers && threshold)
    cpp11::stop("when 'threshold = TRUE' , the number of markers to be masked must be consistent with the columns of the input data matrix!");
  cpp11::writable::doubles_matrix<> output(lgl_n, k);
  
  int counter = 0;
  for (int i=0; i < n; ++i) {
    if (ind.at(i)) {
      for (int j=0; j < k; ++j) {
        if(threshold){//if threshold is true, then only record the intensity that is above the gate threshold
          if (indexList.at(j).at(i))
            output(counter, j) = data(i, j);
        }
        else
        {
          output(counter, j) = data(i, j);
        }
        
      }
      ++counter;
    }
  }
  return output;
}

/**
 * assume the channel, markers are in consistent order among markers_pops, data, markers
 * since we use numeric index to mask the columns in data matrix
//  */
// , cpp11::list markers_pops //each marker may be used by several pops
//                                         , cpp11::doubles_matrix<> data //ncol = length(markers)
//                                         , vector<string> markers //used for output mat colnames,may have other markers
[[cpp11::register]]
cpp11::writable::doubles_matrix<> cpp_getSingleCellExpressionByGate(cpp11::external_pointer<GatingSet> gs,string sampleName
                                        , cpp11::list markers_pops 
                                        , cpp11::doubles_matrix<> data
                                        , cpp11::strings markers 
                                        , bool threshold) {
  
  GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);
  
  //get indices for each marker
  int nMarkers = markers_pops.size();
  vector<BoolVec> indexList(nMarkers);
  for(int i =0; i < nMarkers; i++){
    cpp11::strings pops(markers_pops.at(i));
    // merge the indices from multiple nodes for the same marker
    for(int j = 0; j < pops.size(); ++j){
      VertexID u = gh.getNodeID(pops[j]);
      BoolVec ind = gh.getNodeProperty(u).getIndices();
      if(j == 0)
        indexList.at(i) = ind;  
      else{
        transform(ind.begin(), ind.end(), indexList.at(i).begin(), indexList.at(i).begin(), logical_or<bool>());  
      }
    }
  }
  
  cpp11::writable::doubles_matrix<> output = maskMatrix(indexList, data, threshold);
  
  //attach marker names to data
   cpp11::writable::list_of<cpp11::writable::strings> dimnms(
      {R_NilValue, markers});
  Rf_setAttrib(cpp11::as_sexp(output), cpp11::as_sexp({"dimnames"}), cpp11::as_sexp(dimnms));

  
  return output;
}


[[cpp11::register]]
cpp11::writable::doubles_matrix<> cpp_getSingleCellExpression(cpp11::external_pointer<GatingSet> gs,string sampleName
                                        , vector<string> pops, cpp11::doubles_matrix<> data
                                        , cpp11::strings markers, bool threshold) {
  
  //get indices from each node
  GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);
  
  int nNodes = pops.size();
  vector<BoolVec> indexList(nNodes);
  for(int i =0; i < nNodes; i++){
    string pop = pops.at(i);
    VertexID u = gh.getNodeID(pop);
    indexList.at(i)=gh.getNodeProperty(u).getIndices();
  }
  
  cpp11::writable::doubles_matrix<> output = maskMatrix(indexList, data, threshold);
  
  cpp11::writable::list_of<cpp11::writable::strings> dimnms(
      {R_NilValue, markers});
  
  Rf_setAttrib(cpp11::as_sexp(output), cpp11::as_sexp({"dimnames"}), cpp11::as_sexp(dimnms));

  return output;
}

