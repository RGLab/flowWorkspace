#include "cytolib/GatingSet.hpp"
#include <Rcpp.h>

#include <stdexcept>
// #include "include/gate.hpp"
// #include "include/transformation.hpp"
using namespace std;
using namespace Rcpp;
using namespace cytolib;

//[[Rcpp::plugins(temp)]] 

/**
 * mask and subset the input data matrix based on indexList
 */
NumericMatrix maskMatrix(const vector<BoolVec> & indexList, NumericMatrix const & data, bool threshold){
  
  // or operation among these indices
  unsigned nMarkers = indexList.size();
  
  BoolVec ind = indexList.front();
  if(nMarkers > 1)
    for(unsigned i = 1; i < nMarkers; ++i)
      transform (ind.begin(), ind.end(), indexList.at(i).begin(), ind.begin(),logical_or<bool>());
  
  // grab and mask those rows
  int lgl_n = count(ind.begin(),ind.end(),true);
  unsigned n = data.nrow();
  unsigned k = data.ncol();
  if(k!=  nMarkers && threshold)
    stop("when 'threshold = TRUE' , the number of markers to be masked must be consistent with the columns of the input data matrix!");
  NumericMatrix output(lgl_n, k);
  
  int counter = 0;
  for (unsigned i=0; i < n; ++i) {
    if (ind.at(i)) {
      for (unsigned j=0; j < k; ++j) {
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
 */
//[[Rcpp::export(name=".cpp_getSingleCellExpressionByGate")]]
NumericMatrix getSingleCellExpressionByGate(XPtr<GatingSet> gs,string sampleName
                                        , List markers_pops //each marker may be used by several pops
                                        , NumericMatrix data //ncol = length(markers)
                                        , CharacterVector markers //used for output mat colnames,may have other markers
                                        , bool threshold) {
  
  GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);
  
  //get indices for each marker
  unsigned nMarkers = markers_pops.size();
  vector<BoolVec> indexList(nMarkers);
  for(unsigned i =0; i < nMarkers; i++){
    // Rcpp::Rcout << "marker: " << i << endl;
    Rcpp::CharacterVector pops = markers_pops.at(i);
    // merge the indices from multiple nodes for the same marker
    for(unsigned j = 0; j < pops.size(); ++j){
      // Rcpp::Rcout << "pop: " << j << endl;
      string pop = Rcpp::as<std::string>(pops(j));
      VertexID u = gh.getNodeID(pop);
      BoolVec ind = gh.getNodeProperty(u).getIndices();
      if(j == 0)
        indexList.at(i) = ind;  
      else{
        transform(ind.begin(), ind.end(), indexList.at(i).begin(), indexList.at(i).begin(), logical_or<bool>());  
      }
        
    }
    
  }
  
  NumericMatrix output = maskMatrix(indexList, data, threshold);
  
  //attach marker names to data
  Rcpp::List dimnms =  Rcpp::List::create(CharacterVector::create(),markers);
  
  output.attr("dimnames") = dimnms;
  
  return output;
}


//[[Rcpp::export(name=".cpp_getSingleCellExpression")]]
NumericMatrix getSingleCellExpression(XPtr<GatingSet> gs,string sampleName
                                        , vector<string> pops, NumericMatrix data
                                        , CharacterVector markers, bool threshold) {
  
  //get indices from each node
  GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);
  
  unsigned nNodes = pops.size();
  vector<BoolVec> indexList(nNodes);
  for(unsigned i =0; i < nNodes; i++){
    string pop = pops.at(i);
    VertexID u = gh.getNodeID(pop);
    indexList.at(i)=gh.getNodeProperty(u).getIndices();
  }
  
  NumericMatrix output = maskMatrix(indexList, data, threshold);
  
  
  
  Rcpp::List dimnms =  Rcpp::List::create(CharacterVector::create(),markers);
  
  output.attr("dimnames") = dimnms;
  
  return output;
}

