#include "cytolib/GatingSet.hpp"
#include <Rcpp.h>
#include <RcppParallel.h>
using namespace Rcpp;
using namespace RcppParallel;

//[[Rcpp::plugins(temp)]]
  
  struct getStats : public Worker
  {
    // source 
    Rcpp::XPtr<GatingSet> gs;
    const StringVec sampleNames;
    const StringVec subpopulation;
    const bool isFlowCore;
    const bool isFullPath;
    
    // destination 
    List output;
    Rcpp::CharacterVector sampleVec;
    Rcpp::CharacterVector popVec;
    Rcpp::CharacterVector parentVec;
    Rcpp::IntegerVector countVec;
    Rcpp::IntegerVector parentCountVec;
    
    
    // initialize with source and destination
    getStats(Rcpp::XPtr<GatingSet> gs,const StringVec sampleNames,const StringVec subpopulation, const bool isFlowCore, const bool isFullPath,List output) 
      : gs(gs),sampleNames(sampleNames), subpopulation(subpopulation), isFlowCore(isFlowCore), isFullPath(isFullPath), output(output) {
          sampleVec = output["name"];
          popVec = output["Population"];
          parentVec = output["Parent"];
          countVec = output["Count"];
          parentCountVec = output["ParentCount"];
      
    }
    
    // take the square root of the range of elements requested
    void operator()(std::size_t begin, std::size_t end) {
      unsigned nPop = subpopulation.size();
      
      for(auto i = begin; i < end; i++)
      {
        std::string sn = sampleNames.at(i);
        GatingHierarchy & gh = gs->getGatingHierarchy(sn);
        //we are confident that allNodes is ordered by its nodeIds(ie. vertexID)
        StringVec allNodes = gh.getPopPaths(REGULAR, isFullPath, true);
        for(unsigned j = 0; j < nPop; j++){
          std::string pop = subpopulation.at(j);
          auto counter = i * nPop + j;
          sampleVec(counter) = sn;
          popVec(counter) = pop;
          
          //				get count of this pop
          VertexID u = gh.getNodeID(pop);
          countVec(counter) = gh.getNodeProperty(u).getStats(isFlowCore)["count"];
          
          //				 get parent name
          VertexID pid = gh.getParent(u);
          parentVec(counter) = allNodes.at(pid);
          
          //				get parent count
          parentCountVec(counter) = gh.getNodeProperty(pid).getStats(isFlowCore)["count"];
          
          //increment counter
          counter++;
          
        }
      }
    }
  };
//' grab vectors of pop counts and the parent counts along with their paths and FCS filenames
//'
//' This speeds up the process of getPopStats by putting the loop in c++ and avoiding copying while constructing vectors
//'
//' @param gsPtr external pointer that points to the C data structure of GatingSet
//' @param sampleNames sample names vector
//' @param subpopulation population vector that specify the subset of pops to query
//' @param flowJo logical flag to specify whether flowCore or flowJo counts to return
//' @param isFullPath logical flag to specify whether return the full path or partial path of populations
//' @importFrom RcppParallel RcppParallelLibs
//' @noRd
//[[Rcpp::export(".getPopCounts")]]
Rcpp::List getPopCounts(Rcpp::XPtr<GatingSet> gsPtr, StringVec sampleNames, StringVec subpopulation, bool flowJo, bool isFullPath){
  
  bool isFlowCore = !flowJo;
  
  unsigned nPop = subpopulation.size();
  unsigned nSample = sampleNames.size();
  unsigned nVec = nPop * nSample;
  Rcpp::CharacterVector sampleVec(nVec);
  Rcpp::CharacterVector popVec(nVec);
  Rcpp::CharacterVector parentVec(nVec);
  Rcpp::IntegerVector countVec(nVec);
  Rcpp::IntegerVector parentCountVec(nVec);
  
  auto output = Rcpp::List::create(Rcpp::Named("name", sampleVec)
                              , Rcpp::Named("Population", popVec)
                              , Rcpp::Named("Parent", parentVec)
                              , Rcpp::Named("Count", countVec)
                              , Rcpp::Named("ParentCount", parentCountVec)
                            );
  getStats getStats(gsPtr, sampleNames, subpopulation, isFlowCore, isFullPath, output);
  
  parallelFor(0, nSample, getStats);

  return output;
}


