#include "cytolib/GatingSet.hpp"
#include <cpp11.hpp>
#include <vector>
using namespace cytolib;
  
     
//' grab vectors of pop counts and the parent counts along with their paths and FCS filenames
//'
//' This speeds up the process of getPopStats by putting the loop in c++ and avoiding copying while constructing vectors
//'
//' @param gsPtr external pointer that points to the C data structure of GatingSet
//' @param freq logical flag indicating whether counts should be converted to frequencies
//' @param sampleNames sample names vector
//' @param subpopulation population vector that specify the subset of pops to query
//' @param flowJo logical flag to specify whether flowCore or flowJo counts to return
//' @param isFullPath logical flag to specify whether return the full path or partial path of populations
//' @noRd
[[cpp11::register]]
cpp11::list getPopCounts_cpp(cpp11::external_pointer<GatingSet> gs, bool freq, StringVec subpopulation, bool flowJo, bool isFullPath){
  
  bool isFlowCore = !flowJo;
  StringVec sampleNames = gs->get_sample_uids();
  unsigned nPop = subpopulation.size();
  unsigned nSample = sampleNames.size();
  unsigned nVec = nPop * nSample;
  cpp11::writable::strings sampleVec(nVec);
  cpp11::writable::strings popVec(nVec);
  cpp11::writable::strings parentVec(nVec);
  cpp11::writable::doubles freqVec(nVec);
  cpp11::writable::doubles parentFreqVec(nVec);
  cpp11::writable::integers countVec(nVec);
  cpp11::writable::integers parentCountVec(nVec);
  
  


    for(int i = 0; i < nSample; i++)
    {
        std::string sn = sampleNames.at(i);
        GatingHierarchy & gh = *gs->getGatingHierarchy(sn);
        // hash for pop vs id since it is expensive to compute for large tree
        std::unordered_map<std::string, cytolib::VertexID> pop_vs_id;
        auto all_pops = gh.getNodePaths(0, true, true);
        for (size_t j = 0; j < all_pops.size(); j++) {
          pop_vs_id[all_pops[j]] = j;
        }
        unsigned rootCount = gh.getNodeProperty(gh.getNodeID("root")).getStats(isFlowCore)["count"];
        for(int j = 0; j < nPop; j++){
            std::string pop = subpopulation.at(j);
            int counter = i * nPop + j;
            sampleVec[counter] = sn;
            popVec[counter] = pop;
            
            //get count or frequency of this pop
            auto it = pop_vs_id.find(pop);
          if (it == pop_vs_id.end())
            throw std::domain_error(pop + " not found in gating tree of " + sn);
          cytolib::VertexID u = it->second;

            unsigned thisCount = gh.getNodeProperty(u).getStats(isFlowCore)["count"];
            if(freq)
                if(rootCount)
                    freqVec[counter] = double(thisCount) / double(rootCount);
                else
                    freqVec[counter] = 0.0;
            else
                countVec[counter] = thisCount;
                
            //get parent name
            VertexID pid = gh.getParent(u);
            parentVec[counter] = gh.getNodePath(pid, isFullPath);
            
            //get parent count or frequency
            unsigned parentCount = gh.getNodeProperty(pid).getStats(isFlowCore)["count"];
            if(freq)
                if(rootCount)
                    parentFreqVec[counter] = double(parentCount) / double(rootCount);
                else
                    parentFreqVec[counter] = 0.0;
            else
                parentCountVec[counter] = parentCount;
            
            //increment counter
            counter++;
            
        }
    }
    

  cpp11::writable::list res({cpp11::named_arg("name")= sampleVec
                              , cpp11::named_arg("Population")= popVec
                              , cpp11::named_arg("Parent")= parentVec
  });
  if(freq)
  {
    res.push_back(cpp11::named_arg("Frequency")= freqVec);
    res.push_back(cpp11::named_arg("ParentFrequency")= parentFreqVec);
  }
  else
  {
    res.push_back(cpp11::named_arg("Count")= countVec);
    res.push_back(cpp11::named_arg("ParentCount")= parentCountVec);

  }
  return res;
}
