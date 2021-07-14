// #include "cytolib/GatingSet.hpp"
// #include <Rcpp.h>
// #include <RcppParallel.h>
// using namespace Rcpp;
// using namespace RcppParallel;
// using namespace cytolib;
// //[[Rcpp::plugins(temp)]]
  
//   struct getStats : public Worker
//   {
//     // source 
//     cpp11::external_pointer<GatingSet> gs;
//     const bool freq;
//     const StringVec sampleNames;
//     const StringVec subpopulation;
//     const bool isFlowCore;
//     const bool isFullPath;
    
//     // destination 
//     List output;
//     Rcpp::CharacterVector sampleVec;
//     Rcpp::CharacterVector popVec;
//     Rcpp::CharacterVector parentVec;
//     Rcpp::IntegerVector countVec;
//     Rcpp::DoubleVector freqVec;
//     Rcpp::IntegerVector parentCountVec;
//     Rcpp::DoubleVector parentFreqVec;
    
//     // initialize with source and destination
//     getStats(cpp11::external_pointer<GatingSet> gs, const bool freq, const StringVec sampleNames,const StringVec subpopulation, const bool isFlowCore, const bool isFullPath,List output) 
//       : gs(gs), freq(freq), sampleNames(sampleNames), subpopulation(subpopulation), isFlowCore(isFlowCore), isFullPath(isFullPath), output(output) {
//           sampleVec = output["name"];
//           popVec = output["Population"];
//           parentVec = output["Parent"];
//           if(freq){
//             freqVec = output["Frequency"];
//             parentFreqVec = output["ParentFrequency"];
//           }else{
//             countVec = output["Count"];
//             parentCountVec = output["ParentCount"];
//           }

//     }
    
//     // take the square root of the range of elements requested
//     void operator()(std::size_t begin, std::size_t end) {
//       unsigned nPop = subpopulation.size();
      
//       for(auto i = begin; i < end; i++)
//       {
//         std::string sn = sampleNames.at(i);
//         GatingHierarchy & gh = *gs->getGatingHierarchy(sn);
//         unsigned rootCount = gh.getNodeProperty(gh.getNodeID("root")).getStats(isFlowCore)["count"];
//         for(unsigned j = 0; j < nPop; j++){
//           std::string pop = subpopulation.at(j);
//           auto counter = i * nPop + j;
//           sampleVec(counter) = sn;
//           popVec(counter) = pop;
          
//           //get count or frequency of this pop
//           VertexID u = gh.getNodeID(pop);
//           unsigned thisCount = gh.getNodeProperty(u).getStats(isFlowCore)["count"];
//           if(freq)
//             if(rootCount)
//               freqVec(counter) = double(thisCount) / double(rootCount);
//             else
//               freqVec(counter) = 0.0;
//           else
//             countVec(counter) = thisCount;
              
//           //get parent name
//           VertexID pid = gh.getParent(u);
//           parentVec(counter) = gh.getNodePath(pid, isFullPath);
          
//           //get parent count or frequency
//           unsigned parentCount = gh.getNodeProperty(pid).getStats(isFlowCore)["count"];
//           if(freq)
//             if(rootCount)
//               parentFreqVec(counter) = double(parentCount) / double(rootCount);
//             else
//               parentFreqVec(counter) = 0.0;
//           else
//             parentCountVec(counter) = parentCount;
          
//           //increment counter
//           counter++;
          
//         }
//       }
//     }
//   };
// //' grab vectors of pop counts and the parent counts along with their paths and FCS filenames
// //'
// //' This speeds up the process of getPopStats by putting the loop in c++ and avoiding copying while constructing vectors
// //'
// //' @param gsPtr external pointer that points to the C data structure of GatingSet
// //' @param freq logical flag indicating whether counts should be converted to frequencies
// //' @param sampleNames sample names vector
// //' @param subpopulation population vector that specify the subset of pops to query
// //' @param flowJo logical flag to specify whether flowCore or flowJo counts to return
// //' @param isFullPath logical flag to specify whether return the full path or partial path of populations
// //' @importFrom RcppParallel RcppParallelLibs
// //' @noRd
// //[[Rcpp::export(".getPopCounts")]]
// Rcpp::List getPopCounts(cpp11::external_pointer<GatingSet> gsPtr, bool freq, StringVec subpopulation, bool flowJo, bool isFullPath){
  
//   bool isFlowCore = !flowJo;
//   StringVec sampleNames = gsPtr->get_sample_uids();
//   unsigned nPop = subpopulation.size();
//   unsigned nSample = sampleNames.size();
//   unsigned nVec = nPop * nSample;
//   Rcpp::CharacterVector sampleVec(nVec);
//   Rcpp::CharacterVector popVec(nVec);
//   Rcpp::CharacterVector parentVec(nVec);
  
//   auto output = Rcpp::List::create(Rcpp::Named("name", sampleVec)
//                               , Rcpp::Named("Population", popVec)
//                               , Rcpp::Named("Parent", parentVec)
//                               , Rcpp::Named(freq ? "Frequency" : "Count", freq ? Rcpp::DoubleVector(nVec) : Rcpp::IntegerVector(nVec))
//                               , Rcpp::Named(freq ? "ParentFrequency" : "ParentCount", freq ? Rcpp::DoubleVector(nVec) : Rcpp::IntegerVector(nVec))
//                             );
//   getStats getStats(gsPtr, freq, sampleNames, subpopulation, isFlowCore, isFullPath, output);
  
//   parallelFor(0, nSample, getStats);

//   return output;
// }


