// #include <cytolib/GatingSet.hpp>
// #include <Rcpp.h>
// using namespace Rcpp;
// using namespace cytolib;
// /*
//  * the routine that deals with the core c++ class and its member functions
//  */
// void getDescendants_gh(GatingHierarchy & gh, VertexID u, VertexID_vec & output){
//   // Rcpp::Rcout << "start: "  << u << std::endl;
//   VertexID_vec children = gh.getChildren(u);
//   // Rcpp::Rcout << "size: "  << children.size() << std::endl;

//   for(VertexID_vec::iterator it= children.begin();it!=children.end();it++)
//   {
//     u=*it;
//     // Rcpp::Rcout << u << endl;
//     output.push_back(u);
//     getDescendants_gh(gh, u, output);
//   }
// }

// /*
//  * the wrapper function that exposes to R
//  */
// //' grab all the descendant nodes for a given node
// //'
// //' This is a faster version of flowIncubator:::getDescendants
// //'
// //' @param gsPtr external pointer that points to the C data structure of GatingSet
// //' @param sampleName sample name
// //' @param node node name
// //' @noRd
// //[[Rcpp::export(".getDescendants")]]
// VertexID_vec getDescendants(Rcpp::XPtr<GatingSet> gsPtr, string sampleName, string node){
//   // Rcpp::Rcout << sampleName << std::endl;
//   GatingHierarchy &gh = *gsPtr->getGatingHierarchy(sampleName);
//   VertexID_vec output;


//   getDescendants_gh(gh, gh.getNodeID(node), output);
//   return(output);

// }

