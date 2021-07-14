// #ifndef LISTTOCOMP_H_
// #define LISTTOCOMP_H_
// #include <cpp11.hpp>
// #include <vector>

// #include <cytolib/GatingSet.hpp>
// using namespace cytolib;

// inline compensation mat_to_comp(cpp11::doubles_matrix rmat)
// {
//         vector<string> chnls = cpp11::as_cpp<vector<string>>(rmat.attr("dimnames")[1]);
//         arma::mat mat = as<arma::mat>(rmat);
        
//         compensation comp = compensation(mat, chnls);
//         comp.cid = "1";
//         return comp;
// }
// inline unordered_map<string, compensation> list_to_comps(List comps){
//         unordered_map<string, compensation> res;

//         if(!Rf_isNull(comps.names()))
//         {
//                 vector<string> names = as<vector<string>>(comps.names());
//                 for(auto sn : names)
//                 {
//                         if(sn.size()>0)
//                         {
//                                 cpp11::doubles_matrix rmat = as<cpp11::doubles_matrix>(comps[sn]);
//                                 res[sn] = mat_to_comp(rmat);
//                         }
//                 }
//         }
//         return res;
// }
// #endif
