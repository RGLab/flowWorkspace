#ifndef LISTTOCOMP_H_
#define LISTTOCOMP_H_
#include <cpp11.hpp>
#include <vector>

#include <cytolib/GatingSet.hpp>
using namespace cytolib;

inline compensation mat_to_comp(cpp11::doubles_matrix<> rmat)
{
        vector<string> chnls = cpp11::as_cpp<vector<string>>(cpp11::list(rmat.attr("dimnames"))[1]);
        
        compensation comp = compensation(rmatrix_to_arma(rmat), chnls);
        comp.cid = "1";
        return comp;
}
inline unordered_map<string, compensation> list_to_comps(cpp11::list comps){
        unordered_map<string, compensation> res;

        if(!Rf_isNull(comps.names()))
        {
                vector<string> names = cpp11::as_cpp<vector<string>>(comps.names());
                for(auto sn : names)
                {
                        if(sn.size()>0)
                        {
                                cpp11::doubles_matrix<> rmat(comps[sn]);
                                res[sn] = mat_to_comp(rmat);
                        }
                }
        }
        return res;
}
#endif
