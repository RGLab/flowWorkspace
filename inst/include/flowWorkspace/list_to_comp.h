#ifndef LISTTOCOMP_H_
#define LISTTOCOMP_H_


#include <cytolib/GatingSet.hpp>
using namespace cytolib;

#include <RcppArmadillo.h> //include this instead of Rcpp.h so that RcppArmadillo inclusion won't be preceded by Rcpp.h in RcppExport.cpp
#include <RcppCommon.h>
using namespace Rcpp;

inline compensation mat_to_comp(NumericMatrix rmat)
{
        vector<string> chnls = as<vector<string>>(colnames(rmat));
        arma::mat mat = as<arma::mat>(rmat);
        compensation comp = compensation(mat, chnls);
        comp.cid = "1";
        return comp;
}
inline unordered_map<string, compensation> list_to_comps(List comps){
        unordered_map<string, compensation> res;

        if(!Rf_isNull(comps.names()))
        {
                vector<string> names = as<vector<string>>(comps.names());
                for(auto sn : names)
                {
                        if(sn.size()>0)
                        {
                                NumericMatrix rmat = as<NumericMatrix>(comps[sn]);
                                res[sn] = mat_to_comp(rmat);
                        }
                }
        }
        return res;
}
#endif
