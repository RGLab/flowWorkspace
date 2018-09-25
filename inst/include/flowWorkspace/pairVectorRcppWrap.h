/*
 * pairVectorRcppWrap.h
 *
 *  Created on: Feb 9, 2015
 *      Author: wjiang2
 */

#ifndef PAIRVECTORRCPPWRAP_H_
#define PAIRVECTORRCPPWRAP_H_


#include <cytolib/GatingSet.hpp>
using namespace cytolib;

#include <RcppArmadillo.h> //include this instead of Rcpp.h so that RcppArmadillo inclusion won't be preceded by Rcpp.h in RcppExport.cpp
#include <RcppCommon.h>

namespace Rcpp {
//wrap for KW_PAIR
	 template <> inline SEXP wrap(const KW_PAIR & kw){
		unsigned nSize = kw.size();
		Rcpp::CharacterVector res(nSize);
		Rcpp::CharacterVector res_names(nSize);
		for(unsigned i = 0; i < nSize; i++){
			pair<string, string> thisKw = kw.at(i);
			res[i] = thisKw.second;
			res_names[i] = thisKw.first;
		}
		res.names() = res_names;
		return res;
	};
	 // as for FCS_READ_PARAM
	template <> inline KW_PAIR as(SEXP sexp) {
		Rcpp::StringVector vec(sexp);
		unsigned n = vec.size();
		vector<string> sample_uids = vec.names();
		if(sample_uids.size()!= n)
			Rcpp::stop("names are not the same length of the vector!");
		KW_PAIR res(n);
		for(unsigned i = 0; i < n; i++)
		{
			res[i].first = sample_uids[i];
			res[i].second = vec[i];
		}

		return res;
	}
	 // as for FCS_READ_PARAM
	template <> inline FCS_READ_PARAM as(SEXP sexp) {
		 Rcpp::List cfg(sexp);

		FCS_READ_PARAM config;
		//validity checks
//		vector<string> arg_names = cfg.names();
//		vector<string> expect_arg_names;

		config.header.ignoreTextOffset = cfg["ignoreTextOffset"];
		config.header.nDataset = cfg["dataset"];
		config.header.isEmptyKeyValue = cfg["emptyValue"];

		config.data.which_lines = as<vector<int>>(cfg["which_lines"]);
		config.data.decades = cfg["decades"];
		config.data.truncate_min_val = cfg["truncate_min_val"];
		config.data.min_limit = cfg["min_limit"];
		config.data.truncate_max_range = cfg["truncate_max_range"];
		config.data.num_threads = cfg["num_threads"];
    SEXP trans_sxp = cfg["transformation"];
		unsigned short trans_type = TYPEOF(trans_sxp);
		string transformation;
		if(trans_type  == STRSXP)
			transformation = as<string>(trans_sxp);
		else if(trans_type == LGLSXP)
		{
		  if(as<bool>(trans_sxp))
		    transformation="linearize";
		  else
		    transformation="none";
		}
		else
		  stop("invalid transformation argument!");
		  
		if(transformation=="linearize")
		  config.data.transform = TransformType::linearize;
		else if(transformation=="none")
		  config.data.transform = TransformType::none;
		else if(transformation=="linearize_with_PnG_scaling")
		  config.data.transform = TransformType::linearize_with_PnG_scaling;
		else if(transformation=="scale")
		  config.data.transform = TransformType::scale;
		else
		  stop("unkown transformation type :" + transformation);

		 return config;
	 }
	}



#endif /* PAIRVECTORRCPPWRAP_H_ */
