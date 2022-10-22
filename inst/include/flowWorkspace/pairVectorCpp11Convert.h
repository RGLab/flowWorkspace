/*
 * pairVectorCpp11Convert.h
 *
 *  Created on: Feb 9, 2015
 *      Author: wjiang2
 */

#ifndef PAIRVECTORCPP11CONVERT_H_
#define PAIRVECTORCPP11CONVERT_H_
#include <cpp11.hpp>
#include <Rmath.h>
#include <cytolib/GatingSet.hpp>
using namespace cytolib;

inline arma::mat rmatrix_to_arma(cpp11::doubles_matrix<> rmat){
	auto n = rmat.nrow();
	auto m = rmat.ncol();
	arma::mat res(n, m);
	// copy matrix.
	for (auto j = 0; j < m; j++) {
		for (auto i = 0; i < n; i++) {
		res(i, j) = rmat(i, j);
		}
	}
	return res;
}

inline cpp11::writable::doubles_matrix<> arma_to_rmatrix(const arma::mat &dat){
	auto n = dat.n_rows;
	auto m = dat.n_cols;
	cpp11::writable::doubles_matrix<> res(n, m);
	// copy matrix.
	for (auto j = 0; j < m; j++) {
		for (auto i = 0; i < n; i++) {
		res(i, j) = dat(i, j);
		}
	}
	return res;
}
inline SEXP kw_to_sexp(const KW_PAIR & kw){
		int nSize = kw.size();
		cpp11::writable::strings res(nSize);
		cpp11::writable::strings res_names(nSize);
		for(int i = 0; i < nSize; i++){
			pair<string, string> thisKw = kw.at(i);
			res[i] = thisKw.second;
			res_names[i] = thisKw.first;
		}
		res.names() = res_names;
		return res;
	}
	 // as for FCS_READ_PARAM
inline 	KW_PAIR sexp_to_kw(SEXP sexp) {
		cpp11::strings vec(sexp);
		int n = vec.size();
		auto sample_uids = cpp11::as_cpp<cpp11::strings>(vec.names());
		if(sample_uids.size()!= n)
			cpp11::stop("names are not the same length of the vector!");
		KW_PAIR res(n);
		for(int i = 0; i < n; i++)
		{
			res[i].first = sample_uids[i];
			res[i].second = vec[i];
		}

		return res;
	}

	 // as for FCS_READ_PARAM
inline 	FCS_READ_PARAM sexp_to_fcs_read_param(SEXP sexp) {
		 cpp11::list cfg(sexp);

		FCS_READ_PARAM config;
		//validity checks
//		vector<string> arg_names = cfg.names();
//		vector<string> expect_arg_names;
    if(cfg["ignoreTextOffset"]!=R_NilValue)
		  config.header.ignoreTextOffset = cpp11::as_cpp<bool>(cfg["ignoreTextOffset"]);
    
    if(cfg["dataset"]!=R_NilValue)
      config.header.nDataset = cpp11::as_cpp<int>(cfg["dataset"]);
    
    if(cfg["emptyValue"]!=R_NilValue)
      config.header.isEmptyKeyValue = cpp11::as_cpp<bool>(cfg["emptyValue"]);
    
    if(cfg["which.lines"]!=R_NilValue)
	{
		vector<double> lines_double = cpp11::as_cpp<vector<double>>(cfg["which.lines"]);//direct cast int64_t is not supported by cpp11
		config.data.which_lines = vector<int64_t>(lines_double.begin(), lines_double.end());
	}	

	if(config.data.which_lines.size()==1)
		config.data.seed = Rf_runif(0, RAND_MAX);//set seed from R
	
	if(cfg["decades"]!=R_NilValue)
		config.data.decades = cpp11::as_cpp<double>(cfg["decades"]);
	
	if(cfg["truncate_min_val"]!=R_NilValue)
		config.data.truncate_min_val = cpp11::as_cpp<bool>(cfg["truncate_min_val"]);
	
	if(cfg["min_limit"]!=R_NilValue)
		config.data.min_limit = cpp11::as_cpp<double>(cfg["min_limit"]);
	
	if(cfg["truncate_max_range"]!=R_NilValue)
		config.data.truncate_max_range = cpp11::as_cpp<bool>(cfg["truncate_max_range"]);
	
	if(cfg["num_threads"]!=R_NilValue)
		config.data.num_threads = cpp11::as_cpp<int>(cfg["num_threads"]);
	
	if(cfg["transformation"]!=R_NilValue)
	{
		SEXP trans_sxp = cfg["transformation"];
		unsigned short trans_type = TYPEOF(trans_sxp);
		string transformation;
		if(trans_type  == STRSXP)
			transformation = cpp11::as_cpp<string>(trans_sxp);
		else if(trans_type == LGLSXP)
		{
			if(cpp11::as_cpp<bool>(trans_sxp))
			transformation="linearize";
			else
			transformation="none";
		}
		else
			cpp11::stop("invalid transformation argument!");
			
		if(transformation=="linearize")
			config.data.transform = TransformType::linearize;
		else if(transformation=="none")
			config.data.transform = TransformType::none;
		else if(transformation=="linearize_with_PnG_scaling")
			config.data.transform = TransformType::linearize_with_PnG_scaling;
		else if(transformation=="scale")
			config.data.transform = TransformType::scale;
		else
			cpp11::stop("unkown transformation type :" + transformation);
	}
		return config;
}
	



#endif /* PAIRVECTORCPP11CONVERT_H_ */
