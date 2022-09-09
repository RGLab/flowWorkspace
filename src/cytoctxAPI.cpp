#include <flowWorkspace/pairVectorRcppWrap.h>
#include <cytolib/CytoVFS.hpp>
using namespace Rcpp;
using namespace cytolib;

// [[Rcpp::export]]
XPtr<CytoCtx> new_cytoctx(List cred)
{
	int nthreads = 1;
	if(cred.containsElementNamed("num_threads"))
		nthreads = cred["num_threads"];
    return XPtr<CytoCtx>(new CytoCtx(as<string>(cred["AWS_ACCESS_KEY_ID"])
									 , as<string>(cred["AWS_SECRET_ACCESS_KEY"])
									 ,  as<string>(cred["AWS_REGION"])
									 , nthreads
									 )
    						);
}

// [[Rcpp::export]]
List read_cytoctx(XPtr<CytoCtx> ctx)
{
	auto res = ctx->get_config();

	return List::create(Named("AWS_ACCESS_KEY_ID",res["access_key_id"])
				 ,Named("AWS_SECRET_ACCESS_KEY", res["access_key"])
				 ,Named("AWS_REGION", res["region"])
				 , Named("num_threads", res["num_threads"])
				 );
}
