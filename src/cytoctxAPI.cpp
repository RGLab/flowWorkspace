#include <cytolib/CytoVFS.hpp>
using namespace Rcpp;
using namespace cytolib;

// [[Rcpp::export]]
XPtr<CytoCtx> new_cytoctx(List ctx)
{
	int nthreads = 1;
	if(ctx.containsElementNamed("num_threads"))
		nthreads = cred["num_threads"];
    return XPtr<CytoCtx>(new CytoCtx(as<string>(cred["AWS_ACCESS_KEY_ID"])
									 , as<string>(cred["AWS_SECRET_ACCESS_KEY"])
									 ,  as<string>(cred["AWS_REGION"])
									 , nthreads
									 )
    						);
}
