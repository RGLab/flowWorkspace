
#ifndef __flowWorkspace_h__
#define __flowWorkspace_h__

#include "cytolib/GatingSet.hpp"
using namespace cytolib;
#include <Rcpp.h>
GatingSet * getGsPtr(SEXP _gsPtr);
#endif // __flowWorkspace_h__
