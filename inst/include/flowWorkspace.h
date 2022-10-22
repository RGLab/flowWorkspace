
#ifndef __flowWorkspace_h__
#define __flowWorkspace_h__

#include "cytolib/GatingSet.hpp"
#include "flowWorkspace/pairVectorCpp11Convert.h"
#include "flowWorkspace/list_to_comp.h"
#include "flowWorkspace/convert_to_str_idx.h"
#include "flowWorkspace/convert_trans.h"

using namespace cytolib;
//header included somewhere(not sure where exactly) defines FALSE,which interferes RcppExports.cpp
#ifdef FALSE
  #undef FALSE
#endif

#endif // __flowWorkspace_h__
