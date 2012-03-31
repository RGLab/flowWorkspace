/*
 * R_GatingSet.cpp
 *
 *these are R APIs
 *
 *  Created on: Mar 30, 2012
 *      Author: wjiang2
 */
#include <Rinternals.h>
#include <Rdefines.h>
#include <Rmath.h>
#include "GatingSet.hpp"

static void cooked_goose(SEXP foo)
{
    if (TYPEOF(foo) != EXTPTRSXP)
        error("argument not external pointer");
    GatingSet *x = (GatingSet *) R_ExternalPtrAddr(foo);
    int blather = x[0];
    Free(x);
    if (blather)
        printf("finalizer ran\n");
}


SEXP R_openWorkspace(SEXP fileNames)
{


	fileNames="fjWsExamples/LyoplateTest1Yale.wsp";
	GatingSet gs;
	gs.openWorkspace(fileNames);

	SEXP ans;
    PROTECT(ans = R_MakeExternalPtr(&gs, R_NilValue, R_NilValue));
    R_RegisterCFinalizer(ans, cooked_goose);
    UNPROTECT(1);
    return ans;
}

SEXP R_getGatingHierarchy(SEXP gs)
{
    if (TYPEOF(gs) != EXTPTRSXP)
        error("argument not external pointer");

    GatingSet *x = (GatingSet *) R_ExternalPtrAddr(gs);

    GatingHierarchy *gh=&x->getGatingHierarchy(1);
//    gh->drawGraph();

    SEXP bar;
    PROTECT(bar = allocVector(REALSXP, n));
    for (int i = 0; i < n; ++i)
        REAL(bar)[i] = x[i + 2];
    UNPROTECT(1);
    return bar;
}

