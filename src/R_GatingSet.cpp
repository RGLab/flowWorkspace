/*
 * R_GatingSet.cpp
 *
 *these are R APIs
 *
 *  Created on: Mar 30, 2012
 *      Author: wjiang2
 */
//#include <Rinternals.h>
//#include <Rdefines.h>
//#include <Rmath.h>
//#include "GatingSet.hpp"

#include <Rcpp.h>
//#include <cmath>

RcppExport SEXP newRcppVectorExample(SEXP vector) {
BEGIN_RCPP

    Rcpp::NumericVector orig(vector);                   // keep a copy (as the classic version does)
    Rcpp::NumericVector vec(orig.size());               // create a target vector of the same size

    // we could query size via
    //   int n = vec.size();
    // and loop over the vector, but using the STL is so much nicer
    // so we use a STL transform() algorithm on each element
    std::transform(orig.begin(), orig.end(), vec.begin(), ::sqrt);

    return Rcpp::List::create(Rcpp::Named( "result" ) = vec,
                              Rcpp::Named( "original" ) = orig) ;

END_RCPP
}

//static void cooked_goose(SEXP foo)
//{
//    if (TYPEOF(foo) != EXTPTRSXP)
//        error("argument not external pointer");
//    GatingSet *x = (GatingSet *) R_ExternalPtrAddr(foo);
////    int blather = x[0];
//    Free(x);
//    cout<<"finalizer ran!"<<endl;
//}
//
//RcppExport SEXP R_openWorkspace(SEXP fileName) {
//BEGIN_RCPP
//
//	Rcpp::CharacterVector sFileName(fileName);
////	fileName="fjWsExamples/LyoplateTest1Yale.wsp";
//	GatingSet gs;
//	gs.openWorkspace(sFileName.at);
//
//	SEXP ans;
//    PROTECT(ans = R_MakeExternalPtr(&gs, R_NilValue, R_NilValue));
//    R_RegisterCFinalizer(ans, cooked_goose);
//    UNPROTECT(1);
//    return ans;
//END_RCPP
//}
//
//SEXP R_getGatingHierarchy(SEXP gs)
//{
//    if (TYPEOF(gs) != EXTPTRSXP)
//        error("argument not external pointer");
//
//    GatingSet *x = (GatingSet *) R_ExternalPtrAddr(gs);
//
//    GatingHierarchy *gh=x->getGatingHierarchy(1);
////    gh->drawGraph();
//
//    SEXP bar;
//    PROTECT(bar = allocVector(REALSXP, 1));
//
//    UNPROTECT(1);
//    return bar;
//}

