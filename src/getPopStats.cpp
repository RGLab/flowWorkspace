/*
 * getPopStats.cpp
 *
 * faster version of getPopStats from a GatingSet
 *
 *  Created on: Aug 18, 2014
 *      Author: wjiang2
 */
#include "include/R_GatingSet.hpp"//need to be manually added to RcppExports.CPP as well
//[[Rcpp::plugins(temp)]]
//[[Rcpp::export]]
Rcpp::List getPopCounts(Rcpp::XPtr<GatingSet> gsPtr, StringVec sampleNames, StringVec subpopulation, bool flowJo, bool isFullPath){

	bool isFlowCore = !flowJo;

	unsigned nPop = subpopulation.size();
	unsigned nSample = sampleNames.size();
	unsigned nVec = nPop * nSample;
	Rcpp::CharacterVector sampleVec(nVec);
	Rcpp::CharacterVector popVec(nVec);
	Rcpp::CharacterVector parentVec(nVec);
	Rcpp::IntegerVector countVec(nVec);
	Rcpp::IntegerVector parentCountVec(nVec);

	StringVec allNodes = gsPtr->getGatingHierarchy(0)->getPopPaths(REGULAR, isFullPath, true);

	unsigned counter = 0;
	for(unsigned i = 0; i < nSample; i++){
		std::string sn = sampleNames.at(i);
		GatingHierarchy * gh = gsPtr->getGatingHierarchy(sn);
		for(unsigned j = 0; j < nPop; j++){
			 	std::string pop = subpopulation.at(j);
				sampleVec(counter) = sn;
				popVec(counter) = pop;

//				get count of this pop
				VertexID u = gh->getNodeID(pop);
				countVec(counter) = gh->getNodeProperty(u).getStats(isFlowCore)["count"];

//				 get parent name
				VertexID pid = gh->getParent(u);
				parentVec(counter) = allNodes.at(pid);

//				get parent count
				parentCountVec(counter) = gh->getNodeProperty(pid).getStats(isFlowCore)["count"];

				//increment counter
				counter++;

		}
	}



	return Rcpp::List::create(Rcpp::Named("name", sampleVec)
							, Rcpp::Named("Population", popVec)
							, Rcpp::Named("Parent", parentVec)
							, Rcpp::Named("Count", countVec)
							, Rcpp::Named("ParentCount", parentCountVec)
							);
}



