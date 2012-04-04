//============================================================================
// Name        : src.cpp
// Author      : Mike jiang
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C, Ansi-style
//============================================================================

#include <iostream>
#include <string>

#include "include/flowJoWorkspace.hpp"
#include "include/GatingSet.hpp"
#include "include/GatingHierarchy.hpp"
#include "include/R_GatingSet.hpp"
using namespace std;

#define MAC 0
#define WIN 1



void test(string xml){
		GatingSet gs(xml,1);//select xml workspace to parse
		unsigned short groupID=1;
		gs.parseWorkspace(groupID);

		GatingHierarchy *gh=gs.getGatingHierarchy(1);
//		cout<<gh->getNodeList()<<endl;
		cout<<gh->getSample()<<endl;
//		gh->drawGraph();
		//construct GatingHierarchy without associate it with sample
	//	GatingHierarchy gh;
	//	gh.gating();
	//	gh.gating();
		//construct GatingSet that holds GatingHierarchy for each individual sample and gate it afterwards

		//

}

/*
 * somehow the Rcpp code can't be run within C++ IDE
 * which makes the debugging in c++ environment impossible
 * now, the debug/test has to be done within R after the Rcpp code compiles
 */
void Rcpp_test(string xml){
//	CharacterVector _xml(1);
//	_xml[0]="ddd";
//	R_openWorkspace(_xml);
//	XPtr<GatingSet> gsPtr();
//	R_parseWorkspace(ptr,IntegerVector(1));
//	R_getSamples(gsPtr);
//	SEXP ghPtr=R_getGatingHierarchyI(gsPtr,IntegerVector(1));

}

int main(void) {

	//read xml file by libxml
	vector<string> fileNames;

	fileNames.push_back("../fjWsExamples/HIPC_trial.xml");
	fileNames.push_back("../fjWsExamples/LyoplateTest1Yale.wsp");
	test(fileNames.at(WIN));
//	Rcpp_test(fileNames.at(WIN));



	return (0);
}
