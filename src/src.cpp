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

		//create gating set object
		GatingSet gs(xml,0);

		//parse a particular sample group
		unsigned short groupID=1;
		gs.parseWorkspace(groupID);

		/*
		 * get sample list from gating set
		 */
		vector<string> samples=gs.getSamples();
		vector<string>::iterator it;
		for(it=samples.begin();it!=samples.end();it++)
			cout<<*it<<endl;

		/*
		 *
		 */
		GatingHierarchy *gh=gs.getGatingHierarchy(1);


		vector<string> nodelist=gh->getNodeList();
//		map<string,VertexID>::iterator it1;
		for(it=nodelist.begin();it!=nodelist.end();it++)
			cout<<*it<<endl;
		cout<<gh->getSample()<<endl;
		cout<<gh->getParent(3)<<endl;
		vector<VertexID> children=gh->getChildren(6);
		vector<VertexID>::iterator it1;
		for(it1=children.begin();it1!=children.end();it1++)
					cout<<*it1<<endl;

		gh->drawGraph();
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
