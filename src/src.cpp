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
		cout<<endl<<"parseWorkspace for Group:"<<groupID<<endl;
		gs.parseWorkspace(groupID);

		/*
		 * get sample list from gating set
		 */
		cout<<endl<<"getsamples from gating set"<<endl;

		vector<string> samples=gs.getSamples();
		for(vector<string>::iterator it=samples.begin();it!=samples.end();it++)
			cout<<*it<<endl;

		GatingHierarchy *gh=gs.getGatingHierarchy(1);

		/*
		 * getNodes by the T order
		 */

		cout<<endl<<"tsorted node list"<<endl;
		VertexID_vec vertices=gh->getVertices(true);
		for(VertexID_vec::iterator it=vertices.begin();it!=vertices.end();it++)
		{
			populationNode node=gh->vertexIDToNode(*it);
			cout<<*it<<"."<<node.getName()<<endl;
		}

		/*
		 * getNodes by vertices ID order
		 * and get stats from each node
		 */

		cout<<endl<<"node list in regular order"<<endl;
		vertices=gh->getVertices(false);
		for(VertexID_vec::iterator it=vertices.begin();it!=vertices.end();it++)
		{
			populationNode node=gh->vertexIDToNode(*it);
			cout<<*it<<"."<<node.getName()<<":";
			cout<<node.getStats(false)["count"]<<endl;
		}
		/*
		 * getSample from gating hierarchy
		 */
		cout<<endl<<"get sample name from gating hierarchy"<<endl;
		cout<<gh->getSample()<<endl;
		/*
		 * get children and parent node index
		 */

		cout<<endl<<"check parent node"<<endl;
		for(int i=-1;i<=11;i++)
		{
			VertexID_vec parent=gh->getParent(i);
			cout<<i<<"<--";
			for(VertexID_vec::iterator it=parent.begin();it!=parent.end();it++)
				cout<<*it<<" ";
			cout<<endl;
		}



		cout<<endl<<"check children node"<<endl;
		for(int i=-1;i<=11;i++)
		{
			VertexID_vec children=gh->getChildren(i);
			cout<<i<<"-->";
			for(VertexID_vec::iterator it=children.begin();it!=children.end();it++)
						cout<<*it<<",";
			cout<<endl;
		}



		/*
		 * plot gating hierarchy tree
		 */
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
