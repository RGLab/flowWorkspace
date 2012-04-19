//============================================================================
// Name        : src.cpp
// Author      : Mike jiang
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C, Ansi-style
//============================================================================

#include <iostream>
#include <fstream>
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
		GatingSet gs(xml,2);

		//parse a particular sample group
		unsigned short groupID=0;
		cout<<endl<<"parseWorkspace for Group:"<<groupID<<endl;
		gs.parseWorkspace(groupID,true);

		//parse a set of sampleIDs
//		vector<string> sampleIDs;
//		sampleIDs.push_back("7");
//		gs.parseWorkspace(sampleIDs,false);
		/*
		 * get sample list from gating set
		 */
		cout<<endl<<"get samples from gating set"<<endl;

		vector<string> samples=gs.getSamples();
		for(vector<string>::iterator it=samples.begin();it!=samples.end();it++)
			cout<<*it<<endl;

//		GatingHierarchy* gh=gs.getGatingHierarchy("Specimen_001_A1_A01.fcs");
		GatingHierarchy* gh=gs.getGatingHierarchy(0);
		/*
		 * getNodes by the T order
		 */

//		cout<<endl<<"tsorted node list"<<endl;
		VertexID_vec vertices;
		vertices=gh->getVertices(true);
		for(VertexID_vec::iterator it=vertices.begin();it!=vertices.end();it++)
		{
			nodeProperties *node=gh->getNodeProperty(*it);
			cout<<*it<<"."<<node->getName()<<endl;
		}

		/*
		 * getNodes by vertices ID order
		 * and get stats from each node
		 */

		cout<<endl<<"node list in regular order and stats,gate"<<endl;
		vertices=gh->getVertices(false);
		for(VertexID_vec::iterator it=vertices.begin();it!=vertices.end();it++)
		{
			nodeProperties *node=gh->getNodeProperty(*it);
			cout<<*it<<"."<<node->getName()<<":";
			cout<<node->getStats(false)["count"]<<endl;
			gate * g=node->getGate();
			if(g!=NULL)
				cout<<g->getName()<<endl;
		}

		/*
		 * getPopNames with full path
		 */
		cout<<endl<<"node list with/without full path:"<<endl;
		vector<string> popNames=gh->getPopNames(false,true);
		for(vector<string>::iterator it=popNames.begin();it!=popNames.end();it++)
			cout<<*it<<endl;
		popNames=gh->getPopNames(false,false);
		for(vector<string>::iterator it=popNames.begin();it!=popNames.end();it++)
			cout<<*it<<endl;

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
		 * do the gating after the parsing
		 */
		string ncFile="/home/wjiang2/rglab/workspace/flowWorkspace/output/test.cdf";
		//read colnames from text
		vector<string> params;

		std::ifstream myfile;
		myfile.open("../output/colnames.txt",ifstream::in);
		vector<string> myLines;
		string line;
		while (std::getline(myfile, line))
		{
			params.push_back(line);
		}

		myfile.close();
		gs.attachData(ncFile,params);

		gh->gating();


		cout<<endl<<"print out indices after gating"<<endl;
		vertices=gh->getVertices(false);
		for(VertexID_vec::iterator it=vertices.begin();it!=vertices.end();it++)
		{
			nodeProperties *node=gh->getNodeProperty(*it);
			cout<<*it<<"."<<node->getName()<<":";
			cout<<"flowjo count:"<<node->getStats(false)["count"]<<endl;
			cout<<"flowCore count:"<<node->getCounts(true)<<endl;
			gate * g=node->getGate();
			if(g!=NULL)
				cout<<g->getName()<<endl;
		}

		/*
		 * plot gating hierarchy tree
		 */

//		gh->drawGraph("../output/test.dot");
//		system("dot2gxl ../output/test.dot -o ../output/test.gxl");
		//	return("test.gxl");


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

	fileNames.push_back("../fjWsExamples/Exp1_DC-Mono-NK.wsp");
	fileNames.push_back("../fjWsExamples/Exp1_Treg.wsp");
	fileNames.push_back("../fjWsExamples/Exp2_DC-Mono-NK.wsp");
	test(fileNames.at(0));
//	Rcpp_test(fileNames.at(WIN));



	return (0);
}
