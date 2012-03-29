//============================================================================
// Name        : src.cpp
// Author      : Mike jiang
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C, Ansi-style
//============================================================================

#include <iostream>
#include <string>



#include "flowJoWorkspace.hpp"
#include "GatingSet.hpp"
#include "GatingHierarchy.hpp"
using namespace std;



int main(void) {

	//read xml file by libxml
	vector<string> fileNames;

	fileNames.push_back("fjWsExamples/HIPC_trial.xml");
	fileNames.push_back("fjWsExamples/LyoplateTest1Yale.wsp");
	GatingSet gs;
	gs.openWorkspace(fileNames.at(1));
	unsigned short groupID=1;
	gs.parseWorkspace(groupID);

	GatingHierarchy gh=gs.getGatingHierarchy(1);
	gh.drawGraph();
	//construct GatingHierarchy without associate it with sample
//	GatingHierarchy gh;
//	gh.gating();
//	gh.gating();
	//construct GatingSet that holds GatingHierarchy for each individual sample and gate it afterwards

	//



	return 0;
}
