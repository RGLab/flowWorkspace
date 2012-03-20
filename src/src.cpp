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

using namespace std;



int main(void) {

	//read xml file by libxml
	const char * filename="/home/wjiang2/rglab/workspace/HIPC-Lyoplate/data/HIPC_trial.xml";
	GatingSet gs;
	gs.openWorkspace(filename);


	//construct GatingHierarchy without associate it with sample
//	GatingHierarchy gh;
//	gh.gating();
//	gh.gating();
	//construct GatingSet that holds GatingHierarchy for each individual sample and gate it afterwards

	//



	return 0;
}
