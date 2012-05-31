//============================================================================
// Name        : src.cpp
// Author      : Mike jiang
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C, Ansi-style
//============================================================================
#include "test_header.hpp"

#define MAC 0
#define WIN 1


int main(void) {

	vector<testSuit> myTest(4);
	unsigned short i;
/*
 * currently sampleName info within the testSuit is not used
 * since it is not necessarily consistent with samplenames within gs
 */
	i=0;
	myTest.at(i).filename="/home/wjiang2/rglab/workspace/flowWorkspace/data/HIPC_trial/data/HIPC_trial.xml";
	myTest.at(i).samples["1"]="004_A1_A01.fcs";
	myTest.at(i).samples["2"]="004_B1_B01.fcs";
	myTest.at(i).ncfile="/home/wjiang2/rglab/workspace/flowWorkspace/output/HIPC_trial/nc_comp.nc";
	myTest.at(i).colfile="/home/wjiang2/rglab/workspace/flowWorkspace/output/HIPC_trial/colnames.txt";

	i=1;
	myTest.at(i).filename="../data/Blomberg/data/Exp2_Tcell.wsp";
	myTest.at(i).samples["12"]="Exp2_Sp004_1_Tcell.fcs";
	myTest.at(i).samples["13"]="Exp2_Sp004_2_Tcell.fcs";
	myTest.at(i).ncfile="../output/Blomberg/nc1_comp.nc";
	myTest.at(i).colfile="../output/Blomberg/colnames.txt";

	i=2;
	myTest.at(i).filename="/loc/no-backup/mike/ITN029ST/QA_MFI_RBC_bounary_eventsV3.xml";
	myTest.at(i).samples["74161"]="01107122_F11_I003.fcs";
	myTest.at(i).samples["74162"]="01177007_F02_I016.fcs";
	myTest.at(i).ncfile="../output/ITN/nc1.nc";
	myTest.at(i).colfile="../output/ITN/colnames.txt";

	i=3;
	myTest.at(i).filename="../data/Yale/data/LyoplateTest1Yale.wsp";
	myTest.at(i).samples["1"]="Specimen_001_A1_A01.fcs";

	//	fileNames.push_back("../fjWsExamples/Exp1_Treg.wsp");
//	fileNames.push_back("../fjWsExamples/Exp2_DC-Mono-NK.wsp");

	i=0;
//	unsigned loop=4000;
//	for(unsigned j=0;j<loop;j++)
//	{
//		cout << "press any key to continue:\n>";
//		string input = "";
//		getline(cin, input);
		gs_parse(myTest.at(i),0);

//	}


//	gating_template_test(myTest.at(i));
//	ncdf_test();

//	Rcpp_test(fileNames.at(WIN));

//	spline_test();

//	compCalTbl();


	cout<<"done!"<<endl;


	return (0);
}



