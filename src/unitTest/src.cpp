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

	vector<testSuit> myTest(3);

	myTest.at(0).filename="../data/HIPC_trial/data/HIPC_trial.xml";
	myTest.at(0).samples["1"]="004_A1_A01.fcs";
	myTest.at(0).samples["2"]="004_B1_B01.fcs";
	myTest.at(0).ncfile="../output/HIPC_trial/nc_comp.nc";
	myTest.at(0).colfile="../output/HIPC_trial/colnames.txt";


	myTest.at(1).filename="../data/Blomberg/data/Exp2_Tcell.wsp";
	myTest.at(1).samples["12"]="Exp2_Sp004_1_Tcell.fcs";
	myTest.at(1).samples["13"]="Exp2_Sp004_2_Tcell.fcs";
	myTest.at(1).ncfile="../output/Blomberg/nc1_comp.nc";
	myTest.at(1).colfile="../output/Blomberg/colnames.txt";


	myTest.at(2).filename="../data/Yale/data/LyoplateTest1Yale.wsp";
	myTest.at(2).samples["1"]="Specimen_001_A1_A01.fcs";

	//	fileNames.push_back("../fjWsExamples/Exp1_Treg.wsp");
//	fileNames.push_back("../fjWsExamples/Exp2_DC-Mono-NK.wsp");

	gs_test(myTest.at(1));

//	ncdf_test();

//	Rcpp_test(fileNames.at(WIN));

//	spline_test();

//	compCalTbl();


	cout<<"done!"<<endl;

	return (0);
}



