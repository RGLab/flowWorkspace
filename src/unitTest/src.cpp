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

	//read xml file by libxml
	vector<string> fileNames;

	fileNames.push_back("../data/HIPC_trial/data/HIPC_trial.xml");
	fileNames.push_back("../data/Blomberg/data/Exp2_Tcell.wsp");
	fileNames.push_back("../data/Yale/data/LyoplateTest1Yale.wsp");
//	fileNames.push_back("../fjWsExamples/Exp1_Treg.wsp");
//	fileNames.push_back("../fjWsExamples/Exp2_DC-Mono-NK.wsp");

	gs_test(fileNames.at(1));

//	ncdf_test();

//	Rcpp_test(fileNames.at(WIN));

//	spline_test();

//	compCalTbl();


	cout<<"done!"<<endl;

	return (0);
}



