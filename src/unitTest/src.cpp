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

//	splitString("S/Lv/L/3+/4+/IFNg+",'/');
//	toArray("3980,1.9428805e5,3981,1.9478264e5,3982,1.9527849e5,3983,1.9577559e5,3984,1.9627398e5");


	vector<testSuit> myTest(8);
	unsigned short i;

	i=0;
	myTest.at(i).filename="../data/PBMC/HIPC_trial/data/HIPC_trial.xml";
	myTest.at(i).samples["1"]="004_A1_A01.fcs";
	myTest.at(i).samples["2"]="004_B1_B01.fcs";
	myTest.at(i).sampNloc=1;
	myTest.at(i).ncfile="../output/HIPC_trial/nc_comp.nc";
	myTest.at(i).colfile="../output/HIPC_trial/colnames.txt";
	myTest.at(i).archive="../output/HIPC_trial/gs.dat";

	i=1;
	myTest.at(i).filename="../data/PBMC/Blomberg/data/Exp2_Tcell.wsp";
	myTest.at(i).samples["12"]="Exp2_Sp004_1_Tcell.fcs";
	myTest.at(i).samples["13"]="Exp2_Sp004_2_Tcell.fcs";
	myTest.at(i).sampNloc=2;
	myTest.at(i).ncfile="../output/Blomberg/nc1_comp.nc";
	myTest.at(i).colfile="../output/Blomberg/colnames.txt";
	myTest.at(i).archive="../output/Blomberg/gs.dat";

	i=2;
	myTest.at(i).filename="/loc/no-backup/mike/ITN029ST/QA_template.xml";
	myTest.at(i).samples["74161"]="01107122_F11_I003.fcs";
	myTest.at(i).samples["74162"]="01177007_F02_I016.fcs";
	myTest.at(i).sampNloc=1;
	myTest.at(i).ncfile="../output/ITN/nc1.nc";
	myTest.at(i).colfile="../output/ITN/colnames.txt";
	myTest.at(i).archive="../output//ITN/gs.dat";

	i=3;
	myTest.at(i).filename="../data/PBMC/Yale/data/LyoplateTest1Yale.wsp";
	myTest.at(i).samples["1"]="Specimen_001_A1_A01.fcs";
	myTest.at(i).sampNloc=1;
	myTest.at(i).archive="../output/Yale/gs.dat";
	/*
	 * with negated gate
	 */
	i=4;
	myTest.at(i).filename="../data/Cytotrol/NHLBI/flowJo/NHLBI.xml";
	myTest.at(i).samples["1"]="CytoTrol_CytoTrol_1.fcs";
	myTest.at(i).sampNloc=1;
	myTest.at(i).ncfile="../output/NHLBI/nc1_comp.nc";
	myTest.at(i).colfile="../output/NHLBI/colnames.txt";
	myTest.at(i).archive="../output/NHLBI/gs/file41e925ffb2f5.dat";
	/*
	 * with boolean gate
	 */
	i=5;
	myTest.at(i).filename="../data/HVTN054/Workspace/054-wkspace_tmp_tr.xml";
	myTest.at(i).samples["5345"]="180419.fcs";
	myTest.at(i).sampNloc=1;
	myTest.at(i).ncfile="../output/HVTN054/nc1_comp.nc";
	myTest.at(i).colfile="../output/HVTN054/colnames.txt";
	myTest.at(i).archive="../output/HVTN054/gs/gs.dat";

	i=6;
	myTest.at(i).filename="../data/HVTN080/XML/080 Batch 1057 M.xml";
	myTest.at(i).samples["28"]="517614.fcs";
	myTest.at(i).sampNloc=1;
	myTest.at(i).ncfile="../output/HVTN080/nc_comp.nc";
	myTest.at(i).colfile="../output/HVTN080/colnames.txt";
	myTest.at(i).archive="../output/HVTN080/gs/gs.dat";

	i=7;
	myTest.at(i).filename="../data/NormalizationData/XML files/080 batch 0939.xml";
	myTest.at(i).samples["18"]="461648.fcs";
	myTest.at(i).sampNloc=1;
	myTest.at(i).ncfile="../output/NormalizationData/nc_comp.nc";
	myTest.at(i).colfile="../output/NormalizationData/colnames.txt";
	myTest.at(i).archive="../output/NormalizationData/gs/gs.dat";


	i=4;

//	clone_test(myTest.at(i));
//	unsigned loop=4000;
//	for(unsigned j=0;j<loop;j++)
//	{
//		cout << "press any key to continue:\n>";
//		string input = "";
//		getline(cin, input);
		gs_parse(myTest.at(i),4,false,false);

//	}



//	gating_template_test(myTest.at(i));
//	ncdf_test();

//	Rcpp_test(fileNames.at(WIN));

//	spline_test();

//	compCalTbl();


	cout<<"done!"<<endl;


	return (0);
}



