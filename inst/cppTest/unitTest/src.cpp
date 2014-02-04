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



	vector<testCase> myTest(16);
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
	myTest.at(i).filename="../data/PBMC/Blomberg/Exp2_Tcell.wsp";
	myTest.at(i).samples["12"]="Exp2_Sp004_1_Tcell.fcs";
	myTest.at(i).samples["13"]="Exp2_Sp004_2_Tcell.fcs";
	myTest.at(i).sampNloc=2;
	myTest.at(i).ncfile="../output/Blomberg/nc1_comp.nc";
	myTest.at(i).colfile="../output/Blomberg/colnames.txt";
	myTest.at(i).archive="../output/Blomberg/gs.dat";

	i=2;
	myTest.at(i).filename="/shared/silo_researcher/Gottardo_R/mike_working/ITN029ST/QA_template.xml";
	myTest.at(i).samples["74161"]="01107122_F11_I003.fcs";
	myTest.at(i).samples["74162"]="01177007_F02_I016.fcs";
	myTest.at(i).sampNloc=1;
	myTest.at(i).ncfile="../output/ITN/nc1.nc";
	myTest.at(i).colfile="../output/ITN/colnames.txt";
	myTest.at(i).archive="../output//ITN/gs.dat";

	i=3;
	//testing data not available anymore
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
	//testing data not available anymore

	i=6;
	myTest.at(i).filename="../data/HVTN/HVTN080/XML files/080 Batch 1057 M.xml";
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

	i=8;
	myTest.at(i).filename="../data/RV144/Batch 1264 RV144.wsp";
	myTest.at(i).samples["85"]="977531.fcs";
	myTest.at(i).sampNloc=1;
	myTest.at(i).ncfile="../output/RV144/nc_comp.nc";
	myTest.at(i).colfile="../output/RV144/colnames.txt";
	myTest.at(i).archive="../output/RV144/gs/gs.dat";

	i=9;
	/*
	 * this is a mal-formatted workspace
	 */
//	myTest.at(i).filename="../data/vX/testWsComp.wsp";
//	myTest.at(i).samples["2"]="Specimen_003_D12_D12.fcs";
//	myTest.at(i).sampNloc=1;
//	myTest.at(i).ncfile="../output/vX/nc_comp.nc";
//	myTest.at(i).colfile="../output/vX/colnames.txt";
//	myTest.at(i).archive="../output/vX/gs/gs.dat";


	i=10;
	myTest.at(i).filename="../data/vX/Lesson_8_vX.wsp";
	myTest.at(i).samples["1"]="A1.fcs";
	myTest.at(i).sampNloc=1;
	myTest.at(i).ncfile="../output/vX/A1/nc_comp.nc";
	myTest.at(i).colfile="../output/vX/A1/colnames.txt";
	myTest.at(i).archive="../output/vX/A1/gs/gs.dat";

	i=11;
	myTest.at(i).filename="../data/vX/Lesson_8_vX.wsp";
	myTest.at(i).samples["10"]="B1 .fcs";
	myTest.at(i).sampNloc=1;
	myTest.at(i).ncfile="../output/vX/B1/nc_comp.nc";
	myTest.at(i).colfile="../output/vX/B1/colnames.txt";
	myTest.at(i).archive="../output/vX/B1/gs/gs.dat";

	i=12;
	myTest.at(i).filename="../data/vX/Lesson_8_vX.wsp";
	myTest.at(i).samples["19"]="C1.fcs";
	myTest.at(i).sampNloc=1;
	myTest.at(i).ncfile="../output/vX/C1/nc_comp.nc";
	myTest.at(i).colfile="../output/vX/C1/colnames.txt";
	myTest.at(i).archive="../output/vX/C1/gs/gs.dat";

	i=13;
	myTest.at(i).filename="../data/vX/Lesson_8_vX.wsp";
	myTest.at(i).samples["28"]="D1.fcs";
	myTest.at(i).sampNloc=1;
	myTest.at(i).ncfile="../output/vX/D1/nc_comp.nc";
	myTest.at(i).colfile="../output/vX/D1/colnames.txt";
	myTest.at(i).archive="../output/vX/D1/gs/gs.dat";

	i=14;
	myTest.at(i).filename="/home/wjiang2/rglab/workspace/BioC/release/data_store/flowWorkspaceData/inst/gs_auto1";
	myTest.at(i).samples["28"]="D1.fcs";
	myTest.at(i).sampNloc=1;
	myTest.at(i).ncfile="/home/wjiang2/rglab/workspace/BioC/release/data_store/flowWorkspaceData/inst/gs_auto1/file65224bf698a.nc";
	myTest.at(i).colfile="../output/vX/D1/colnames.txt";
	myTest.at(i).archive="/home/wjiang2/rglab/workspace/BioC/release/data_store/flowWorkspaceData/inst/gs_auto1/e2PN2xnk0u.txt";

	i=15;
	myTest.at(i).filename="../data/mssm/CFSP_Analysis14.wsp";
	myTest.at(i).samples["35"]="35120.fcs";
	myTest.at(i).sampNloc=1;
	myTest.at(i).ncfile="../output/mssm/data.nc";
	myTest.at(i).colfile="../output/mssm/colnames.txt";
	myTest.at(i).archive="../data/mssm/gs.dat";

//	cout<<"select test sample id: "<<endl;
//	cin>>i;
	i=15;

//	clone_test(myTest.at(i));
//	unsigned loop=4000;
//	for(unsigned j=0;j<loop;j++)
//	{
//		cout << "press any key to continue:\n>";
//		string input = "";
//		getline(cin, input);
//		gs_parse(myTest.at(i),4,false,false);

//	}
//	cout<<"whether to load archive:"<<endl;
	bool isLoadArhive;
//	cin>>isLoadArhive;
	isLoadArhive = false;
	unsigned format = ARCHIVE_TYPE_BINARY;
	archiving_test(myTest.at(i),4,false,isLoadArhive, format);


//	gating_template_test(myTest.at(i));
//	ncdf_test();

//	Rcpp_test(fileNames.at(WIN));

//	spline_test();

//	compCalTbl();


	cout<<"done!"<<endl;


	return (0);
}



