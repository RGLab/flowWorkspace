
#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MODULE Suites
#include <boost/test/unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>

#include "test_header.hpp"
float gTol = 0.05;
unsigned short myTestPolymorphism(){
	gate * g= NULL;

	rectGate rectg =rectGate();
	g=&rectg;

	rectGate * newG = dynamic_cast<rectGate*>(g);
	return newG->getType();

}
//BOOST_AUTO_TEST_SUITE(Polymorph)
//BOOST_AUTO_TEST_CASE(gateCastDown)
//{
//	BOOST_CHECK(myTestPolymorphism() == RECTGATE);
//	BOOST_CHECK(myTestPolymorphism() != POLYGONGATE);
//
//}
//BOOST_AUTO_TEST_SUITE_END()
//
//BOOST_AUTO_TEST_SUITE(RegExp)
//BOOST_AUTO_TEST_CASE(flowChannelMatch)
//{
//	string strPattern = "[FS]SC-[AWH]";
//	boost::regex ex(strPattern);
//
//	BOOST_CHECK(boost::regex_match( "FSC",ex) == false);
//	BOOST_CHECK(boost::regex_match( "FSC-A",ex) == true);
//	BOOST_CHECK(boost::regex_match( "FSC-AD",ex) == false);
//}
//
//BOOST_AUTO_TEST_SUITE_END()
struct globalFixture{
	globalFixture(){
		cout << "Enter tolerance (e.g. 0.08):" << endl;
//		cin >> gTol;
		gTol = 0.08;
	};
	~globalFixture(){};

};
BOOST_GLOBAL_FIXTURE(globalFixture)
struct parseWorkspaceFixture{
	parseWorkspaceFixture(){
		myTest.tolerance = gTol;
		myTest.isParseGate = true;
		myTest.dMode = 0;
		myTest.xmlParserOption = 1;
		myTest.isTemplate = false;
		myTest.isLoadArhive = false;
		myTest.archiveFormat = ARCHIVE_TYPE_BINARY;
		myTest.isSaveArchive = false;
	};
	~parseWorkspaceFixture(){};
	testCase myTest;

};

BOOST_FIXTURE_TEST_SUITE(parseWorkspace,parseWorkspaceFixture)
BOOST_AUTO_TEST_CASE(PBMC_HIPC_trial)
{

	myTest.filename="../data/PBMC/HIPC_trial/data/HIPC_trial.xml";
	myTest.wsType = WS_MAC;
	myTest.samples["1"]="004_A1_A01.fcs";
	myTest.samples["2"]="004_B1_B01.fcs";
	myTest.sampNloc=1;
	myTest.ncfile="../output/HIPC_trial/nc_comp.nc";
	myTest.colfile="../output/HIPC_trial/colnames.txt";
	myTest.archive="../output/HIPC_trial/gs.dat";

//	myTest.isLoadArhive = true;
	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}
BOOST_AUTO_TEST_CASE(PBMC_Blomberg)
{
	myTest.filename="../data/PBMC/Blomberg/Exp2_Tcell.wsp";
	myTest.wsType = WS_WIN;
	myTest.samples["12"]="Exp2_Sp004_1_Tcell.fcs";
	myTest.samples["13"]="Exp2_Sp004_2_Tcell.fcs";
	myTest.sampNloc=2;
	myTest.ncfile="../output/Blomberg/nc1_comp.nc";
	myTest.colfile="../output/Blomberg/colnames.txt";
	myTest.archive="../output/Blomberg/gs.dat";

//	myTest.isLoadArhive = true;

	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}
BOOST_AUTO_TEST_CASE(ITN029ST)
{
	myTest.filename="../fjWsExamples/QA_template.xml";
	myTest.wsType = WS_MAC;
	myTest.samples["74161"]="01107122_F11_I003.fcs";
	myTest.samples["74162"]="01177007_F02_I016.fcs";
	myTest.sampNloc=1;
	myTest.ncfile="../output/ITN/nc1.nc";
	myTest.colfile="../output/ITN/colnames.txt";
	myTest.archive="../output//ITN/gs.dat";

	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}
BOOST_AUTO_TEST_CASE(Cytotrol_NHLBI)
{
	myTest.filename="../data/Cytotrol/NHLBI/flowJo/NHLBI.xml";
	myTest.wsType = WS_MAC;
	myTest.samples["1"]="CytoTrol_CytoTrol_1.fcs";
	myTest.sampNloc=1;
	myTest.ncfile="../output/NHLBI/nc1_comp.nc";
	myTest.colfile="../output/NHLBI/colnames.txt";
	myTest.archive="../output/NHLBI/gs/file41e925ffb2f5.dat";

	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}
BOOST_AUTO_TEST_CASE(HVTN080_batch_1057)
{
	myTest.filename="../fjWsExamples/080 Batch 1057 M.xml";
	myTest.wsType = WS_MAC;
	myTest.samples["28"]="517614.fcs";
	myTest.sampNloc=1;
	myTest.ncfile="../output/HVTN080/nc_comp.nc";
	myTest.colfile="../output/HVTN080/colnames.txt";
	myTest.archive="../output/HVTN080/gs/gs.dat";


	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}
BOOST_AUTO_TEST_CASE(HVTN080_batch_0939)
{
	myTest.filename="../fjWsExamples/080 batch 0939.xml";
	myTest.wsType = WS_MAC;
	myTest.samples["18"]="461648.fcs";
	myTest.sampNloc=1;
	myTest.ncfile="../output/NormalizationData/nc_comp.nc";
	myTest.colfile="../output/NormalizationData/colnames.txt";
	myTest.archive="../output/NormalizationData/gs/gs.dat";



	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}
BOOST_AUTO_TEST_CASE(Lesson_8_vX_A)
{
	myTest.filename="../data/vX/Lesson_8_vX.wsp";
	myTest.wsType = WS_VX;
	myTest.samples["1"]="A1.fcs";
	myTest.sampNloc=1;
	myTest.ncfile="../output/vX/A1/nc_comp.nc";
	myTest.colfile="../output/vX/A1/colnames.txt";
	myTest.archive="../output/vX/A1/gs/gs.dat";


	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}
BOOST_AUTO_TEST_CASE(Lesson_8_vX_B)
{
	myTest.filename="../data/vX/Lesson_8_vX.wsp";
	myTest.wsType = WS_VX;
	myTest.samples["10"]="B1 .fcs";
	myTest.sampNloc=1;
	myTest.ncfile="../output/vX/B1/nc_comp.nc";
	myTest.colfile="../output/vX/B1/colnames.txt";
	myTest.archive="../output/vX/B1/gs/gs.dat";

	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}
BOOST_AUTO_TEST_CASE(bioaster)
{
	myTest.filename="../data/bioaster_ellipsoidGate/Manip du 29-11-2013/Matrice 1.wsp";
	myTest.wsType = WS_VX;
	myTest.samples["8"]="PANEL 1_Matrice 1.fcs";
	myTest.sampNloc=1;
	myTest.ncfile="../output/bioaster/nc_comp.nc";
	myTest.colfile="../output/bioaster/colnames.txt";
	myTest.archive="../output/bioaster/gs/gs.dat";

//	myTest.isSaveArchive = true;
//	myTest.isLoadArhive = true;

//	myTest.dMode = GATE_LEVEL;

	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}

//BOOST_AUTO_TEST_CASE(mssm)
//{
//	myTest.filename="../data/mssm/CFSP_Analysis14.wsp";
//	myTest.wsType = WS_VX;
//	myTest.samples["35"]="35120.fcs";
//	myTest.sampNloc=1;
//	myTest.ncfile="../output/mssm/data.nc";
//	myTest.colfile="../output/mssm/colnames.txt";
//	myTest.archive="../data/mssm/gs.dat";
//
//	parser_test(myTest);
//
//	vector<bool> isTrue(myTest.isEqual.size(), true);
//	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());
//
//}
//BOOST_AUTO_TEST_CASE(RV144) //workspace missing (original  .xml is not valid xml)
//{
//	myTest.filename="../fjWsExamples/Batch 1264 RV144.wsp";
//	myTest.wsType = WS_WIN;
//	myTest.samples["85"]="977531.fcs";
//	myTest.sampNloc=1;
//	myTest.ncfile="../output/RV144/nc_comp.nc";
//	myTest.colfile="../output/RV144/colnames.txt";
//	myTest.archive="../output/RV144/gs/gs.dat";
//
//
//
//	parser_test(myTest);
//
//	vector<bool> isTrue(myTest.isEqual.size(), true);
//	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());
//
//}
BOOST_AUTO_TEST_SUITE_END()
