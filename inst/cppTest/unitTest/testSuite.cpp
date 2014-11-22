
#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MODULE Suites
#include <boost/test/unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>

#include "test_header.hpp"
float gTol = 0.05;

//unsigned short myTestPolymorphism(){
//	gate * g= NULL;
//
//	rectGate rectg =rectGate();
//	g=&rectg;
//
//	rectGate * newG = dynamic_cast<rectGate*>(g);
//	return newG->getType();
//
//}
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
	parseWorkspaceFixture(): argc(boost::unit_test_framework::framework::master_test_suite().argc),
	           argv(boost::unit_test_framework::framework::master_test_suite().argv)
	{
		/*
		 * parse argv
		 */
		map<string, string> arg_map;
		for(int i = 1; i < argc; i++){
			string thisArg = argv[i];
			vector<string> strSplit;
			boost::split(strSplit,thisArg, boost::is_any_of("="));
			if(strSplit.size() != 2)
				throw(domain_error("invalid arguments!"));
			else{
				string argName = strSplit.at(0);
				boost::replace_first(argName, "--", "");
				string argValue = strSplit.at(1);
				arg_map[argName] = argValue;
			}
		}
		map<string, string>::iterator it;
		myTest.tolerance = gTol;
		myTest.isParseGate = true;
		myTest.xmlParserOption = 1;
		myTest.isTemplate = false;

		myTest.archiveFormat = ARCHIVE_TYPE_BINARY;
		myTest.isSaveArchive = false;

		it = arg_map.find("archiveType");
		myTest.archiveType = it==arg_map.end()?false:it->second == "PB";

		it = arg_map.find("isLoadArchive");

		myTest.isLoadArhive = it==arg_map.end()?false:boost::lexical_cast<bool>(it->second);

		it = arg_map.find("isSaveArchive");
		myTest.isSaveArchive = it==arg_map.end()?false:boost::lexical_cast<bool>(it->second);

		it = arg_map.find("g_loglevel");
		g_loglevel = it==arg_map.end()?false:boost::lexical_cast<unsigned>(it->second);

	};


	~parseWorkspaceFixture(){};
   int argc;
   char **argv;
	testCase myTest;

};

BOOST_FIXTURE_TEST_SUITE(parseWorkspace,parseWorkspaceFixture)
BOOST_AUTO_TEST_CASE(PBMC_HIPC_trial)
{

	myTest.filename="../wsTestSuite/PBMC/HIPC_trial/data/HIPC_trial.xml";
	myTest.wsType = WS_MAC;
	myTest.samples["1"]="004_A1_A01.fcs";
	myTest.samples["2"]="004_B1_B01.fcs";
	myTest.sampNloc=1;
	myTest.ncfile="../output/HIPC_trial/nc_comp.nc";
	myTest.colfile="../output/HIPC_trial/colnames.txt";
	myTest.archive="../output/HIPC_trial/gs";
//	g_loglevel = GATE_LEVEL;
	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}
BOOST_AUTO_TEST_CASE(PBMC_Blomberg)
{
	myTest.filename="../wsTestSuite/PBMC/Blomberg/Exp2_Tcell.wsp";
	myTest.wsType = WS_WIN;
	myTest.samples["12"]="Exp2_Sp004_1_Tcell.fcs";
	myTest.samples["13"]="Exp2_Sp004_2_Tcell.fcs";
	myTest.sampNloc=2;
	myTest.ncfile="../output/Blomberg/nc1_comp.nc";
	myTest.colfile="../output/Blomberg/colnames.txt";
	myTest.archive="../output/Blomberg/gs";
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
	myTest.archive="../output//ITN/gs";

	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}
BOOST_AUTO_TEST_CASE(Cytotrol_NHLBI)
{
	myTest.filename="../wsTestSuite/Cytotrol/NHLBI/flowJo/NHLBI.xml";
	myTest.wsType = WS_MAC;
	myTest.samples["1"]="CytoTrol_CytoTrol_1.fcs";
	myTest.sampNloc=1;
	myTest.ncfile="../output/NHLBI/nc1_comp.nc";
	myTest.colfile="../output/NHLBI/colnames.txt";
	myTest.archive="../output/NHLBI/gs/file41e925ffb2f5";
//	g_loglevel = GATE_LEVEL;

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
	myTest.archive="../output/HVTN080/gs/gs";


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
	myTest.archive="../output/NormalizationData/gs/gs";

	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}
BOOST_AUTO_TEST_CASE(Lesson_8_vX_A)
{
	myTest.filename="../wsTestSuite/vX/Lesson_8_vX.wsp";
	myTest.wsType = WS_VX;
	myTest.samples["1"]="A1.fcs";
	myTest.sampNloc=1;
	myTest.ncfile="../output/vX/A1/nc_comp.nc";
	myTest.colfile="../output/vX/A1/colnames.txt";
	myTest.archive="../output/vX/A1/gs/gs";

	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}
BOOST_AUTO_TEST_CASE(Lesson_8_vX_B)
{
	myTest.filename="../wsTestSuite/vX/Lesson_8_vX.wsp";
	myTest.wsType = WS_VX;
	myTest.samples["10"]="B1 .fcs";
	myTest.sampNloc=1;
	myTest.ncfile="../output/vX/B1/nc_comp.nc";
	myTest.colfile="../output/vX/B1/colnames.txt";
	myTest.archive="../output/vX/B1/gs/gs";

	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}
BOOST_AUTO_TEST_CASE(bioaster)
{
	myTest.filename="../wsTestSuite/bioaster_ellipsoidGate/Matrice 1.wsp";
	myTest.wsType = WS_VX;
	myTest.samples["8"]="PANEL 1_Matrice 1.fcs";
	myTest.sampNloc=1;
	myTest.ncfile="../output/bioaster/nc_comp.nc";
	myTest.colfile="../output/bioaster/colnames.txt";
	myTest.archive="../output/bioaster/gs/gs";


	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}
//BOOST_AUTO_TEST_CASE(lyoplate)
//{
//
//	myTest.filename="";
//	myTest.wsType = WS_MAC;
//	myTest.samples["1"]="12828_1_Tcell_A01.fcs";
//	myTest.sampNloc=1;
//	myTest.ncfile="";
//	myTest.colfile="/loc/no-backup/mike/colnames.txt";
//	myTest.archive="/home/wjiang2/rglab/workspace/analysis/Lyoplate_new/output/test_pb/D54tFo7RPl";
////	g_loglevel = GATE_LEVEL;
//	parser_test(myTest);
//
//	vector<bool> isTrue(myTest.isEqual.size(), true);
//	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());
//
//}
//BOOST_AUTO_TEST_CASE(mssm)
//{
//	myTest.filename="../data/mssm/CFSP_Analysis14.wsp";
//	myTest.wsType = WS_VX;
//	myTest.samples["35"]="35120.fcs";
//	myTest.sampNloc=1;
//	myTest.ncfile="../output/mssm/data.nc";
//	myTest.colfile="../output/mssm/colnames.txt";
//	myTest.archive="../data/mssm/gs";
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
//	myTest.archive="../output/RV144/gs/gs";
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
