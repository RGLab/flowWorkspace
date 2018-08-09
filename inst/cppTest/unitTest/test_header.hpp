/*
 * test_header.hpp
 *
 *  Created on: May 15, 2012
 *      Author: wjiang2
 */

#ifndef TEST_HEADER_HPP_
#define TEST_HEADER_HPP_



#include "flowWorkspace/openWorkspace.hpp"
#include "flowWorkspace/flowJoWorkspace.hpp"
#include "cytolib/GatingSet.hpp"
#include "cytolib/GatingHierarchy.hpp"
#include "cytolib/transformation.hpp"
#include "cytolib/spline.hpp"
using namespace std;
using namespace cytolib;
using namespace flowWorkspace;

struct testCase{
	string filename; //xml file name
	string archive; // archived gating set dat file
	vector<bool> isEqual; // the bool vector records the counts discrepancy (using cv) between flowJo and flowCore
	float tolerance; // the threshold for cv value
	bool isTemplate;// whether test the template copying feature
	bool isLoadArhive;// whether to load archived gs
	bool isSaveArchive;
	unsigned archiveFormat;
	bool archiveType;// boost or google
	vector<VertexID> skipPops;
	SAMPLE_NAME_LOCATION sample_name_location;
	int xmlParserOption;
	int group_id;
	ParseWorkspaceParameters config;
	testCase()
	{
		tolerance = 0.08;
		archiveFormat = ARCHIVE_TYPE_BINARY;
		archiveType = PB;
		isTemplate = false;
		isLoadArhive = false;
		isSaveArchive = false;
		sample_name_location = SAMPLE_NAME_LOCATION::KEY_WORD;
		xmlParserOption = 1;
		group_id = 0;
	}
} ;


void gh_gating(GatingHierarchy & gh,bool is_fix_slash_in_channel_name, bool isH5, string h5_path,  compensation comp);
void gh_counts(GatingHierarchy& gh,vector<bool> &isEqual, const float tolerance);
void clone_test(testCase myTest);
//void gs_parse(testCase,unsigned short,bool,bool);
void parser_test(testCase &);
void ncdf_test();
void compCalTbl();
void spline_test();
void cpConsTest();
#endif /* TEST_HEADER_HPP_ */
