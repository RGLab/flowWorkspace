/*
 * test_header.hpp
 *
 *  Created on: May 15, 2012
 *      Author: wjiang2
 */

#ifndef TEST_HEADER_HPP_
#define TEST_HEADER_HPP_


#include <iostream>
#include <fstream>
#include <string>

#include "../include/flowJoWorkspace.hpp"
#include "../include/GatingSet.hpp"
#include "../include/GatingHierarchy.hpp"
#include "../include/R_GatingSet.hpp"
#include "../include/transformation.hpp"
#include "../include/spline.hpp"
#include "ncdfFlow.hpp"
using namespace std;

struct testCase{
	string filename; //xml file name
	string colfile; // text file that records the compensated channel names
	string ncfile; // raw data stored in hdf format
	map<string,string> samples; // fcs file name vs sampleID
	unsigned short sampNloc; // the location where the sample name to be parsed
	string archive; // archived gating set dat file
	vector<bool> isEqual; // the bool vector records the counts discrepancy (using cv) between flowJo and flowCore
	float tolerance; // the threshold for cv value
	bool isParseGate; //whether to parse gate from xml
	unsigned short dMode;//debug output level
	int xmlParserOption;//xml parser option passed down to libxml2
	bool isTemplate;// whether test the template copying feature
	bool isLoadArhive;// whether to load archived gs
	bool isSaveArchive;
	unsigned archiveFormat;

} ;
hdfFlow gs_attachCDF(GatingSet & gs,testCase myTest);
void gs_gating(GatingSet &gs,string curSample, hdfFlow nc);
void gh_counts(GatingHierarchy* gh,vector<bool> &isEqual, const float tolerance);
void clone_test(testCase myTest);
//void gs_parse(testCase,unsigned short,bool,bool);
void parser_test(testCase &);
void ncdf_test();
void compCalTbl();
void spline_test();
void cpConsTest();
#endif /* TEST_HEADER_HPP_ */
