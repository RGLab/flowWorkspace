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

using namespace std;

struct testSuit{
	string filename;
	string colfile;
	string ncfile;
	map<string,string> samples;
	unsigned short sampNloc;
	string archive;
} ;


void gs_parse(testSuit,unsigned short,bool,bool);
void ncdf_test();
void compCalTbl();
void spline_test();
#endif /* TEST_HEADER_HPP_ */
