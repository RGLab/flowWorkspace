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

void gs_test(string);
void ncdf_test();
void compCalTbl();
void spline_test();
#endif /* TEST_HEADER_HPP_ */
