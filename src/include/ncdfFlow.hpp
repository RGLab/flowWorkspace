/*
 * ncdfFlow.hpp
 *
 *  Created on: Apr 11, 2012
 *      Author: wjiang2
 */

#ifndef NCDFFLOW_HPP_
#define NCDFFLOW_HPP_
#include <string>
#include <netcdfcpp.h>
#include "flowData.hpp"
using namespace std;

class ncdfFlow{
	string fileName;
	vector<string> params;
public:
	ncdfFlow();
	ncdfFlow(string _fileName);
	void fileName_set(string _fileName);
	void params_set(vector<string> _params);
	vector<string> params_get();
	string fileName_get();
	float * readSlice(unsigned int sampleID);
	flowData readflowData(unsigned int sampleID);

};

#endif /* NCDFFLOW_HPP_ */
