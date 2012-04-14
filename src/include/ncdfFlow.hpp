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

using namespace std;

class ncdfFlow{
	string fileName;
//	unsigned short nChannels;
public:
	ncdfFlow();
	ncdfFlow(string _fileName);
	void fileName_set(string _fileName);
	string fileName_get();
	float * readSlice(unsigned int sampleID);

};

#endif /* NCDFFLOW_HPP_ */
