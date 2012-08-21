/*
 * flowData.hpp
 *
 *  Created on: Apr 13, 2012
 *      Author: wjiang2
 */

#ifndef FLOWDATA_HPP_
#define FLOWDATA_HPP_
#include <vector>
#include <iostream>
#include <string>
#include <valarray>
#include <stdexcept>
#include <Rcpp.h>
using namespace std;
using namespace Rcpp;
typedef vector<bool> POPINDICES;//maybe try boost::dynamic_bitset to speed up

/*
 * representing one FCS data
 */
class flowData{

	vector<string> params;
	unsigned sampleID;//it is only valid when access cdf version of flowdata, used as index for sample dimension
	valarray<double> data;
	unsigned nEvents;
public:
	flowData & operator=(const flowData& source);//explicitly define the copy assignment since the default one is compiler-specific
	flowData();
	flowData(const double* mat,vector<string>,unsigned _nEvents,unsigned _sampleID);
	flowData(NumericMatrix mat,unsigned _sampleID);
	slice getSlice(string channel);
	void updateSlice(string channel,valarray<double> x);
	valarray<double> subset(string channel);
	/*
	 * accessors
	 */
	void setParams(vector<string> _params);
	vector<string> getParams(){return params;};
	void setEventCount(unsigned _nEvents){nEvents=_nEvents;};
	unsigned getEventsCount(){return nEvents;};
	void setSampleID(unsigned _sampleID){sampleID=_sampleID;};
	unsigned getSampleID(){return sampleID;};

	void clear(){data.resize(0);};
	unsigned dataSize(){return data.size();};
	void getData(double * mat,unsigned nSize);
	valarray<double> getData();
};




#endif /* FLOWDATA_HPP_ */
