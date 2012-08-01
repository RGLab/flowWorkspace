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
 * define 2-D matrix class out of valarray to represent channel*events data matrix from one FCS
 * Note that the destructor of this class does not free the data,it is up to caller to release the resource for this class
 * once finish using it
 */

//template <float * INITVAL,unsigned CHANNELS,unsigned EVENTS>
class flowData{

public:

	vector<string> params;
	unsigned sampleID;//it is only valid when access cdf version of flowdata, used as index for sample dimension
	valarray<double> data;
	unsigned nEvents;
	flowData();
	flowData(const double* mat,vector<string>,unsigned _nEvents,unsigned _sampleID);
	flowData(NumericMatrix mat,unsigned _sampleID);
	slice getSlice(string channel);
	valarray<double> subset(string channel);
	void params_set(vector<string> _params);

};




#endif /* FLOWDATA_HPP_ */
