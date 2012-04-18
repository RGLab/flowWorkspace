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
#include <stdexcept>
using namespace std;
typedef vector<bool> POPINDICES;

/*
 * define 2-D matrix class out of valarray to represent channel*events data matrix from one FCS
 * Note that the destructor of this class does not free the data,it is up to caller to release the resource for this class
 * once finish using it
 */

//template <float * INITVAL,unsigned CHANNELS,unsigned EVENTS>
class flowData{

public:
//	valarray<float> * data;
	vector<string> params;
	float * data;
	unsigned nEvents,nChannls;
	flowData();
	flowData(float* mat,unsigned _nEvents,unsigned _nChannls);
//	flowData(valarray<float> mat,unsigned nEvents,unsigned nChannls);
	~flowData();
//	flowData subset(POPINDICES rows);
	void params_set(vector<string> _params);
};


//typedef valarray<valarray<float> > val2d;
//class flowData{
//	val2d  data();
//
//public:
//	flowData(unsigned nEvents,unsigned nChannls);
//	val2d subset(POPINDICES rows,POPINDICES cols);
//
//};


#endif /* FLOWDATA_HPP_ */
