/*
 * flowData.hpp
 *
 *  Created on: Apr 13, 2012
 *      Author: wjiang2
 */

#ifndef FLOWDATA_HPP_
#define FLOWDATA_HPP_
#include <valarray>
//#include "populationNode.hpp"
using namespace std;
typedef valarray<bool> POPINDICES;

/*
 * define 2-D matrix class out of valarray to represent channel*events data matrix from one FCS
 */

//template <float * INITVAL,unsigned CHANNELS,unsigned EVENTS>
class flowData{

public:
	valarray<float> * data;
	unsigned nRow,nCol;
	flowData();
	flowData(float* mat,unsigned nEvents,unsigned nChannls);
	flowData(valarray<float> mat,unsigned nEvents,unsigned nChannls);
	~flowData();
	flowData subset(POPINDICES rows);

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
