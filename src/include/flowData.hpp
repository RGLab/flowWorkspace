/*
 * flowData.hpp
 *
 *  Created on: Apr 13, 2012
 *      Author: wjiang2
 */

#ifndef FLOWDATA_HPP_
#define FLOWDATA_HPP_
#include <valarray>
using namespace std;

/*
 * define 2-D matrix class out of valarray
 */

//template <float * INITVAL,unsigned CHANNELS,unsigned EVENTS>
class flowData{
	valarray<float> * data;
	unsigned nRow,nCol;
public:
	flowData(float* mat,unsigned nEvents,unsigned nChannls);
	valarray<float> subset(POPINDICES rows);

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
