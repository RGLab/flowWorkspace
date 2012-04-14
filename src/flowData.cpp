/*
 * flowData.cpp
 *
 *  Created on: Apr 13, 2012
 *      Author: wjiang2
 */

#include "include/flowData.hpp"


flowData::flowData(float* mat,unsigned nEvents,unsigned nChannls){

	data=new valarray<float>(mat,nEvents*nChannls);
	nRow=nEvents;
	nCol=nChannls;
}

flowData::~flowData(){
	delete data;
}
valarray<float> flowData::subset(POPINDICES rowInd){

	unsigned newLen=nRow*nCol;
	POPINDICES newInd(newLen);
	//duplicate row indices and merge them into a longer one
	for(unsigned i=0;i<nCol;i++)
	{

		 valarray<bool> mymask (true,nRow);

	}
}
