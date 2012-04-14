/*
 * flowData.cpp
 *
 *  Created on: Apr 13, 2012
 *      Author: wjiang2
 */

#include "include/flowData.hpp"

flowData::flowData(){};

flowData::flowData(float* mat,unsigned nEvents,unsigned nChannls){

	data=new valarray<float>(mat,nEvents*nChannls);
	nRow=nEvents;
	nCol=nChannls;
}
flowData::flowData(valarray<float> mat,unsigned nEvents,unsigned nChannls){

	data=new valarray<float>(mat);
	nRow=nEvents;
	nCol=nChannls;
}
flowData::~flowData(){
	if(data!=NULL)
		delete data;
}

flowData flowData::subset(POPINDICES rowInd){

//	unsigned newLen=nRow*nCol;
//	POPINDICES newInd(newLen);
//	for(unsigned i=0;i<nCol;i++)
//	{
//
//		 valarray<bool> mymask (true,nRow);
//
//	}
//	return data[newInd];
	/*
	 * convert the rowInd form valarray<bool> to gslice
	 */
	gslice newInd;
	unsigned nNewRow;//new size of the the data
	valarray<float> val=(*data);
	valarray<float> res=val[newInd];
	return  flowData(res,nNewRow,this->nCol);
}
