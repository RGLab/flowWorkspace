/*
 * flowData.cpp
 *
 *  Created on: Apr 13, 2012
 *      Author: wjiang2
 */

#include "include/flowData.hpp"

flowData::flowData(){};

flowData::flowData(double* mat,unsigned _nEvents,unsigned _nChannls){

//	data=new valarray<float>(mat,nEvents*nChannls);
	data=mat;
	nEvents=_nEvents;
	nChannls=_nChannls;
}

//flowData::flowData(valarray<float> mat,unsigned nEvents,unsigned nChannls){
//
//	data=new valarray<float>(mat);
//	nRow=nEvents;
//	nCol=nChannls;
//}
/*
 * this object simply serves as a wrapper to hold dimension info, it is up to caller to free raw data that is pointed by this flowData object,
 */
flowData::~flowData(){
//	if(data!=NULL)
//	{
//		delete data;
//		cout<<"free flowData"<<endl;
//	}

}
void flowData::params_set(vector<string> _params){
	if(_params.size()!=nChannls)
		throw(domain_error("the number of parameters is not consistent with cdf file!"));
	params=_params;
}
//flowData flowData::subset(POPINDICES rowInd){
//
////	unsigned newLen=nRow*nCol;
////	POPINDICES newInd(newLen);
////	for(unsigned i=0;i<nCol;i++)
////	{
////
////		 valarray<bool> mymask (true,nRow);
////
////	}
////	return data[newInd];
//	/*
//	 * convert the rowInd form valarray<bool> to gslice
//	 */
//	gslice newInd;
//
//	unsigned nNewRow=newInd.size();//new size of the the data
//	return  flowData((*data)[newInd],nNewRow,this->nCol);
//}
