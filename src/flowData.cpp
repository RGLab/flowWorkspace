/*
 * flowData.cpp
 *
 *  Created on: Apr 13, 2012
 *      Author: wjiang2
 */

#include "include/flowData.hpp"

#include <algorithm>


flowData::flowData(){};

flowData::flowData(double* mat,vector<string> _params,unsigned _nEvents,unsigned _sampleID){


	params=_params;
	unsigned nChannls=params.size();
	nEvents=_nEvents;
	unsigned nSize=nChannls*nEvents;
	sampleID=_sampleID;
	data.resize(nSize);
	data=valarray<double>(mat,nSize);
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
	if(_params.size()!=params.size())
		throw(domain_error("the number of parameters is not consistent with cdf file!"));
	params=_params;
}

unsigned find_pos(vector<string> s,string pattern ){
	vector<string>::iterator it1,it2,res;
	it1=s.begin();
	it2=s.end();
	res=find(it1,it2,pattern);
	if(res==it2)
		throw(domain_error(pattern.append(" not found in ncdfFlowSet!")));
	return (res-it1);
}

slice flowData::getSlice(string channel){

		unsigned paramInd=find_pos(params,channel);

//		valarray<double> data(this->data,nEvents*nChannls);
		return slice(paramInd*nEvents,nEvents,1);

}

valarray<double> flowData::subset(string channel){
		return data[getSlice(channel)];

}



