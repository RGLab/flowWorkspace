/*
 * flowData.cpp
 *
 *  Created on: Apr 13, 2012
 *      Author: wjiang2
 */

#include "include/flowData.hpp"

#include <algorithm>


flowData::flowData(){};
flowData & flowData::operator=(const flowData& source){
	params=source.params;
	sampleID=source.sampleID;//it is only valid when access cdf version of flowdata, used as index for sample dimension
	nEvents=source.nEvents;
	data.resize(source.data.size());
	data=source.data;
	return *this;

}
flowData::flowData(const float* mat,vector<string> _params,unsigned _nEvents,unsigned _sampleID){


	params=_params;
	unsigned nChannls=params.size();
	nEvents=_nEvents;
	unsigned nSize=nChannls*nEvents;
	sampleID=_sampleID;
	data.resize(nSize);
	data=valarray<float>(mat,nSize);
}
flowData::flowData(NumericMatrix mat,unsigned _sampleID){

	List dimnames=mat.attr("dimnames");
	params=dimnames[1];
	unsigned nChannls=params.size();
	nEvents=mat.nrow();
	unsigned nSize=nChannls*nEvents;
	sampleID=_sampleID;
	data.resize(nSize);
	for(unsigned j=0;j<nSize;j++)
		data[j]=mat[j];

}
valarray<float> flowData::getData(){
	return data;
}
void flowData::getData(float * mat,unsigned nSize){
	for(unsigned i=0;i<nSize;i++)
		mat[i]=data[i];
}
void flowData::setParams(vector<string> _params){
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
		throw(domain_error(pattern.append(" not found in flowData!")));
	return (res-it1);
}


void flowData::updateSlice(string channel,valarray<float> x){
	data[getSlice(channel)]=x;
}

slice flowData::getSlice(string channel){

		unsigned paramInd=find_pos(params,channel);

//		valarray<float> data(this->data,nEvents*nChannls);
		return slice(paramInd*nEvents,nEvents,1);

}

valarray<float> flowData::subset(string channel){
		return data[getSlice(channel)];

}



