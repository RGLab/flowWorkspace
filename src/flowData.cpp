/*
 * flowData.cpp
 *
 *  Created on: Apr 13, 2012
 *      Author: wjiang2
 */

#include "include/flowData.hpp"

#include <algorithm>


flowData::flowData(){ignore_case = false;nEvents =0;}
flowData & flowData::operator=(const flowData& source){
	params=source.params;
	ignore_case=source.ignore_case;
	sampleID=source.sampleID;//it is only valid when access cdf version of flowdata, used as index for sample dimension
	nEvents=source.nEvents;
	data.resize(source.data.size());
	data=source.data;
	return *this;

}
flowData::flowData(const double* mat,vector<string> _params,unsigned _nEvents,unsigned _sampleID, bool _ignore_case){


	params=_params;
	unsigned nChannls=params.size();
	nEvents=_nEvents;
	unsigned nSize=nChannls*nEvents;
	sampleID=_sampleID;
	data.resize(nSize);
	data=valarray<double>(mat,nSize);
	ignore_case = _ignore_case;
}
flowData::flowData(NumericMatrix mat,unsigned _sampleID, bool _ignore_case){

	List dimnames=mat.attr("dimnames");
	params=as<vector<string> >(dimnames[1]);
	unsigned nChannls=params.size();
	nEvents=mat.nrow();
	unsigned nSize=nChannls*nEvents;
	sampleID=_sampleID;
	data.resize(nSize);
	for(unsigned j=0;j<nSize;j++)
		data[j]=mat[j];
	ignore_case = _ignore_case;
}
valarray<double> flowData::getData(){
	return data;
}
void flowData::getData(double * mat,unsigned nSize){
	for(unsigned i=0;i<nSize;i++)
		mat[i]=data[i];
}
void flowData::setParams(vector<string> _params){
	if(_params.size()!=params.size())
		throw(domain_error("the number of parameters is not consistent with cdf file!"));
	params=_params;
}
struct InsensitiveCompare
{
  std::string comp;

  InsensitiveCompare( std::string const &s ) : comp(s) {}

  bool operator() ( std::string const &test ) const
  {
//	  string icomp = boost::to_lower_copy(comp);
//	  string itest = boost::to_lower_copy(test);
//	  return (icomp.compare(itest) == 0);
    return (boost::iequals(comp, test));
  }
};
unsigned find_pos(vector<string> s,string pattern, bool ignore_case){
	vector<string>::iterator it1,it2,res;
	it1=s.begin();
	it2=s.end();
	if(ignore_case)
	{
		res=find_if(it1,it2,InsensitiveCompare(pattern));
	}
	else
		res=find(it1,it2,pattern);
	if(res==it2)
		throw(domain_error(pattern.append(" not found in flowData!")));
	return (res-it1);
}


void flowData::updateSlice(string channel,valarray<double> x){
	data[getSlice(channel)]=x;
}

slice flowData::getSlice(string channel){

		unsigned paramInd=find_pos(params,channel, ignore_case);

//		valarray<double> data(this->data,nEvents*nChannls);
		return slice(paramInd*nEvents,nEvents,1);

}

valarray<double> flowData::subset(string channel){
		return data[getSlice(channel)];

}



