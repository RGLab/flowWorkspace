/*
 * nodeProperties.cpp
 *
 *  Created on: Apr 16, 2012
 *      Author: wjiang2
 */
#include "include/nodeProperties.hpp"
#include <algorithm>
nodeProperties::nodeProperties(){
	thisGate=NULL;
	hidden=false;

}

/* since nodeProperties contains non-copyable scope_ptr member
 * , customized copy and assignment constructor is required
 *
 */
nodeProperties::nodeProperties(const nodeProperties& np){
	thisName=np.thisName;

	thisGate=np.thisGate==NULL?NULL:np.thisGate->clone();
	if(np.indices.get()!=NULL)
		indices.reset(np.indices->clone());
	fjStats=np.fjStats;
	fcStats=np.fcStats;
	hidden=np.hidden;


}
nodeProperties & nodeProperties::operator=(nodeProperties np){
	std::swap(thisName, np.thisName);
	std::swap(thisGate, np.thisGate);
	if(np.indices.get()!=NULL)
		indices.reset(np.indices->clone());
	std::swap(fjStats, np.fjStats);
	std::swap(fcStats, np.fcStats);
	std::swap(hidden, np.hidden);

	return *this;

}

/*
 * gate is dynamically created,so they are freed here in destroy method
 */
nodeProperties::~nodeProperties(){

//	COUT<<"entring the destructor of nodeProperties"<<endl;

	if(thisGate!=NULL)
	{
		if(g_loglevel>=GATE_LEVEL)
			COUT<<"free gate:"<<this->thisName<<endl;
		delete thisGate;
	}
}


POPSTATS nodeProperties::getStats(bool isFlowCore){

	return(isFlowCore?this->fcStats:this->fjStats);
}

void nodeProperties::setStats(POPSTATS s,bool isFlowCore){
	if(isFlowCore)
		fcStats=s;
	else
		fjStats=s;

}

gate * nodeProperties::getGate(){
	if(thisGate==NULL)
		throw(domain_error("gate is not parsed!"));
	return(thisGate);
}

string nodeProperties::getName(){
	return(this->thisName);
}

void nodeProperties::setName(const char * popName){
	if(string(popName).find('/') != std::string::npos){
		throw(domain_error("pop name contains '/' character!"));
	}
	thisName=popName;
}
void nodeProperties::setHiddenFlag(bool _value){
	hidden=_value;
}
bool nodeProperties::getHiddenFlag(){
	return (hidden);
}

void nodeProperties::setGate(gate *gate){
	thisGate=gate;
}

vector<bool> nodeProperties::getIndices(){
		if(!this->isGated())
			throw(domain_error("trying to get Indices for unGated node!"));
		return indices->getIndices();
		}

void nodeProperties::setIndices(unsigned _nEvent){
		indices.reset(new ROOTINDICES(_nEvent));
}
void nodeProperties::setIndices(vector<bool> _ind){
	unsigned nEvents=count(_ind.begin(),_ind.end(),true);
	unsigned nSizeInt=sizeof(unsigned)*nEvents;
	unsigned nSizeBool=_ind.size()/8;

	if(nSizeInt<nSizeBool)
		indices.reset(new INTINDICES(_ind));
	else
		indices.reset(new BOOLINDICES(_ind));

}
/*
 * potentially it is step can be done within the same loop in gating
 * TODO:MFI can be calculated here as well
 */
void nodeProperties::computeStats(){
		fcStats["count"]=getCounts();
}

unsigned nodeProperties::getCounts(){
	if(!this->isGated())
		throw(domain_error("trying to get counts for unGated node!"));
	return indices->getCount();

}


