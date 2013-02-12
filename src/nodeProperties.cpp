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

}

/*
 * gate is dynamically created,so they are freed here in destroy method
 */
nodeProperties::~nodeProperties(){

//	cout<<"entring the destructor of nodeProperties"<<endl;

	if(thisGate!=NULL)
	{
		if(dMode>=GATE_LEVEL)
			cout<<"free gate:"<<this->thisName<<endl;
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
	thisName=popName;
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

		ROOTINDICES *ind=new ROOTINDICES(_nEvent);
		indices.reset(ind);
}
void nodeProperties::setIndices(vector<bool> _ind){
	unsigned nEvents=count(_ind.begin(),_ind.end(),true);
	unsigned nSizeInt=sizeof(unsigned)*nEvents;
	unsigned nSizeBool=_ind.size()/8;

	POPINDICES * ind;
	if(nSizeInt<nSizeBool)
		ind=new INTINDICES(_ind);
	else
		ind=new BOOLINDICES(_ind);
	indices.reset(ind);
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

nodeProperties * nodeProperties::clone(bool gateResult){
		nodeProperties * res=new nodeProperties();
		//copy pop name
		res->setName(thisName.c_str());
		/*
		 * copying gate if applicable
		 */

		if(thisGate!=NULL)
			res->setGate(thisGate->clone());

		/*
		 * copy gated results
		 */
		if(gateResult)
		{
			res->setStats(fcStats,true);
			res->setStats(fjStats,false);
			res->indices.reset(indices->clone());
		}

		return res;
	}
