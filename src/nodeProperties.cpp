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
//	indices=NULL;
}

/*
 * gate is dynamically created,so they are freed here in destroy method
 */
nodeProperties::~nodeProperties(){

	cout<<"entring the destructor of nodeProperties"<<endl;

	if(thisGate!=NULL)
	{
		if(dMode>=GATE_LEVEL)
			cout<<"free gate:"<<this->thisName<<endl;
		delete thisGate;
	}
}


POPSTATS nodeProperties::getStats(bool isFlowCore=false){
//		return(isFlowCore?this->fcStats:this->fjStats);
	return(isFlowCore?this->fcStats:this->fjStats);
}

//unsigned nodeProperties::getCounts(bool isFlowCore=false){
//	if(isFlowCore)
//	{
//		return count(indices.begin(),indices.end(),true);
//	}
//	else
//		return this->fjStats["count"];
//
//}
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
/*
 * potentially it is step can be done within the same loop in gating
 * TODO:MFI can be calculated here as well
 */
void nodeProperties::computeStats(){
		fcStats["count"]=getCounts();
}

unsigned nodeProperties::getCounts(){

	return count(indices.begin(),indices.end(),true);

}

nodeProperties * nodeProperties::clone(){
		nodeProperties * res=new nodeProperties(*this);
		res->thisGate=thisGate->clone();
		return res;
	}
