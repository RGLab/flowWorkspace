/*
 * nodeProperties.cpp
 *
 *  Created on: Apr 16, 2012
 *      Author: wjiang2
 */
#include "include/nodeProperties.hpp"

nodeProperties::nodeProperties(){
	thisGate=NULL;
//	indices=NULL;
}

nodeProperties::~nodeProperties(){


	if(thisGate!=NULL)
	{
		if(dMode>=GATE_LEVEL)
			cout<<"free gate:"<<this->thisName<<endl;
		delete thisGate;
	}
}


POPSTATS nodeProperties::getStats(bool isFlowCore=false){
		return(isFlowCore?this->fcStats:this->fjStats);
}

gate * nodeProperties::getGate(){
	return(this->thisGate);
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
