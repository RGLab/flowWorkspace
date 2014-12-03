/*
 * POPINDICES.cpp
 *
 *  Created on: Oct 8, 2012
 *      Author: wjiang2
 */

#include "include/POPINDICES.hpp"

vector<bool> BOOLINDICES::getIndices(){
	return x;
}

/*
 * convert int to bool indices
 */
vector<bool> INTINDICES::getIndices(){

	vector<bool> res(nEvents,false);

	for(vector<unsigned>::iterator it=x.begin();it!=x.end();it++){
		unsigned i=*it;
		res.at(i)=true;
	}
	return res;
}

vector<bool> ROOTINDICES::getIndices(){

	vector<bool> res(nEvents,true);

	return res;
}


unsigned BOOLINDICES::getCount(){
	return count(x.begin(),x.end(),true);
}

unsigned INTINDICES::getCount(){

	return x.size();
}

unsigned ROOTINDICES::getCount(){

	return nEvents;
}
BOOLINDICES::BOOLINDICES(vector <bool> _ind){
	x=_ind;
	nEvents=_ind.size();

}

INTINDICES::INTINDICES(vector <bool> _ind){

	for(vector<bool>::iterator it=_ind.begin();it!=_ind.end();it++)
	{
		unsigned i=it-_ind.begin();
		if(*it)
			x.push_back(i);
	}
	nEvents=_ind.size();
}


POPINDICES * BOOLINDICES::clone(){
	BOOLINDICES * res=new BOOLINDICES(*this);
	return res;
}

POPINDICES * INTINDICES::clone(){

	INTINDICES * res=new INTINDICES(*this);
	return res;
}
POPINDICES * ROOTINDICES::clone(){

	ROOTINDICES * res=new ROOTINDICES(*this);
	return res;
}
