/*
 * POPINDICES.cpp
 *
 *  Created on: Oct 8, 2012
 *      Author: wjiang2
 */

#include "include/POPINDICES.hpp"
void packToBytes(const vector <bool> & x, vector<unsigned char> & bytes){
	/*
	 * pack bits into bytes
	 */

	for(unsigned i =0 ; i < x.size(); i++) {
		unsigned byteIndex = i / 8;
		unsigned bitIndex = i % 8;
		if(x[i]) {
			// set bit
			unsigned char mask  = 1 << bitIndex;
			bytes[byteIndex] = bytes[byteIndex] | mask;
		}
	}

}
void unpackFromBytes(vector <bool> & x, const vector<unsigned char>& x_bytes){

	/*
	 * unpack bytes into bits
	 */
	for(unsigned i =0 ; i < x.size(); i++) {
		unsigned byteIndex = i / 8;
		unsigned bitIndex = i % 8;

		x[i] = x_bytes[byteIndex] & (1 << bitIndex);
	}

}

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
void BOOLINDICES::convertToPb(pb::POPINDICES & ind_pb){
	ind_pb.set_indtype(pb::BOOL);
	unsigned nBits=x.size();
	unsigned nBytes=ceil(float(nBits)/8);
	vector<unsigned char> bytes(nBytes,0);
	packToBytes(x, bytes);
	string * byte_pb = ind_pb.mutable_bind();
	for(unsigned i = 0; i < bytes.size(); i++){
		unsigned char byte = bytes.at(i);
		byte_pb->append(string(1, byte));
	}
	ind_pb.set_nevents(nEvents);
}
BOOLINDICES::BOOLINDICES(const pb::POPINDICES & ind_pb){
	nEvents = ind_pb.nevents();
	//fetch byte stream from pb
	vector<unsigned char> bytes(ind_pb.bind().begin(),ind_pb.bind().end());
	//convert it to bit vector
	x.resize(nEvents,false);
	unpackFromBytes(x, bytes);
}

POPINDICES * INTINDICES::clone(){

	INTINDICES * res=new INTINDICES(*this);
	return res;
}
void INTINDICES::convertToPb(pb::POPINDICES & ind_pb){
	ind_pb.set_indtype(pb::INT);
	BOOST_FOREACH(vector<unsigned>::value_type & it, x){
		ind_pb.add_iind(it);
	}
	ind_pb.set_nevents(nEvents);
}

INTINDICES::INTINDICES(const pb::POPINDICES & ind_pb){
	nEvents = ind_pb.nevents();
	unsigned nSize = ind_pb.iind_size();
	x = vector<unsigned>(nSize);
	for(unsigned i = 0; i < nSize; i++)
		x.at(i) = ind_pb.iind(i);
}

POPINDICES * ROOTINDICES::clone(){

	ROOTINDICES * res=new ROOTINDICES(*this);
	return res;
}
void ROOTINDICES::convertToPb(pb::POPINDICES & ind_pb){
	ind_pb.set_indtype(pb::ROOT);
	ind_pb.set_nevents(nEvents);
}
ROOTINDICES::ROOTINDICES(const pb::POPINDICES & ind_pb){
	nEvents = ind_pb.nevents();
}
