/*
 * compensation.hpp
 *
 *  Created on: Apr 8, 2016
 *      Author: wjiang2
 */

#ifndef INCLUDE_COMPENSATION_HPP_
#define INCLUDE_COMPENSATION_HPP_
#include "transformation.hpp"
class compensation{
public:
	string cid;
	string prefix;
	string suffix;
	string name;
	string comment;// store "Acquisition-defined" when the spillOver matrix is not supplied and cid==-1
	vector<string> marker;
	vector<double> spillOver;
	void convertToPb(pb::COMP & comp_pb);
	compensation(){};
	void updateChannels(const CHANNEL_MAP & chnl_map);
	compensation(const pb::COMP & comp_pb);
};


#endif /* INCLUDE_COMPENSATION_HPP_ */
