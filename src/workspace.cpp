/*
 * workspace.cpp
 *
 *  Created on: Mar 22, 2012
 *      Author: wjiang2
 */




#include "include/workspace.hpp"
#include <iostream>
#include <sstream>
workspace::~workspace(){

//	COUT<<"entring the destructor of workspace"<<endl;

	 /*free the document */
	if(doc!=NULL)
	{
		xmlFreeDoc(doc);
		doc = NULL;

		/*
		 *Free the global variables that may
		 *have been allocated by the parser.
		 */
		xmlCleanupParser();
		if(g_loglevel>=GATING_SET_LEVEL)
			COUT<<"xml freed!"<<endl;
	}
}


valarray<double> workspace::toArray(string sCalTable){


//	sCalTable="3980,1.9428805e5,3981,1.9478264e5,3982,1.9527849e5,3983,1.9577559e5,3984,1.9627398e5";
	vector<string> stringVec;
	boost::split(stringVec,sCalTable,boost::is_any_of(","));
//

	valarray<double> res(stringVec.size());
	for(unsigned i=0;i<stringVec.size();i++)
	{
		res[i]=atof(stringVec.at(i).c_str());
//		COUT<<res[i]<<",";
	}
	return res;
}

void compensation::updateChannels(const CHANNEL_MAP & chnl_map){

	for(vector<string>::iterator it = marker.begin(); it != marker.end(); it++)
	{
		string curName = *it;

		CHANNEL_MAP::const_iterator itChnl = chnl_map.find(curName);
		if(itChnl!=chnl_map.end())
			*it = itChnl->second;
	}
}
void compensation::convertToPb(pb::COMP & comp_pb){
	comp_pb.set_cid(cid);
	comp_pb.set_name(name);
	comp_pb.set_prefix(prefix);
	comp_pb.set_suffix(suffix);
	comp_pb.set_comment(comment);
	BOOST_FOREACH(vector<double>::value_type & it, spillOver){
		comp_pb.add_spillover(it);
	}
	BOOST_FOREACH(vector<string>::value_type & it, marker){
			comp_pb.add_marker(it);
		}
}
compensation::compensation(const pb::COMP & comp_pb):cid(comp_pb.cid()),prefix(comp_pb.prefix()),suffix(comp_pb.suffix()),name(comp_pb.name()),comment(comp_pb.comment()){
	for(int i = 0; i < comp_pb.marker_size(); i++)
		marker.push_back(comp_pb.marker(i));
	for(int i = 0; i < comp_pb.spillover_size(); i++)
			spillOver.push_back(comp_pb.spillover(i));
}
