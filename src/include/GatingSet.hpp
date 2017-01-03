/*
 * GatingSet.hpp
 *
 *  Created on: Mar 15, 2012
 *      Author: wjiang2
 */



#ifndef GATINGSET_HPP_
#define GATINGSET_HPP_
#include "GatingHierarchy.hpp"
#include "macFlowJoWorkspace.hpp"
#include "winFlowJoWorkspace.hpp"
#include <string>
#include <map>

using namespace std;

#define ARCHIVE_TYPE_BINARY 0
#define ARCHIVE_TYPE_TEXT 1
#define ARCHIVE_TYPE_XML 2
#define PB true
#define BS false

#define WS_WIN 1
#define WS_MAC 2
#define WS_VX 3
#define WS_MAC_3 4

 /*
 * have to use pointer GatingHierarchy * here,
 * because GatingHierarchy's destructor is responsible for clearing dynamically allocated memory
 * within GatingHierarchy,like nodeProperties * that further contains the gate *.
 * if don't use GatingHierarchy *,then these children pointers have to be taken care of outside of
 * GatingHierarchy class,which could be problematic.
 *
 */
typedef map<string,GatingHierarchy*> gh_map;

/**
 * \class GatingSet
 * \brief A container class that stores multiple GatingHierarchy objects.
 *
 *  It also stores the global transformations.
 *
 */
class GatingSet{

	biexpTrans globalBiExpTrans; //default bi-exponential transformation functions
	linTrans globalLinTrans;
	trans_global_vec gTrans;//parsed from xml workspace
	gh_map ghs;  /**< the map stores pairs of sample name and GatingHierarchy pointer */
	workspace * wsPtr;

private:

	void freeWorkspace();//this is private because it is not supposed to be called anywhere other than destructor
public:
	~GatingSet();
	GatingSet(){wsPtr=NULL;};
	void setSample(string oldName, string newName);
	GatingSet(string,bool,unsigned short,int,unsigned short wsType);
	GatingSet(GatingHierarchy *,vector<string>);
	GatingSet(vector<string>);
	GatingSet(string filename);
	GatingHierarchy * getGatingHierarchy(string );
	GatingHierarchy * getGatingHierarchy(unsigned int);
	void gating();
	void parseWorkspace(unsigned short,bool, StringVec sampleNames);
	void parseWorkspace(vector<string>,bool, StringVec sampleNames);
	vector<string> getSamples(void);

	GatingSet * clone(vector<string> samples);

	void serialize_pb(string filename);
	void add(GatingSet & gs,vector<string> sampleNames);
	void addTransMap(string gName,trans_map tm);
	void convertToPb(pb::GatingSet & gs_pb,string path);
	void updateChannels(const CHANNEL_MAP & chnl_map);
	void set_gTrans(const trans_global_vec & _gTrans){gTrans = _gTrans;};
};



#endif /* GATINGSET_HPP_ */
