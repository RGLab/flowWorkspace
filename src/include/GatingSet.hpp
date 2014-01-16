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
#include <boost/foreach.hpp>




using namespace std;

#define ARCHIVE_TYPE_BINARY 0
#define ARCHIVE_TYPE_TEXT 1
#define ARCHIVE_TYPE_XML 2

/*
 * have to use pointer GatingHierarchy * here,
 * because GatingHierarchy's destructor is responsible for clearing dynamically allocated memory
 * within GatingHierarchy,like nodeProperties * that further contains the gate *.
 * if don't use GatingHierarchy *,then these children pointers have to be taken care of outside of
 * GatingHierarchy class,which could be problematic.
 *
 */
typedef map<string,GatingHierarchy*> gh_map;

/*GatingSet is multiple GatingHierarchies that has the flow data associated and gated*/
class GatingSet{

	friend class boost::serialization::access;
	biexpTrans globalBiExpTrans; //default bi-exponential transformation functions
	linTrans globalLinTrans;
	trans_global_vec gTrans;//parsed from xml workspace
	gh_map ghs;
	unsigned short dMode;//debug level to control print out
	workspace * wsPtr;

private:
	template<class Archive>
		void save(Archive & ar, const unsigned int version) const
			{




				ar.register_type(static_cast<biexpTrans *>(NULL));
				ar.register_type(static_cast<flinTrans *>(NULL));
				ar.register_type(static_cast<logTrans *>(NULL));
				ar.register_type(static_cast<linTrans *>(NULL));
				ar & BOOST_SERIALIZATION_NVP(globalBiExpTrans);
				ar & BOOST_SERIALIZATION_NVP(globalLinTrans);
				ar & BOOST_SERIALIZATION_NVP(gTrans);
	//			ar & nc;
				ar & BOOST_SERIALIZATION_NVP(ghs);

				ar & BOOST_SERIALIZATION_NVP(dMode);

	//	        ar.register_type(static_cast<flowJoWorkspace *>(NULL));
	//			ar & ws;
	    }
	template<class Archive>
		void load(Archive & ar, const unsigned int version) {
				ar.register_type(static_cast<biexpTrans *>(NULL));
				ar.register_type(static_cast<flinTrans *>(NULL));
				ar.register_type(static_cast<logTrans *>(NULL));
				ar.register_type(static_cast<linTrans *>(NULL));
				if(version>0){
					ar & BOOST_SERIALIZATION_NVP(globalBiExpTrans);
					ar & BOOST_SERIALIZATION_NVP(globalLinTrans);
				}


				ar & BOOST_SERIALIZATION_NVP(gTrans);
				ar & BOOST_SERIALIZATION_NVP(ghs);
				ar & BOOST_SERIALIZATION_NVP(dMode);

		}
	BOOST_SERIALIZATION_SPLIT_MEMBER()

	void freeWorkspace();//this is private because it is not supposed to be called anywhere other than destructor
public:
	~GatingSet();
	GatingSet(){wsPtr=NULL;};
	void setSample(string oldName, string newName);
	GatingSet(string,bool,unsigned short,int,unsigned short _dMode=1);
	GatingSet(GatingHierarchy *,vector<string>,unsigned short _dMode=1);
	GatingSet(vector<string>,unsigned short _dMode=1);
	GatingHierarchy * getGatingHierarchy(string );
	GatingHierarchy * getGatingHierarchy(unsigned int);
	void gating();
	void parseWorkspace(unsigned short,bool);
	void parseWorkspace(vector<string>,bool);
	vector<string> getSamples(void);

	GatingSet * clone_treeOnly(vector<string> samples);
//	GatingSet * clone_sstream(vector<string> samples);
//	GatingSet * clone_fstream(vector<string> samples);
	void add(GatingSet & gs,vector<string> sampleNames,unsigned short _dMode=1);
};

BOOST_CLASS_VERSION(GatingSet,1)

void save_gs(const GatingSet &gs,string filename, unsigned short format);
void restore_gs(GatingSet &s, string filename, unsigned short format);

#endif /* GATINGSET_HPP_ */
