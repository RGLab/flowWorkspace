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
//	friend std::ostream & operator<<(std::ostream &os, const GatingSet &gs);
	friend class boost::serialization::access;

	trans_global_vec gTrans;
	gh_map ghs;
	ncdfFlow nc;
	unsigned short dMode;//debug level to control print out
	workspace * ws;

private:
	template<class Archive>
	    void serialize(Archive &ar, const unsigned int version)
	    {



			ar.register_type(static_cast<biexpTrans *>(NULL));
			ar.register_type(static_cast<logicleTrans *>(NULL));
			ar.register_type(static_cast<logTrans *>(NULL));
			ar.register_type(static_cast<linTrans *>(NULL));
			ar & gTrans;

			ar & nc;
			ar & ghs;

	        ar & dMode;

//	        ar.register_type(static_cast<flowJoWorkspace *>(NULL));
//			ar & ws;
	    }
public:
	~GatingSet();
	GatingSet(){ws=NULL;};
	GatingSet(string,bool,unsigned short,unsigned short);
	GatingSet(GatingHierarchy *,vector<string>,unsigned short);
	GatingHierarchy * getGatingHierarchy(string );
	GatingHierarchy * getGatingHierarchy(unsigned int);
	void gating();
	void parseWorkspace(unsigned short,bool);
	void parseWorkspace(vector<string>,bool);
	vector<string> getSamples(void);
	void attachData(string,vector<string>,vector<string>);
	ncdfFlow getNcObj(){return nc;}
	workspace const * getWorkspace(){return ws;}
	GatingSet * clone();

};
void save_gs(const GatingSet &gs,string filename);
void restore_gs(GatingSet &s, string filename);

#endif /* GATINGSET_HPP_ */
