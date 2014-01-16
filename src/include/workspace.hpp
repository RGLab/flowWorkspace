/*
 * workspace.hpp
 *
 *  Created on: Mar 22, 2012
 *      Author: wjiang2
 */

#ifndef WORKSPACE_HPP_
#define WORKSPACE_HPP_
#include <vector>
#include <string>
#include <libxml/xpath.h>
#include <libxml/parser.h>
#include "wsNode.hpp"
#include "transformation.hpp"
#include <iostream>
#include <algorithm>
#include <math.h>
#include <fstream>


using namespace std;


using namespace std;
/*TODO: so far I have seen the major difference between win and mac workspace is the xpath(like xpath of sample node)
 * if this is the case eventually we can try to use one template class (eliminate two derived classes )
 * with T structure that stores different versions of xpaths for win/mac,for example:
 *
 * struct winWorkspace{
 * xpath_sample=xxx
 * ....
 * }
 *
 * struct macWorkspace{
 * xpath_sample=xxx
 * ....
 * }
 *
 * this may potentially reduce the amount of code
 *
 */
class compensation{
//	friend std::ostream & operator<<(std::ostream &os, const compensation &gh);
	friend class boost::serialization::access;

public:
	string cid;
	string prefix;
	string suffix;
	string name;
	string comment;// store "Acquisition-defined" when the spillOver matrix is not supplied and cid==-1
	vector<string> marker;
	vector<double> spillOver;
private:
template<class Archive>
				void serialize(Archive &ar, const unsigned int version)
				{
					ar & BOOST_SERIALIZATION_NVP(cid);
					ar & BOOST_SERIALIZATION_NVP(prefix);
					ar & BOOST_SERIALIZATION_NVP(suffix);
					ar & BOOST_SERIALIZATION_NVP(name);
					ar & BOOST_SERIALIZATION_NVP(comment);
					ar & BOOST_SERIALIZATION_NVP(marker);
					ar & BOOST_SERIALIZATION_NVP(spillOver);
				}
};


struct xpath{
	string group;
	string sampleRef;
	string sample;
	string sampleNode;
	string popNode;
	string gateDim;
	string gateParam;
	unsigned short sampNloc;//get FCS filename(or sampleName) from either $FIL keyword or name attribute of sampleNode
	template<class Archive>
		void serialize(Archive &ar, const unsigned int version)
		{


			ar & BOOST_SERIALIZATION_NVP(group);
			ar & BOOST_SERIALIZATION_NVP(sampleRef);
			ar & BOOST_SERIALIZATION_NVP(sample);
			ar & BOOST_SERIALIZATION_NVP(sampleNode);
			ar & BOOST_SERIALIZATION_NVP(popNode);
			ar & BOOST_SERIALIZATION_NVP(sampNloc);
		}
};



class workspace{
//	friend std::ostream & operator<<(std::ostream &os, const workspace &gh);
	friend class boost::serialization::access;
public:
	 xpath nodePath;
//protected:

	 xmlDoc * doc;
	 unsigned short dMode;//debug mode passed from gatingset class
private:
	 template<class Archive>
	 		    void serialize(Archive &ar, const unsigned int version)
	 		    {
	 				ar & BOOST_SERIALIZATION_NVP(nodePath);
	 				ar & BOOST_SERIALIZATION_NVP(doc);
	 				ar & BOOST_SERIALIZATION_NVP(dMode);
	 		    }
public:
	 workspace(){doc=NULL;};
	 virtual ~workspace();
	 virtual string xPathSample(string sampleID)=0;
	 virtual PARAM_VEC getTransFlag(wsSampleNode sampleNode)=0;
	 virtual trans_local getTransformation(wsRootNode,const compensation &,PARAM_VEC &,trans_global_vec *,biexpTrans * _globalBiExpTrans,linTrans * _globalLinTrans)=0;
	 virtual compensation getCompensation(wsSampleNode)=0;
	 virtual trans_global_vec getGlobalTrans()=0;
	 virtual vector <string> getSampleID(unsigned short)=0;
	 virtual string getSampleName(wsSampleNode &)=0;
	 virtual wsRootNode getRoot(wsSampleNode sampleNode)=0;
	 virtual wsPopNodeSet getSubPop(wsNode *)=0;
	 virtual gate * getGate(wsPopNode &)=0;//gate is dynamically allocated within this function,it is currently freed within gate pointer owner object nodeProperties
	 virtual void to_popNode(wsRootNode &, nodeProperties &)=0;
	 virtual void to_popNode(wsPopNode &,nodeProperties &,bool isGating)=0;
	 valarray<double> toArray(string sCalTable);
	 virtual void parseVersionList(){};


};


#endif /* WORKSPACE_HPP_ */

