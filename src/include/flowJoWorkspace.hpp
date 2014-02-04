/*
 * flowJoWorkspace.hpp
 *
 *  Created on: Mar 15, 2012
 *      Author: wjiang2
 */

#ifndef FLOWJOWORKSPACE_HPP_
#define FLOWJOWORKSPACE_HPP_

#include "workspace.hpp"
#include "transformation.hpp"
#include <sstream>
#include <boost/lexical_cast.hpp>

class flowJoWorkspace:public workspace{
//	friend std::ostream & operator<<(std::ostream &os, const flowJoWorkspace &gh);
	friend class boost::serialization::access;
private:
	string versionList;

	template<class Archive>
		    void serialize(Archive &ar, const unsigned int version)
		    {
        		ar & boost::serialization::make_nvp("workspace",boost::serialization::base_object<workspace>(*this));
				ar & BOOST_SERIALIZATION_NVP(versionList);
		    }
public:

	 vector <string> getSampleID(unsigned short);
	 virtual PARAM_VEC getTransFlag(wsSampleNode sampleNode);
     wsRootNode getRoot(wsSampleNode sampleNode);
     wsPopNodeSet getSubPop(wsNode * node);
     void to_popNode(wsRootNode &, nodeProperties & np);
     void to_popNode(wsPopNode &, nodeProperties & np,bool isParseGate);
     string getSampleName(wsSampleNode &);
     void parseVersionList();
     unsigned short getVersionMin();

};





#endif /* FLOWJOWORKSPACE_HPP_ */
