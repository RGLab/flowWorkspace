/*
 * macFlowJoWorkspace.hpp
 *
 *  Created on: May 15, 2012
 *      Author: wjiang2
 */

#ifndef MACFLOWJOWORKSPACE_HPP_
#define MACFLOWJOWORKSPACE_HPP_
#include "flowJoWorkspace.hpp"

class macFlowJoWorkspace:public flowJoWorkspace{
friend class boost::serialization::access;

private:
	template<class Archive>
				void serialize(Archive &ar, const unsigned int version)
				{
					ar & boost::serialization::base_object<flowJoWorkspace>(*this);

				}
	vector<BOOL_GATE_OP> parseBooleanSpec(string specs,vector<string> gPaths);
public:
	macFlowJoWorkspace(xmlDoc *);
	compensation getCompensation(wsSampleNode sampleNode);
	PARAM_VEC getTransFlag(wsSampleNode sampleNode);
	trans_local getTransformation(wsRootNode,const compensation & comp, PARAM_VEC & transFlag,trans_global_vec*,biexpTrans * _globalBiExpTrans,linTrans * _globalLinTrans);
	trans_global_vec getGlobalTrans();
	string xPathSample(string sampleID);
	gate * getGate(wsPopNode &);
	polygonGate * getGate(wsPolyGateNode &);
	boolGate * getGate(wsBooleanGateNode &);
	ellipseGate * getGate(wsEllipseGateNode &);
	rangeGate * getGate(wsRangeGateNode &);
};

class macFlowJoWorkspace_3:public macFlowJoWorkspace{
	friend class boost::serialization::access;
private:
	template<class Archive>
				void serialize(Archive &ar, const unsigned int version)
				{
					ar & boost::serialization::base_object<macFlowJoWorkspace>(*this);

				}
public:
	macFlowJoWorkspace_3(xmlDoc * _doc);
};




#endif /* MACFLOWJOWORKSPACE_HPP_ */
