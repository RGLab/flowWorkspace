/*
 * flowJoWorkspace.hpp
 *
 *  Created on: Mar 15, 2012
 *      Author: wjiang2
 */

#ifndef FLOWJOWORKSPACE_HPP_
#define FLOWJOWORKSPACE_HPP_

#include "workspace.hpp"
#include "cytolib/transformation.hpp"
#include <sstream>
#include <boost/lexical_cast.hpp>
#include <boost/tokenizer.hpp>

class flowJoWorkspace:public workspace{
private:
	string versionList;
public:
	 flowJoWorkspace(xmlDoc *);
	 vector <string> getSampleID(unsigned short);
	 virtual PARAM_VEC getTransFlag(wsSampleNode sampleNode);
     wsRootNode getRoot(wsSampleNode sampleNode);
     wsPopNodeSet getSubPop(wsNode * node);
     void to_popNode(wsRootNode &, nodeProperties & np);
     void to_popNode(wsPopNode &, nodeProperties & np,bool isParseGate);
     string getSampleName(wsSampleNode &);
     void parseVersionList();
     unsigned short getVersionMin();
     vector<BOOL_GATE_OP> parseBooleanSpec(string specs,vector<string> gPaths);
};





#endif /* FLOWJOWORKSPACE_HPP_ */
