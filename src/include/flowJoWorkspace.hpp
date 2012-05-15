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



class flowJoWorkspace:public workspace{

public:
//	~flowJoWorkspace();
	 vector <string> getSampleID(unsigned short);

     wsRootNode getRoot(wsSampleNode sampleNode);
     wsPopNodeSet getSubPop(wsNode * node);
     nodeProperties * to_popNode(wsRootNode &);
     nodeProperties * to_popNode(wsPopNode &,bool isParseGate);
     string getSampleName(wsSampleNode &);
//     valarray<double> toArray(string sCalTable);
//     virtual string xPathSample(string sampleID)=0;

};





#endif /* FLOWJOWORKSPACE_HPP_ */
