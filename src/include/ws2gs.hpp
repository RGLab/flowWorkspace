/*
 * ws2gs.hpp
 *
 *  Created on: Aug 8, 2017
 *      Author: wjiang2
 */

#ifndef INCLUDE_WS2GS_HPP_
#define INCLUDE_WS2GS_HPP_

#include "ws2gh.hpp"

workspace * openWorkspace(string sFileName,unsigned short sampNloc,int xmlParserOption,unsigned short wsType);
GatingSet * ws2gs(workspace * ws, vector<string> sampleIDs,bool isParseGate, StringVec sampleNames);




#endif /* INCLUDE_WS2GS_HPP_ */
