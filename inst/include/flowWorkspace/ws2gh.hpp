/*
 * ws2gh.hpp
 *
 *  Created on: Aug 8, 2017
 *      Author: wjiang2
 */

#ifndef INCLUDE_WS2GH_HPP_
#define INCLUDE_WS2GH_HPP_

#include <cytolib/GatingSet.hpp>
#include "macFlowJoWorkspace.hpp"
#include "winFlowJoWorkspace.hpp"


VertexID addRoot(populationTree &tree, wsRootNode root, workspace & ws);
void addPopulation(populationTree &tree, VertexID parentID,workspace & ws,wsNode * parentNode,bool isParseGate);
GatingHierarchy * ws2gh(wsSampleNode curSampleNode,workspace & ws,bool isParseGate,trans_global_vec * _gTrans,biexpTrans * _globalBiExpTrans,linTrans * _globalLinTrans);

#endif /* INCLUDE_WS2GH_HPP_ */
