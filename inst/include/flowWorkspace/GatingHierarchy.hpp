/*
 * GatingHierarchy.hpp
 *
 *  Created on: Mar 17, 2012
 *      Author: mike
 */

#ifndef GATINGHIERARCHY_HPP_
#define GATINGHIERARCHY_HPP_
#include <iostream>
#include <string>
#include <vector>
#include "populationTree.hpp"
#include "flowJoWorkspace.hpp"
#include <libxml/xpath.h>
#include <fstream>
#include <algorithm>
#include <boost/graph/graphviz.hpp>
#include <boost/graph/graph_traits.hpp>
#include <boost/graph/topological_sort.hpp>
#include <boost/graph/breadth_first_search.hpp>
#define REGULAR 0
#define TSORT 1
#define BFS 2
/*
 * because __SIZE_TYPE__ is long long unsigned int by gcc on win64 (mingw64)
 * we cast it to unsigned int before pass it to Rcpp::wrap to avoid error
 */
typedef unsigned int NODEID;

using namespace std;
typedef map<string,VertexID> VertexID_map;
typedef vector<VertexID> VertexID_vec;
typedef vector<string> StringVec;
typedef vector<double> DoubleVec;
typedef vector<bool> BoolVec;
typedef vector<NODEID> NODEID_vec;


//struct OurVertexPropertyWriter {
//
//	OurVertexPropertyWriter(populationTree &g_) : g(g_) {}
//
//    template <class Vertex>
//    void operator() (std::ostream &out, Vertex u) {
//
//    	out<<"[shape=record,label=\"{"<<g[u].getName()<<"|count:"<<g[u].fjStats["count"]<<"}\"]";
//
//
//    }
//
//    populationTree &g;
//};

struct OurVertexPropertyWriterR {

	OurVertexPropertyWriterR(populationTree &g_) : g(g_) {}

    template <class Vertex>
    void operator() (std::ostream &out, Vertex u) {
    	nodeProperties &curNode=g[u];
    	bool isBoolGate=false;
    	bool hidden = false;
    	if(u!=0)
    	{
    		unsigned short gateType=curNode.getGate()->getType();
    		isBoolGate=(gateType==BOOLGATE);
    		hidden=curNode.getHiddenFlag();
    	}
    	out<<"[shape=record,label=\""<<curNode.getName()<<"\",isBool="<<isBoolGate<<",hidden="<<hidden<<"]";


    }

    populationTree &g;
};



/*GatingHierarchy is a tree that holds the gate definitions hierarchically,
 along with the transformation functions and compensation matrix,
 Once the one FCS file is associated,the tree can also hold indices that subset the events
 It can serves as a gating template when data is empty
 */

class GatingHierarchy{
public:
	compensation comp;/*compensation is currently done in R due to the linear Algebra
						e[, cols] <- t(solve(t(spillover))%*%t(e[,cols]))
						we can try uBlas for this simple task, but when cid=="-1",we still need to
						do this in R since comp is extracted from FCS keyword (unless it can be optionally extracted from workspace keyword)
	 	 	 	 	  */
	flowData fdata;
	populationTree tree;

	bool isLoaded;
	PARAM_VEC transFlag;
	trans_local trans;
public:

	/*append the gate to the tree*/
	void addChild(VertexID parent,VertexID child);
	VertexID addGate(gate* g,VertexID parentID,string popName);
	void removeNode(VertexID nodeID);
	void addPopulation(VertexID parentID,workspace & ws,wsNode * parentNode,bool isGating);
	VertexID addRoot(wsRootNode, workspace & ws);
	VertexID addRoot();
	GatingHierarchy();
	GatingHierarchy(pb::GatingHierarchy & pb_gh, map<intptr_t, transformation *>& trans_tbl);

	GatingHierarchy(wsSampleNode curSampleNode,workspace & ws,bool isGating,trans_global_vec * _gTrans,biexpTrans * _globalBiExpTrans,linTrans * _globalLinTrans);


	flowData getData(VertexID nodeID);//from memory
//	flowData getData(string string,VertexID nodeID);//from cdf
//	void loadData(string);
	void loadData(const flowData &);

	void unloadData();


	compensation getCompensation();
	void updateChannels(const CHANNEL_MAP & chnl_map);
	trans_local getLocalTrans(){return trans;}
	void printLocalTrans();//for the debugging purpose
	void transforming(double timestep);
	void gating(VertexID,bool recompute=false, bool computeTerminalBool=true);
	void calgate(VertexID, bool computeTerminalBool=true);
	vector<bool> boolGating(VertexID, bool computeTerminalBool);
	vector<bool> boolGating(vector<BOOL_GATE_OP> boolOpSpec, bool computeTerminalBool);
	void extendGate(float);
	void extendGate(float,float);

	void transformGate();
	void adjustGate(map<string,float> & gains);
	void drawGraph(string out);
	int getChildren(VertexID source,string childName);

	VertexID getCommonAncestor(VertexID_vec ,unsigned & nDepths);
	unsigned getNodeDepths(VertexID);
	VertexID getNodeID(vector<string> gatePath);
	VertexID getRefNodeID(VertexID u,vector<string> refPath);
	VertexID_vec queryByPath(VertexID ancestorID,vector<string> gatePath);
	VertexID getNodeID(string gatePath);
	VertexID getDescendant(VertexID u,string popName);

	VertexID_vec getVertices(unsigned short order=0);//return the node list in vertexID order or T order
	vector<string> getPopPaths(unsigned short order,bool fullPath,bool showHidden);
	VertexID getAncestor(VertexID u,unsigned short level);
	EdgeID getInEdges(VertexID target);
	VertexID getParent(VertexID);
	VertexID_vec getDescendants(VertexID u,string name);
	VertexID_vec getChildren(VertexID);
	nodeProperties & getNodeProperty(VertexID);

	GatingHierarchy * clone(const trans_map & _trans,trans_global_vec * _gTrans);
	GatingHierarchy * clone();
	void addTransMap(trans_map tm);
	void convertToPb(pb::GatingHierarchy & gh_pb);
};

#endif /* GATINGHIERARCHY_HPP_ */
