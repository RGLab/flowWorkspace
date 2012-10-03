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
#include "ncdfFlow.hpp"
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

using namespace std;
typedef map<string,VertexID> VertexID_map;
typedef vector<VertexID> VertexID_vec;



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
    	nodeProperties *curNode=g[u];
    	bool isBoolGate=false;
    	if(!u==0)
    	{
    		unsigned short gateType=curNode->getGate()->getType();
    		isBoolGate=(gateType==BOOLGATE);
    	}
    	out<<"[shape=record,label=\""<<curNode->getName()<<"\",isBool="<<isBoolGate<<"]";


    }

    populationTree &g;
};



/*GatingHierarchy is a tree that holds the gate definitions hierarchically,
 along with the transformation functions and compensation matrix,
 Once the one FCS file is associated,the tree can also hold indices that subset the events
 It can serves as a gating template when data is empty
 */

class GatingHierarchy{
//	friend std::ostream & operator<<(std::ostream &os, const GatingHierarchy &gh);
	friend class boost::serialization::access;
private:
	compensation comp;/*compensation is currently done in R due to the linear Algebra
						e[, cols] <- t(solve(t(spillover))%*%t(e[,cols]))
						we can try uBlas for this simple task, but when cid=="-1",we still need to
						do this in R since comp is extracted from FCS keyword (unless it can be optionally extracted from workspace keyword)
	 	 	 	 	  */
	flowData fdata;
	populationTree tree;
	bool isGated;
	bool isLoaded;
	/*
	 * Deprecated: we don't want to keep a separate view of ncdfFlowSet in c++
	 */
	ncdfFlow *nc;//a pointer to the global cdf data stored within gatingSet
	workspace * thisWs;

	trans_global_vec *gTrans;//pointer to the global trans stored in gs
	PARAM_VEC transFlag;
	trans_local trans;

	template<class Archive>
		    void serialize(Archive &ar, const unsigned int version)
		    {

				ar & comp;
				ar & fdata;
				ar & tree;
		        ar & isGated;
		        ar & isLoaded;
//		        ar & nc;

//		        ar.register_type(static_cast<flowJoWorkspace *>(NULL));
//		        ar & thisWs;

		        ar.register_type(static_cast<biexpTrans *>(NULL));
				ar.register_type(static_cast<logicleTrans *>(NULL));
				ar.register_type(static_cast<logTrans *>(NULL));
				ar.register_type(static_cast<linTrans *>(NULL));
		        ar & gTrans;

		        ar & transFlag;


		        ar & trans;
		        ar & dMode;
		    }
public:

	unsigned short dMode;//debug mode passed from GatingSet


	/*remove the gate from */
//	void removeGate(unsigned short popId);
//	void removeGate(string popName);

	/*append the gate to the tree*/
	void addChild(VertexID parent,VertexID child);
	void addGate(gate& g,string popName);
	void addPopulation(VertexID parentID,wsNode * parentNode,bool isGating);
	VertexID addRoot(nodeProperties* rootNode);
	GatingHierarchy();
	~GatingHierarchy();

	GatingHierarchy(wsSampleNode curSampleNode,workspace * ws,bool isGating,ncdfFlow *,trans_global_vec * _gTrans,unsigned short dMode);


	flowData getData(VertexID nodeID);//from memory
	flowData getData(string string,VertexID nodeID);//from cdf
	void loadData(string);
	void loadData(const flowData &);

	void unloadData();


	compensation getCompensation();
	trans_local getLocalTrans(){return trans;}
	void printLocalTrans();//for the debugging purpose
	void transforming(bool);
	void gating(VertexID,bool recompute=false);
	void calgate(VertexID);
	POPINDICES boolGating(VertexID);
	void extendGate();
	void drawGraph(string out);
	VertexID getChildren(VertexID source,string childName);
	VertexID getNodeID(vector<string> gatePath);
	VertexID getNodeID(VertexID u,string popName);
	VertexID_vec getVertices(unsigned short order=0);//return the node list in vertexID order or T order
	vector<string> getPopNames(unsigned short order,bool isPath);
	VertexID getAncestor(VertexID u,unsigned short level);
	VertexID_vec getParent(VertexID);
	VertexID getDescendant(VertexID u,string name);
	VertexID_vec getChildren(VertexID);
	nodeProperties * getNodeProperty(VertexID);
	void setNcPtr(ncdfFlow *_nc){nc=_nc;}
	GatingHierarchy * clone(const trans_map & _trans,trans_global_vec * _gTrans);
	GatingHierarchy * clone();
};



#endif /* GATINGHIERARCHY_HPP_ */
