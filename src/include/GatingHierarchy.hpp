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
using namespace std;
typedef map<string,VertexID> VertexID_map;
typedef vector<VertexID> VertexID_vec;

struct OurVertexPropertyWriter {

	OurVertexPropertyWriter(populationTree &g_) : g(g_) {}

    template <class Vertex>
    void operator() (std::ostream &out, Vertex u) {

    	out<<"[shape=record,label=\"{"<<g[u].getName()<<"|count:"<<g[u].fjStats["count"]<<"}\"]";


    }

    populationTree &g;
};

struct OurVertexPropertyWriterR {

	OurVertexPropertyWriterR(populationTree &g_) : g(g_) {}

    template <class Vertex>
    void operator() (std::ostream &out, Vertex u) {

    	out<<"[shape=record,label=\""<<g[u]->getName()<<"\"]";


    }

    populationTree &g;
};

class transformation{

};

class compensation{

};


/*GatingHierarchy is a tree that holds the gate definitions hierarchically,
 along with the transformation functions and compensation matrix,
 Once the one FCS file is associated,the tree can also hold indices that subset the events
 It can serves as a gating template when data is empty
 */

class GatingHierarchy{
//	transformation trans;
//	compensation comp;
	ncdfFlow *nc;//a pointer to the global cdf data stored within gatingSet
	flowData data;
	string sampleName;
	populationTree tree;
	bool isGated;
	/*
	 * this field is for the easy query by node name
	 * since boost does not provide this query feature explicitly,
	 * however it adds the potential data inconsistency risk when performing the non-read-only
	 * operation on the tree
	 */
//	VertexID_map nodelist;
	workspace * thisWs;
public:
	unsigned short dMode;//debug mode passed from GatingSet

	/*retrieve the gate definition from a particular node*/
//	gate getGate(unsigned short gateid);
//	gate getGate(string popName);

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
//	GatingHierarchy(string sampleID,workspace * ws,unsigned short _dMode);
	GatingHierarchy(wsSampleNode curSampleNode,workspace * ws,bool isGating,ncdfFlow *,unsigned short _dMode);

	/*associate the tree with data matrix*/
//	void addData();

	/**/
//	vector<bool> * getIndice(string popName);

	flowData getData(VertexID nodeID);
	void loadData();
//	void gating(gate& g,string popName);

	void gating();
//	void gating(VertexID);
	void drawGraph(string out);
	string getSample(void){return sampleName;};
	void setSample(string _sampleName){sampleName=_sampleName;};
	VertexID_vec getVertices(bool tsort);//return the node list in vertexID order or T order
	vector<string> getPopNames(bool tsort,bool isPath);
	VertexID_vec getParent(VertexID);
	VertexID_vec getChildren(VertexID);
	nodeProperties * getNodeProperty(VertexID);
};



#endif /* GATINGHIERARCHY_HPP_ */
