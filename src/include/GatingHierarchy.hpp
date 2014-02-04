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

using namespace std;
typedef map<string,VertexID> VertexID_map;
typedef vector<VertexID> VertexID_vec;
typedef vector<string> StringVec;
typedef vector<double> DoubleVec;
typedef vector<bool> BoolVec;


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
    	if(!u==0)
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

	bool isLoaded;

	PARAM_VEC transFlag;
	trans_local trans;
	template<class Archive>
				    void save(Archive &ar, const unsigned int version) const
				    {
						ar & BOOST_SERIALIZATION_NVP(comp);
						ar & BOOST_SERIALIZATION_NVP(fdata);
						ar & BOOST_SERIALIZATION_NVP(tree);

				        ar & BOOST_SERIALIZATION_NVP(isLoaded);

				        ar.register_type(static_cast<biexpTrans *>(NULL));
						ar.register_type(static_cast<flinTrans *>(NULL));
						ar.register_type(static_cast<logTrans *>(NULL));
						ar.register_type(static_cast<linTrans *>(NULL));

				        ar & BOOST_SERIALIZATION_NVP(transFlag);


				        ar & BOOST_SERIALIZATION_NVP(trans);
				        ar & BOOST_SERIALIZATION_NVP(dMode);
				    }
	template<class Archive>
			void load(Archive &ar, const unsigned int version)
			{

				ar & BOOST_SERIALIZATION_NVP(comp);
				ar & BOOST_SERIALIZATION_NVP(fdata);
				if(version < 2){
					//load the old structure
					populationTreeOld treeOld;
					ar & BOOST_SERIALIZATION_NVP(treeOld);
					//copy the graph structure without property
					tree=populationTree(num_vertices(treeOld));//copy vertex
					//copy edges
					typedef boost::graph_traits<populationTreeOld>::edge_iterator myEdgeIt;
					myEdgeIt first, last;

					for (tie(first, last) = edges(treeOld);first != last; ++first)
					{
						EdgeID e = *first;
						VertexID s=source(e,treeOld);
						VertexID t=target(e,treeOld);
						boost::add_edge(s,t,tree);
					}


					//copy properties manually
					boost::graph_traits<populationTree>::vertex_iterator v_start, v_end;

					for (tie(v_start, v_end) = vertices(treeOld);
							v_start != v_end; ++v_start)
					{
						VertexID thisV = *v_start;
						tree[thisV] = *(treeOld[thisV]);
					}
				}
				else
					ar & BOOST_SERIALIZATION_NVP(tree);

				if(version==0){
					bool isGated=false;
					ar & BOOST_SERIALIZATION_NVP(isGated);
				}


		        ar & BOOST_SERIALIZATION_NVP(isLoaded);

		        ar.register_type(static_cast<biexpTrans *>(NULL));
				ar.register_type(static_cast<flinTrans *>(NULL));
				ar.register_type(static_cast<logTrans *>(NULL));
				ar.register_type(static_cast<linTrans *>(NULL));
				if(version==0){
					trans_global_vec *gTrans;
					ar & BOOST_SERIALIZATION_NVP(gTrans);
				}


		        ar & BOOST_SERIALIZATION_NVP(transFlag);


		        ar & BOOST_SERIALIZATION_NVP(trans);
		        ar & BOOST_SERIALIZATION_NVP(dMode);
		    }
BOOST_SERIALIZATION_SPLIT_MEMBER()

public:

	unsigned short dMode;//debug mode passed from GatingSet


	/*remove the gate from */
//	void removeGate(unsigned short popId);
//	void removeGate(string popName);

	/*append the gate to the tree*/
	void addChild(VertexID parent,VertexID child);
	VertexID addGate(gate* g,VertexID parentID,string popName);
	void removeNode(VertexID nodeID);
	void addPopulation(VertexID parentID,workspace & ws,wsNode * parentNode,bool isGating);
	VertexID addRoot(wsRootNode, workspace & ws);
	VertexID addRoot();
	GatingHierarchy();
//	~GatingHierarchy();

	GatingHierarchy(wsSampleNode curSampleNode,workspace & ws,bool isGating,trans_global_vec * _gTrans,biexpTrans * _globalBiExpTrans,linTrans * _globalLinTrans,unsigned short dMode);


	flowData getData(VertexID nodeID);//from memory
//	flowData getData(string string,VertexID nodeID);//from cdf
//	void loadData(string);
	void loadData(const flowData &);

	void unloadData();


	compensation getCompensation();
	trans_local getLocalTrans(){return trans;}
	void printLocalTrans();//for the debugging purpose
	void transforming();
	void gating(VertexID,bool recompute=false);
	void calgate(VertexID);
	vector<bool> boolGating(VertexID);
	void extendGate(float);
	void adjustGate(map<string,float> & gains);
	void drawGraph(string out);
	VertexID getChildren(VertexID source,string childName);
	VertexID getNodeID(vector<string> gatePath);
	VertexID getNodeID(VertexID u,string popName);
	VertexID_vec getVertices(unsigned short order=0);//return the node list in vertexID order or T order
	vector<string> getPopNames(unsigned short order,bool isPath,bool showHidden);
	VertexID getAncestor(VertexID u,unsigned short level);
	EdgeID getInEdges(VertexID target);
	VertexID getParent(VertexID);
	VertexID getDescendant(VertexID u,string name);
	VertexID_vec getChildren(VertexID);
	nodeProperties & getNodeProperty(VertexID);

	GatingHierarchy * clone(const trans_map & _trans,trans_global_vec * _gTrans);
	GatingHierarchy * clone();
};
BOOST_CLASS_VERSION(GatingHierarchy,2)


#endif /* GATINGHIERARCHY_HPP_ */
