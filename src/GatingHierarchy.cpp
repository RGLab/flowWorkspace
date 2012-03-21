/*
 * GatingHierarchy.cpp
 *
 *  Created on: Mar 20, 2012
 *      Author: wjiang2
 */

#include "GatingHierarchy.hpp"
//#include "boost/"

//default constructor without argument
GatingHierarchy::GatingHierarchy()
{

}
//constructor for sampleNode argument
GatingHierarchy::GatingHierarchy(xmlChar * sampleID,flowJoWorkspace &ws)
{
	xmlNodePtr curSample=ws.getSample(sampleID);
	xmlNodePtr curSampleNode=ws.getSampleNode(curSample);
	addRoot(curSampleNode);

//	addPopulation();
}
void GatingHierarchy::addRoot(xmlNodePtr rootNode)
{
	typedef boost::graph_traits<populationTree>::vertex_descriptor vertex_t;

	// Create  vertices in that graph
	vertex_t u = boost::add_vertex(tree);

	xmlChar * popName=xmlGetProp(rootNode,(const xmlChar*)"name");
	tree[u].setName((const char *)popName);
	xmlFree(popName);
			//	rootcount<-xmlGetAttr(x,"count")###
			//Fix a bug here. If "count" is empty or doesn't exist, need to look at the parent Sample xml node and get the eventCount property
			//		if(is.null(rootcount)){
			//		    rootcount<-xpathApply(x,"./ancestor::Sample",function(x)xmlGetAttr(x,"eventCount"))[[1]]
			//		}
}
void GatingHierarchy::addPopulation(xmlXPathObjectPtr popNode)
{

}
void GatingHierarchy::addGate(gate& g,string popName)
{

	typedef boost::graph_traits<populationTree>::vertex_descriptor vertex_t;

	// Create  vertices in that graph
//	vertex_t u = boost::add_vertex(tree);


//	vertex_t v = boost::add_vertex(g);

	// Create an edge conecting those two vertices
//	edge_t e; bool b;
//	boost::tie(e,b) = boost::add_edge(u,v,g);

//	boost::add_edge()
}

void GatingHierarchy::gating()
{
	cout <<"test gating"<<endl;
}
