/*
 * GatingHierarchy.cpp
 *
 *  Created on: Mar 20, 2012
 *      Author: wjiang2
 */

#include "GatingHierarchy.hpp"
#include <boost/graph/graphviz.hpp>
#include <fstream>
//default constructor without argument
GatingHierarchy::GatingHierarchy()
{

}
//constructor for sampleNode argument
GatingHierarchy::GatingHierarchy(xmlChar * sampleID,workspace &ws)
{
	xmlNodePtr curSampleNode=ws.getSampleNode(sampleID);
	wsRootNode root=ws.getRoot(curSampleNode);
	addRoot(root.to_popNode());

//	addPopulation(curSampleNode);

}
void GatingHierarchy::addRoot(populationNode rootNode)
{


	// Create  vertices in that graph
	VertexID u = boost::add_vertex(tree);
	nodelist[rootNode.getName()]=u;

	tree[u]=rootNode;

			//	rootcount<-xmlGetAttr(x,"count")###
			//Fix a bug here. If "count" is empty or doesn't exist, need to look at the parent Sample xml node and get the eventCount property
			//		if(is.null(rootcount)){
			//		    rootcount<-xpathApply(x,"./ancestor::Sample",function(x)xmlGetAttr(x,"eventCount"))[[1]]
			//		}
}
void GatingHierarchy::addPopulation(xmlNodePtr parentNode)
{

//	.nextPopulation<-function(x,level){
//			#Get all the population nodes one level below this one..
//			xpathApply(x,paste("./descendant::Population[count(ancestor-or-self::Population) = ",level+xpathApply(x,"count(ancestor-or-self::Population)"),"]",sep=""));
//		}
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

void GatingHierarchy::drawGraph()
{
//	ofstream outputFile("test.dot");
	//...
	boost::write_graphviz(cout,tree,OurVertexPropertyWriter(tree));
//	system("pwd");
//	system("dot2gxl test.dot -o test.gxl");

}
