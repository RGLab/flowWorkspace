/*
 * GatingHierarchy.cpp
 *
 *  Created on: Mar 20, 2012
 *      Author: wjiang2
 */

#include <GatingHierarchy.hpp>
#include <boost/graph/graphviz.hpp>
#include <fstream>

/*need to be careful that gate within each node of the GatingHierarchy is
 * dynamically allocated,even it GatingHierarchy gets copied before destroyed
 * these gates are already gone since the gate objects were already freed by
 * this destructor
 */
//TODO:try to free the gates
GatingHierarchy::~GatingHierarchy()
{
	//free the gate objects witin each node
//	boost::vertices(tree).first()


}

//default constructor without argument
GatingHierarchy::GatingHierarchy()
{

}
//constructor for sampleNode argument
//GatingHierarchy::GatingHierarchy(string sampleID,workspace * ws)
//{
//	thisWs=ws;
//
//	wsSampleNode curSampleNode=thisWs->getSample(sampleID);
//	wsRootNode root=thisWs->getRoot(curSampleNode);
//	VertexID pVerID=addRoot(thisWs->to_popNode(&root));
////	wsRootNode popNode=root;//getPopulation();
//	addPopulation(pVerID,&root);
//
//}
GatingHierarchy::GatingHierarchy(wsSampleNode curSampleNode,workspace * ws)
{
	thisWs=ws;
	wsRootNode root=thisWs->getRoot(curSampleNode);
	VertexID pVerID=addRoot(thisWs->to_popNode(root));
//	wsRootNode popNode=root;//getPopulation();
	addPopulation(pVerID,&root);

}
VertexID GatingHierarchy::addRoot(populationNode rootNode)
{
	// Create  vertices in that graph
	VertexID u = boost::add_vertex(tree);

	tree[u]=rootNode;
	nodelist[rootNode.getName()]=u;

	return(u);
}

void GatingHierarchy::addPopulation(VertexID parentID,wsNode * parentNode)
{


	wsPopNodeSet children =thisWs->getSubPop(parentNode);
	wsPopNodeSet::iterator it;
		for(it=children.begin();it!=children.end();it++)
		{
			//add boost node
			VertexID curChildID = boost::add_vertex(tree);
			wsPopNode curChildNode=(*it);
			//convert to the node format that GatingHierarchy understands
			populationNode curChild=thisWs->to_popNode(curChildNode);
			cout<<"node created:"<<curChild.getName()<<endl;
			//attach the populationNode to the boost node as property
			tree[curChildID]=curChild;
			//add relation between current node and parent node
			boost::add_edge(parentID,curChildID,tree);
			//update the node map for the easy query by pop name
			nodelist[curChild.getName()]=curChildID;
			//recursively add its descendants
			addPopulation(curChildID,&curChildNode);
		}


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

string GatingHierarchy::drawGraph()
{
	ofstream outputFile("test.dot");
	//...
	boost::write_graphviz(outputFile,tree,OurVertexPropertyWriter(tree));
	outputFile.close();
	system("dot2gxl test.dot -o test.gxl");
	return("test.gxl");
//	system("pwd");


}
