/*
 * code originally from GatingHierarchy.cpp
 *
 *  Created on: Mar 19, 2012
 *      Author: wjiang2
 */
#include "include/ws2gh.hpp"

/*
 * add root node first before recursively add the other nodes
 * since root node does not have gates as the others do
 */
VertexID addRoot(populationTree &tree, wsRootNode root, workspace & ws)
{
	// Create  vertices in that graph
	VertexID u = boost::add_vertex(tree);
	nodeProperties& rootNode=tree[u];
	ws.to_popNode(root,rootNode);

	return(u);
}

/*
 * recursively append the populations to the tree
 * when the boolean gates are encountered before its reference nodes
 * we still can add it as it is because gating path is stored as population names instead of actual VertexID.
 * Thus we will deal with the the boolean gate in the actual gating process
 */
void addPopulation(populationTree &tree, VertexID parentID,workspace & ws,wsNode * parentNode,bool isParseGate)
{


	wsPopNodeSet children =ws.getSubPop(parentNode);
	wsPopNodeSet::iterator it;
		for(it=children.begin();it!=children.end();it++)
		{
			//add boost node
			VertexID curChildID = boost::add_vertex(tree);
			wsPopNode curChildNode=(*it);
			//convert to the node format that GatingHierarchy understands
			nodeProperties &curChild=tree[curChildID];
			//attach the populationNode to the boost node as property
			try
			{
				ws.to_popNode(curChildNode,curChild,isParseGate);
			}
			catch(logic_error & e){
				if(my_throw_on_error){
					throw(e);
				}
				else
				{
					//remove the failed node
					boost::remove_vertex(curChildID,tree);
					COUT << e.what()<< endl;
					break;
				}

			}
			if(g_loglevel>=POPULATION_LEVEL)
				COUT<<"node created:"<<curChild.getName()<<endl;

//			//interpolate curlyquad gate here since it needs the access to comp
//			gate * g = curChild.getGate();
//			if(g->getType() == CURLYQUADGATE)
//			{
//				CurlyGuadGate * curlyGate = dynamic_cast<CurlyGuadGate *>(g);
//				curlyGate->interpolate(comp);
//			}

			//add relation between current node and parent node
			boost::add_edge(parentID,curChildID,tree);
			//update the node map for the easy query by pop name

			//recursively add its descendants
			addPopulation(tree, curChildID,ws,&curChildNode,isParseGate);
		}


}
/*
 * Constructor that starts from a particular sampleNode from workspace to build a tree
 */
GatingHierarchy * ws2gh(wsSampleNode curSampleNode,workspace & ws,bool isParseGate,trans_global_vec * _gTrans,biexpTrans * _globalBiExpTrans,linTrans * _globalLinTrans)
{

	GatingHierarchy * gh;
	wsRootNode root=ws.getRoot(curSampleNode);
	if(isParseGate)
	{

		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
			COUT<<endl<<"parsing compensation..."<<endl;
		compensation comp=ws.getCompensation(curSampleNode);

		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
			COUT<<endl<<"parsing trans flags..."<<endl;
		PARAM_VEC transFlag=ws.getTransFlag(curSampleNode);

		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
			COUT<<endl<<"parsing transformation..."<<endl;
		//prefixed version
		trans_local trans = ws.getTransformation(root,comp,transFlag,_gTrans,_globalBiExpTrans,_globalLinTrans, true);

		/*
		 * unprefixed version. Both version of trans are added (sometime they are identical)
		 * so that the trans defined on uncompensated channel (e.g. SSC-A) can still be valid
		 * without being necessarily adding comp prefix.
		 * It is mainly to patch the legacy workspace of mac or win where the implicit trans is added for channel
		 * when its 'log' keyword is 1.
		 * vX doesn't have this issue since trans for each parameter/channel
		 * is explicitly defined in transform node.
		 */
		trans_local trans_raw=ws.getTransformation(root,comp,transFlag,_gTrans,_globalBiExpTrans,_globalLinTrans, false);
		//merge raw version of trans map to theprefixed version
		trans_map tp = trans_raw.getTransMap();
		for(trans_map::iterator it=tp.begin();it!=tp.end();it++)
		{
			trans.addTrans(it->first, it->second);
		}
		gh = new GatingHierarchy(comp, transFlag, trans);
	}
	else
		gh = new GatingHierarchy();

	if(g_loglevel>=POPULATION_LEVEL)
		COUT<<endl<<"parsing populations..."<<endl;

	populationTree &tree = gh->getTree();
	VertexID pVerID=addRoot(tree, root,ws);
	addPopulation(tree, pVerID,ws,&root,isParseGate);
	return gh;
}
