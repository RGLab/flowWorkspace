/*
 * GatingHierarchy.cpp
 *
 *  Created on: Mar 20, 2012
 *      Author: wjiang2
 */

#include "include/GatingHierarchy.hpp"







//default constructor without argument
GatingHierarchy::GatingHierarchy()
{

	isLoaded=false;
}



/*
 * Constructor that starts from a particular sampleNode from workspace to build a tree
 */
GatingHierarchy::GatingHierarchy(wsSampleNode curSampleNode,workspace & ws,bool isParseGate,trans_global_vec * _gTrans,biexpTrans * _globalBiExpTrans,linTrans * _globalLinTrans)
{


	isLoaded=false;

	wsRootNode root=ws.getRoot(curSampleNode);
	if(isParseGate)
	{

		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
			COUT<<endl<<"parsing compensation..."<<endl;
		comp=ws.getCompensation(curSampleNode);

		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
			COUT<<endl<<"parsing trans flags..."<<endl;
		transFlag=ws.getTransFlag(curSampleNode);

		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
			COUT<<endl<<"parsing transformation..."<<endl;
		trans=ws.getTransformation(root,comp,transFlag,_gTrans,_globalBiExpTrans,_globalLinTrans);
	}
	if(g_loglevel>=POPULATION_LEVEL)
		COUT<<endl<<"parsing populations..."<<endl;
	VertexID pVerID=addRoot(root,ws);
	addPopulation(pVerID,ws,&root,isParseGate);

}
/*
 * add root node first before recursively add the other nodes
 * since root node does not have gates as the others do
 */
VertexID GatingHierarchy::addRoot(wsRootNode root, workspace & ws)
{
	// Create  vertices in that graph
	VertexID u = boost::add_vertex(tree);
	nodeProperties& rootNode=tree[u];
	ws.to_popNode(root,rootNode);

	return(u);
}
/*
 * add empty root with only name set as default 'root'
 */
VertexID GatingHierarchy::addRoot(){


	// Create  vertices in that graph
	VertexID u = boost::add_vertex(tree);
	nodeProperties & rootNode=tree[u];
	rootNode.setName("root");



	return(u);
}

/*
 * recursively append the populations to the tree
 * when the boolean gates are encountered before its reference nodes
 * we still can add it as it is because gating path is stored as population names instead of actual VertexID.
 * Thus we will deal with the the boolean gate in the actual gating process
 */
void GatingHierarchy::addPopulation(VertexID parentID,workspace & ws,wsNode * parentNode,bool isParseGate)
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
			ws.to_popNode(curChildNode,curChild,isParseGate);
			if(g_loglevel>=POPULATION_LEVEL)
				COUT<<"node created:"<<curChild.getName()<<endl;


			//add relation between current node and parent node
			boost::add_edge(parentID,curChildID,tree);
			//update the node map for the easy query by pop name

			//recursively add its descendants
			addPopulation(curChildID,ws,&curChildNode,isParseGate);
		}


}
/*
 * this is for semi-automated pipeline to add population node associated with gate
 * assuming gate split the parent population into two subpops, one of which is to keep
 * depends on isNegate flag of the gate
 */
VertexID GatingHierarchy::addGate(gate* g,VertexID parentID,string popName)
{

	typedef boost::graph_traits<populationTree>::vertex_descriptor vertex_t;

	//validity check
	int res = getChildren(parentID, popName);
	if( res >0 ){
		popName.append(" already exists!");
		throw(domain_error(popName));
	}else{
		VertexID curChildID = boost::add_vertex(tree);

		nodeProperties &curChild = tree[curChildID];
		curChild.setName(popName.c_str());
		curChild.setGate(g);
		if(g_loglevel>=POPULATION_LEVEL)
			COUT<<"node created:"<<curChild.getName()<<endl;
		//attach the populationNode to the boost node as property

		//add relation between current node and parent node
		boost::add_edge(parentID,curChildID,tree);
		return curChildID;
	}

}
/*
 * remove the node along with associated population properities including indices and gates
 */
void GatingHierarchy::removeNode(VertexID nodeID)
{

	typedef boost::graph_traits<populationTree>::vertex_descriptor vertex_t;
	//remove edge associated with this node
	EdgeID e=getInEdges(nodeID);
	/*removing vertex cause the rearrange node index
	 * so make sure do it after get edge descriptor
	 */
	boost::remove_edge(e,tree);
	boost::remove_vertex(nodeID,tree);

}
compensation GatingHierarchy::getCompensation(){
	return comp;
}

void GatingHierarchy::printLocalTrans(){
	COUT<<endl<<"get trans from gating hierarchy"<<endl;
	trans_map trans=this->trans.getTransMap();

	for (trans_map::iterator it=trans.begin();it!=trans.end();it++)
	{
		transformation * curTrans=it->second;


		if(!curTrans->isInterpolated())
				throw(domain_error("non-interpolated calibration table:"+curTrans->getName()+curTrans->getChannel()+" from channel"+it->first));


		COUT<<it->first<<curTrans->getName()<<" "<<curTrans->getChannel()<<endl;;

	}
}

/*
 * subset operation is done within R,so there is no need for this member function
 * to apply subsetting within c++ thus avoid unnecessary numeric operation in c++
 * Note: need to manually free memory pointed by flowData
 */
/*
 * Deprecated: we don't want to keep a separate view of ncdfFlowSet in c++
 */
//flowData GatingHierarchy::getData(string sampleName,VertexID nodeID)
//{
////	COUT<<"reading data from ncdf"<<endl;
//
//	flowData res=nc->readflowData(sampleName);
//	//subset the results by indices for non-root node
//	if(nodeID>0)
//	{
//		throw(domain_error("accessing data through non-root node is not supported yet!"));
//	}
//	else
//		return res;
//}
/*
 * in-memory version
 */
flowData GatingHierarchy::getData(VertexID nodeID)
{
//	COUT<<"reading data from ncdf"<<endl;

	flowData res=fdata;
	//subset the results by indices for non-root node
	if(nodeID>0)
	{
		throw(domain_error("accessing data through non-root node is not supported yet!"));
	}
	else
		return res;
}
/*
 * load data from ncdfFlow file
 * TODO:the memory for flowData was actually allocated by getData function, it may be safer to set flag within getData in future when
 * we decide to keep getData seperate from loadData
 */
/*
 * Deprecated: we don't want to keep a separate view of ncdfFlowSet in c++
 */
//void GatingHierarchy::loadData(string sampleName)
//{
//
//	if(!isLoaded)
//	{
//		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
//					COUT <<"loading data from cdf.."<<endl;
//		fdata=getData(sampleName,0);
//		isLoaded=true;
//	}
//
//
//
//}
/*
 * non-cdf version
*/
void GatingHierarchy::loadData(const flowData & _fdata)
{

	if(!isLoaded)
	{
		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
					COUT <<"loading data from memory.."<<endl;
		fdata=_fdata;
		isLoaded=true;
	}



}

void GatingHierarchy::unloadData()
{

	if(isLoaded)
	{
		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
					COUT <<"unloading raw data.."<<endl;
//		delete fdata.data;
		fdata.clear();
		isLoaded=false;
	}



}
/*
 * transform the data
 */
void GatingHierarchy::transforming()
{
	if(g_loglevel>=GATING_HIERARCHY_LEVEL)
		COUT <<"start transforming data :"<<fdata.getSampleID()<<endl;
	if(!isLoaded)
		throw(domain_error("data is not loaded yet!"));

//	unsigned nEvents=fdata.nEvents;
//	unsigned nChannls=fdata.nChannls;
	vector<string> channels=fdata.getParams();

	/*
	 * transforming each marker
	 */
	for(vector<string>::iterator it1=channels.begin();it1!=channels.end();it1++)
	{

		string curChannel=*it1;

		transformation *curTrans=trans.getTran(curChannel);

		if(curTrans!=NULL)
		{
			if(curTrans->gateOnly())
				continue;

			valarray<double> x(this->fdata.subset(curChannel));
			if(g_loglevel>=GATING_HIERARCHY_LEVEL)
				COUT<<"transforming "<<curChannel<<" with func:"<<curTrans->getChannel()<<endl;

			curTrans->transforming(x);
			/*
			 * update fdata
			 */
			fdata.updateSlice(curChannel,x);
//			fdata.data[fdata.getSlice(curChannel)]=x;

		}


	}

	/*
	 * write the entire slice back to cdf
	 */
//	if(updateCDF)
//	{
//		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
//			COUT<<"saving transformed data to CDF..."<<endl;
//		nc->writeflowData(fdata);
//	}
}
/*
 * extend gates if necessary
 */
void GatingHierarchy::extendGate(float extend_val){
	if(g_loglevel>=GATING_HIERARCHY_LEVEL)
			COUT <<endl<<"start extending Gates for:"<<fdata.getSampleID()<<endl;

		if(!isLoaded)
				throw(domain_error("data is not loaded yet!"));

		VertexID_vec vertices=getVertices(0);

		for(VertexID_vec::iterator it=vertices.begin();it!=vertices.end();it++)
		{
			VertexID u=*it;
			nodeProperties & node=getNodeProperty(u);
			if(u!=0)
			{
				gate *g=node.getGate();
				if(g==NULL)
					throw(domain_error("no gate available for this node"));
				if(g_loglevel>=POPULATION_LEVEL)
					COUT <<node.getName()<<endl;
				if(g->getType()!=BOOLGATE)
					g->extend(fdata,extend_val);
			}
		}
}
/*
 * the version without the need of loading data
 * by supplying the extend_to value
 */
void GatingHierarchy::extendGate(float extend_val, float extend_to){
	if(g_loglevel>=GATING_HIERARCHY_LEVEL)
			COUT <<endl<<"start extending Gates for:"<<fdata.getSampleID()<<endl;


		VertexID_vec vertices=getVertices(0);

		for(VertexID_vec::iterator it=vertices.begin();it!=vertices.end();it++)
		{
			VertexID u=*it;
			nodeProperties & node=getNodeProperty(u);
			if(u!=0)
			{
				gate *g=node.getGate();
				if(g==NULL)
					throw(domain_error("no gate available for this node"));
				if(g_loglevel>=POPULATION_LEVEL)
					COUT <<node.getName()<<endl;
				if(g->getType()!=BOOLGATE)
					g->extend(extend_val,extend_to);
			}
		}
}
/*
 * adjust gates by gains
 */
void GatingHierarchy::adjustGate(map<string,float> &gains){
	if(g_loglevel>=GATING_HIERARCHY_LEVEL)
			COUT <<endl<<"start rescale Gates by gains for:"<<fdata.getSampleID()<<endl;


		VertexID_vec vertices=getVertices(0);

		for(VertexID_vec::iterator it=vertices.begin();it!=vertices.end();it++)
		{
			VertexID u=*it;
			nodeProperties & node=getNodeProperty(u);
			if(u!=0)
			{
				gate *g=node.getGate();
				if(g==NULL)
					throw(domain_error("no gate available for this node"));
				if(g_loglevel>=POPULATION_LEVEL)
					COUT <<node.getName()<<endl;
				if(g->getType()!=BOOLGATE)
					g->gain(gains);
			}
		}
}

/*
 * transform gates
 */
void GatingHierarchy::transformGate(){
	if(g_loglevel>=GATING_HIERARCHY_LEVEL)
			COUT <<endl<<"start transform Gates for:"<<fdata.getSampleID()<<endl;


		VertexID_vec vertices=getVertices(0);

		for(VertexID_vec::iterator it=vertices.begin();it!=vertices.end();it++)
		{
			VertexID u=*it;
			nodeProperties & node=getNodeProperty(u);
			if(u!=0)
			{
				gate *g=node.getGate();
				if(g==NULL)
					throw(domain_error("no gate available for this node"));
				if(g_loglevel>=POPULATION_LEVEL)
					COUT <<node.getName()<<endl;
				if(g->getType()!=BOOLGATE)
					g->transforming(trans);
			}
		}
}
/*
 * traverse the tree to gate each pops
 * assuming data have already been compensated and transformed
 *
 */
void GatingHierarchy::gating(VertexID u,bool recompute)
{

//	if(!isLoaded)
//			throw(domain_error("data is not loaded yet!"));


	nodeProperties & node=getNodeProperty(u);
	if(u==0)
	{
		node.setIndices(fdata.getEventsCount());
		node.computeStats();
	}else
	{
		/*
		 * check if current population is already gated (by boolGate)
		 *
		 */
		if(recompute||!node.isGated())
			calgate(u);
	}



	//recursively gate all the descendants of u
	VertexID_vec children=getChildren(u);
	for(VertexID_vec::iterator it=children.begin();it!=children.end();it++)
	{
		//add boost node
		VertexID curChildID = *it;
		gating(curChildID,recompute);
	}

}
void GatingHierarchy::calgate(VertexID u)
{
	nodeProperties & node=getNodeProperty(u);

	/*
	 * check if parent population is already gated
	 * because the boolgate's parent might be visited later than boolgate itself
	 */

	VertexID pid=getParent(u);

	nodeProperties & parentNode =getNodeProperty(pid);
	if(!parentNode.isGated())
	{
		if(g_loglevel>=POPULATION_LEVEL)
			COUT <<"go to the ungated parent node:"<<parentNode.getName()<<endl;
		calgate(pid);
	}



	if(g_loglevel>=POPULATION_LEVEL)
		COUT <<"gating on:"<<node.getName()<<endl;

	gate *g=node.getGate();

	if(g==NULL)
		throw(domain_error("no gate available for this node"));

	/*
	 * calculate the indices for the current node
	 */
	vector<bool> curIndices;
	switch(g->getType())
	{
	case BOOLGATE:
		curIndices=boolGating(u);
		break;
	case LOGICALGATE://skip any gating operation since the indice is already set once the gate is added
		node.computeStats();
		return;
	default:
		curIndices=g->gating(fdata);
	}

	//combine with parent indices
	transform (curIndices.begin(), curIndices.end(), parentNode.getIndices().begin(), curIndices.begin(),logical_and<bool>());
	node.setIndices(curIndices);
	node.computeStats();
}
/**
 * bool gating operates on the indices of reference nodes
 * because they are global, thus needs to be combined with parent indices
 * in cases of negated gate (i.e. !A & !B)
 * @param u
 * @return
 */

vector<bool> GatingHierarchy::boolGating(VertexID u){

	nodeProperties & node=getNodeProperty(u);
	gate * g=node.getGate();

	//init the indices
//	unsigned nEvents=fdata.getEventsCount();

//	vector<bool> ind(nEvents,true);
	/*it is kinda of expensive to init a long bool vector
	 *
	 */
	vector<bool> ind;
	/*
	 * combine the indices of reference populations
	 */

	vector<BOOL_GATE_OP> boolOpSpec=g->getBoolSpec();
	for(vector<BOOL_GATE_OP>::iterator it=boolOpSpec.begin();it!=boolOpSpec.end();it++)
	{
		/*
		 * find id of reference node
		 */
		VertexID nodeID;
		/*
		 * assume the reference node has already added during the parsing stage
		 */
		vector<string> nodePath=it->path;
		nodeID=getRefNodeID(u,nodePath);

		nodeProperties & curPop=getNodeProperty(nodeID);
		//prevent self-referencing
		if(nodeID == u){
			string strErr = "The boolean gate is referencing to itself: ";
			strErr.append(curPop.getName());
			throw(domain_error(strErr));
		}

		if(!curPop.isGated())
		{
			if(g_loglevel>=POPULATION_LEVEL)
				COUT <<"go to the ungated reference node:"<<curPop.getName()<<endl;
			calgate(nodeID);
		}

		vector<bool> curPopInd=curPop.getIndices();
		if(it->isNot)
			curPopInd.flip();

		/*
		 * for the first reference node
		 * assign the indices directly without logical operation
		 */
		if(it==boolOpSpec.begin())
			ind=curPopInd;
		else
		{
			switch(it->op)
			{
				case '&':
					transform (ind.begin(), ind.end(), curPopInd.begin(), ind.begin(),logical_and<bool>());
					break;
				case '|':
					transform (ind.begin(), ind.end(), curPopInd.begin(), ind.begin(),logical_or<bool>());
					break;
				default:
					throw(domain_error("not supported operator!"));
			}
		}

	}

	if(g->isNegate())
		ind.flip();

	return ind;

}
/**
 * external boolOpSpec can be provided .
 * It is mainly used by openCyto rectRef gate
 * (needs to be combined with parent indices)
 *
 * @param u
 * @param boolOpSpec
 * @return
 */
vector<bool> GatingHierarchy::boolGating(vector<BOOL_GATE_OP> boolOpSpec){

	vector<bool> ind;
	/*
	 * combine the indices of reference populations
	 */


	for(vector<BOOL_GATE_OP>::iterator it=boolOpSpec.begin();it!=boolOpSpec.end();it++)
	{
		/*
		 * find id of reference node
		 */
		VertexID nodeID;
		/*
		 * assume the reference node has already added during the parsing stage
		 */
		vector<string> nodePath=it->path;

		nodeID=getNodeID(nodePath);//search ID by path


		nodeProperties & curPop=getNodeProperty(nodeID);

		if(!curPop.isGated())
		{
			if(g_loglevel>=POPULATION_LEVEL)
				COUT <<"go to the ungated reference node:"<<curPop.getName()<<endl;
			calgate(nodeID);
		}

		vector<bool> curPopInd=curPop.getIndices();
		if(it->isNot)
			curPopInd.flip();

		/*
		 * for the first reference node
		 * assign the indices directly without logical operation
		 */
		if(it==boolOpSpec.begin())
			ind=curPopInd;
		else
		{
			switch(it->op)
			{
				case '&':
					transform (ind.begin(), ind.end(), curPopInd.begin(), ind.begin(),logical_and<bool>());
					break;
				case '|':
					transform (ind.begin(), ind.end(), curPopInd.begin(), ind.begin(),logical_or<bool>());
					break;
				default:
					throw(domain_error("not supported operator!"));
			}
		}

	}

	return ind;

}


/*
 * current output the graph in dot format
 * and further covert it to gxl in order for Rgraphviz to read since it does not support dot directly
 * right now the data exchange is through file system,it would be nice to do it in memory
 */
void GatingHierarchy::drawGraph(string output)
{
	ofstream outputFile(output.c_str());

	boost::write_graphviz(outputFile,tree,OurVertexPropertyWriterR(tree));
	outputFile.close();


}

class custom_bfs_visitor : public boost::default_bfs_visitor
{

public:
	custom_bfs_visitor(VertexID_vec& v) : vlist(v) { }
	VertexID_vec & vlist;
  template < typename Vertex, typename Graph >
  void discover_vertex(Vertex u, const Graph & g) const
  {
	  vlist.push_back(u);
//	  v=u;
  }

};

/*
 * retrieve the vertexIDs in topological order,BFS or in regular order
 */

VertexID_vec GatingHierarchy::getVertices(unsigned short order){

	VertexID_vec res, vertices;
	switch (order)
	{

		case REGULAR:
		{
			VertexIt it_begin,it_end;
			tie(it_begin,it_end)=boost::vertices(tree);
			for(VertexIt it=it_begin;it!=it_end;it++)
				res.push_back((unsigned long)*it);
		}
		break;

		case TSORT:
		{
			boost::topological_sort(tree,back_inserter(vertices));
			for(VertexID_vec::reverse_iterator it=vertices.rbegin();it!=vertices.rend();it++)
				res.push_back(*it);
		}
		break;

		case BFS:
		{
			custom_bfs_visitor vis(res);
//			vector<VertexID> p(num_vertices(tree));
//			populationTree tree_copy(num_vertices(tree));
			boost::breadth_first_search(tree, vertex(0, tree)
										, boost::visitor(
												vis
//												boost::make_bfs_visitor(boost::record_predecessors(&p[0]
//																									 ,boost::on_tree_edge()
//																									)
//																					)
														)
										);
//			res=vis.vlist;

		}
		break;

		default:
			throw(domain_error("not valid sort type for tree traversal!"));
	}

	return(res);

}

/**
 *  Unary predicate for checking whether a string is empty
 * @param path
 * @return
 */
bool isEmpty(string path){
	return(path.empty());

}
/**
 * retrieve the VertexID by the gating path
 * @param gatePath single string containing full(or partial) gating path
 * @return
 */
VertexID GatingHierarchy::getNodeID(string gatePath){
	StringVec res;
	boost::split(res,gatePath,boost::is_any_of("/"));
	//remove the empty string
	res.erase(remove_if(res.begin(),res.end(), isEmpty), res.end());
	return (getNodeID(res));

}
/**
 * retrieve the VertexID by the gating path.
 * this serves as a parser to convert generic gating path into internal node ID
 * and it doesn't allow ambiguity (throw the exception when multiple nodes match)
 * @param gatePath a string vector of full(or partial) gating path
 * @return node id
 */
VertexID GatingHierarchy::getNodeID(vector<string> gatePath){
	VertexID_vec nodeIDs = queryByPath(gatePath);
	unsigned nMatches = nodeIDs.size();
	if(nMatches == 1)
			return nodeIDs.at(0);
	else{
		string errMsg;
		for(unsigned i = 0; i < gatePath.size()-1; i ++)
			errMsg.append(gatePath.at(i) + "/");
		errMsg.append(gatePath.at(gatePath.size()-1));
		if(nMatches == 0)
			throw(domain_error(errMsg + " not found!" ));
		else
			throw(domain_error(errMsg + " is ambiguous within the gating tree!"));
	}

}

/**
 *  find the most immediate common ancestor
 *
 * @param u input node ID
 * @param v input node ID
 * @param nDepths the depths of ancestor node. It is used to as the measurement to determine which reference node to win when multiple matches occur.
 * 											   The deeper it is, the nearer the reference is to the boolean node.
 * @return
 */
VertexID GatingHierarchy::getCommonAncestor(VertexID u, VertexID v, unsigned & nDepths){
	if(u == v)
		throw(domain_error("Can't proceed the process of finding common ancestor for identical nodes"));

	VertexID_vec p_u ,p_v ;
	VertexID res = 0;
	/*
	 * calculate the levels of going up in order to find their common ancestor
	 */

	/*
	 * trace each node back to the root
	 */

	for(VertexID curNode = u; curNode != 0; curNode = getParent(curNode))
		p_u.push_back(curNode);

	for(VertexID curNode = v; curNode != 0; curNode = getParent(curNode))
			p_v.push_back(curNode);

	/*
	 * walking the two path (toop-down) simultaneously and stop at the first diverging node
	 */
	VertexID_vec::reverse_iterator it_u, it_v;
	for(it_u = p_u.rbegin(), it_v = p_v.rbegin(); it_u != p_u.rend(), it_v != p_v.rend(); it_u++, it_v++)
	{
		if(*it_u == *it_v)
			res = * it_u;
		else
			break;
	}

	nDepths = distance(p_u.rbegin(), it_u);

	return res;

}

/**
  * Searching for reference node for bool gating given the refnode name and current bool node id
  *
  *
  * @param u the current bool node
 * @param refPath the reference node name
 * @return reference node id
 */
VertexID GatingHierarchy::getRefNodeID(VertexID u,vector<string> refPath){

	VertexID_vec nodeIDs = queryByPath(refPath);
	unsigned nMatches = nodeIDs.size();
	if(nMatches == 1)
			return nodeIDs.at(0);
	else{
		string errMsg = "Reference node: ";
		for(unsigned i = 0; i < refPath.size()-1; i ++)
			errMsg.append(refPath.at(i) + "/");
		errMsg.append(refPath.at(refPath.size()-1));
		if(nMatches == 0)
			throw(domain_error(errMsg + " not found!" ));
		else{
			/*
			 * select the nearest one to u when multiple nodes matches
			 * The deeper the common ancestor is, the closer the refNode is to the target bool node
			 */

			vector<unsigned> similarity;
			for(VertexID_vec::iterator it = nodeIDs.begin(); it!= nodeIDs.end(); it++){
				unsigned nAncestorDepths;
				VertexID ancestor = getCommonAncestor(u, *it, nAncestorDepths);
				/*
				 * set to minimum when the reference node is the descendant of bool node itself
				 * then this reference node should be excluded since it will cause infinite-loop of self-referral
				 */
				if(ancestor == u)
					nAncestorDepths = 0;
				similarity.push_back(nAncestorDepths);
			}


			vector<unsigned>::iterator maxIt = max_element(similarity.begin(), similarity.end());
			if(count(similarity.begin(), similarity.end(), *maxIt) > 1)
				throw(domain_error(errMsg + " can't be determined due to the multiple matches with the same distance to boolean node!" ));
			else{
				short nPos = distance(similarity.begin(), maxIt);
				return nodeIDs.at(nPos);
			}

		}

	}


}

/**
 * retrieve the VertexIDs by the gating path.
 * This routine allows multiple matches
 * @param gatePath input
 * @return node IDs that matches to the query path
 */
VertexID_vec GatingHierarchy::queryByPath(vector<string> gatePath){
	VertexID_vec res;
	/*
	 * search for the leaf node
	 */
	string leafName=gatePath.at(gatePath.size()-1);
	VertexID_vec leafIDs=getDescendants(0,leafName);


	/*
	 * bottom-up searching each route from matched leaf nodes
	 */
	VertexID_vec::iterator it_leaf,it_matched;
	it_matched = leafIDs.end();

	for(it_leaf = leafIDs.begin(); it_leaf != leafIDs.end(); it_leaf++)
	{
		/*
		 * bottom up matching to the given gating path
		 *
		 */

		VertexID curLeafID = *it_leaf;

		// start from the parent of the leaf node
		VertexID curNodeID = curLeafID;
		vector<string>::reverse_iterator it;
		for(it = gatePath.rbegin()+1;it!=gatePath.rend();it++)
		{
			//get current parent from node path
			string parentNameFromPath = *it;

			/*
			 * retrieve the actual parent node from the tree
			 */
			VertexID parentID = getParent(curNodeID);
			string parentName = getNodeProperty(parentID).getName();
			//compare it to the parent node from the path
			if(parentName.compare(parentNameFromPath) != 0)
			{
				break; //not matched then exit current route
			}else{
				//move up to the next ancestor and continue the matching process
				curNodeID = parentID;
			}
		}

		//when it succeeds to the end of path
		if(it == gatePath.rend())
			res.push_back(curLeafID);

	}


	return res;

}

/**
 * search for all the nodes that matches the pop name given the ancestor node id
 * @param u the ancestor node id to search from
 * @param name the node name to search for
 * @return the vector of node id that match
 */
VertexID_vec GatingHierarchy::getDescendants(VertexID u,string name){
	VertexID_vec nodesTomatch, res;
	custom_bfs_visitor vis(nodesTomatch);
	boost::breadth_first_search(tree, u, boost::visitor(vis));
	VertexID_vec::iterator it;
	for(it=nodesTomatch.begin();it!=nodesTomatch.end();it++)
	{
		u=*it;
		if(getNodeProperty(u).getName().compare(name)==0)
			res.push_back(u);
	}
//	if(it==nodesTomatch.end())
//	{
//		if(g_loglevel>=POPULATION_LEVEL)
//			COUT<<name<<" not found under the node: "<<boost::lexical_cast<string>(u)<<". returning the root instead."<<endl;;
//		u=0;
//	}
	return res;
}



/**
 * retrieve VertexID that matches population name given an ancestor node
 * It is used to search for the first node in the gating path (full or partial).
 * This is different from getRefNodeID in the way that pop name must be uniquely identifiable in the tree.
 * @param u the ancestor node id
 * @param popName the population name to match
 * @return node ID
 */
VertexID GatingHierarchy::getDescendant(VertexID u,string popName){


	/*
	 * top-down searching from that ancestor
	 */
	VertexID_vec res = getDescendants(u,popName);
	unsigned nMatches = res.size();

	switch (nMatches){
	case 0:
			popName.append(" not found within the gating tree!");
			throw(domain_error(popName));
	case 1:
			return (res.at(0));

	default:
			popName.append(" is ambiguous within the gating tree!");
			throw(domain_error(popName));
	}

}

/**
 * retrieve population paths
  * the assumption is each node only has one parent
 *
 * @param order passed to getVertices function
 * @param fullPath flag indicates whether to return full path or partial path
 * @param showHidden wether to include the hidden nodes
 * @return
 */
vector<string> GatingHierarchy::getPopPaths(unsigned short order,bool fullPath,bool showHidden){

	VertexID_vec vertices=getVertices(order);
	vector<string> res;
	for(VertexID_vec::iterator it=vertices.begin();it!=vertices.end();it++)
	{
		VertexID u=*it;
		nodeProperties & np = getNodeProperty(u);

		if(!showHidden&&np.getHiddenFlag())
			continue;

		string nodeName=np.getName();
		/*
		 * append ancestors on its way of tracing back to the root node
		 */

		while(u > 0)
		{
			//when full path is false, check if the current partial path is uniquely identifiable
			if(!fullPath)
			{
				try{
					getNodeID(nodeName);
					break;//quit the path growing if not no error (i.e. it is unique)
				}
				catch(const domain_error & e){
					// otherwise do nothing but continue to grow the path
				}

			}

			nodeName="/"+nodeName;
			u=getParent(u);
				if(u>0)//don't append the root node
					nodeName=getNodeProperty(u).getName()+nodeName;



		}


		res.push_back(nodeName);

	}
	return res;
}
/*
 * assume getParent only returns one parent node
 */
VertexID GatingHierarchy::getAncestor(VertexID u,unsigned short level){

	for(unsigned short i=0;i<level;i++)
		u=getParent(u);
	return(u);
}
/*
 * using boost in_edges out_edges to retrieve adjacent vertices
 * assuming only one parent for each node
 */
EdgeID GatingHierarchy::getInEdges(VertexID target){
	vector<EdgeID> res;
	string err;
	err.append(boost::lexical_cast<string>(target));

	if(target<=boost::num_vertices(tree)-1)
	{

		boost::graph_traits<populationTree>::in_edge_iterator in_i, in_end;

		for (tie(in_i, in_end) = in_edges(target,tree);
			         in_i != in_end; ++in_i)
		{
			EdgeID e = *in_i;
			res.push_back(e);
		}

	}
	else
		throw(domain_error(err+" :invalid vertexID!"));


	if(res.size()==0)
		throw(domain_error(err+" :parent not found!"));
	if(res.size()>1) //we only allow one parent per node
		throw(domain_error(err+" :multiple parent nodes found!"));

	return(res.at(0));
}

VertexID GatingHierarchy::getParent(VertexID target){
	EdgeID e=getInEdges(target);
	return  boost::source(e, tree);
}
/*
 * retrieve all children nodes
 */
VertexID_vec GatingHierarchy::getChildren(VertexID source){

	VertexID_vec res;
	if(source<=boost::num_vertices(tree)-1)
	{

		EdgeID e;
		boost::graph_traits<populationTree>::out_edge_iterator out_i, out_end;

		for (tie(out_i, out_end) = out_edges(source,tree);
				 out_i != out_end; ++out_i)
			{
			  e = *out_i;
			  VertexID  targ = target(e, tree);
			  res.push_back(targ);
			}
	}
	else
	{
		COUT<<"invalid vertexID:"<<source<<endl;
//		res.push_back(0);
	}
	return(res);
}

/**
 * retrieve single child node by parent id and child name.
 * @param source id of the source node
 * @param childName the child node name
 * @return the child node id if succeeds; otherwise return -1.
 */
int GatingHierarchy::getChildren(VertexID source,string childName){

	int curNodeID;
	VertexID_vec children=getChildren(source);
	VertexID_vec::iterator it;
	for(it=children.begin();it!=children.end();it++)
	{
		curNodeID=*it;
		if(getNodeProperty(curNodeID).getName().compare(childName)==0)
			break;
	}
	if(it==children.end())
		curNodeID = -1;


	return(curNodeID);

}
/*
 *
 * make sure to use this API always since since it is safe way to access tree nodes due to the validity check
 *
 *since the vertex bundle should always exist as long as the  tree and node exist, thus it is safe
 * to return the reference of it
 */
nodeProperties & GatingHierarchy::getNodeProperty(VertexID u){


	if(u<=boost::num_vertices(tree)-1)
		return(tree[u]);
	else
	{
		throw(out_of_range("returning empty node due to the invalid vertexID:" + boost::lexical_cast<std::string>(u)));

	}
}

/*
 *TODO:to deal with trans copying (especially how to sync with gTrans)
  up to caller to free the memory
 */
GatingHierarchy * GatingHierarchy::clone(const trans_map & _trans,trans_global_vec * _gTrans){

	GatingHierarchy * res=new GatingHierarchy();


	res->trans.setTransMap(_trans);

	res->comp=comp;

	res->tree=tree;

	return res;
}
/*
 * TODO:this overloading function is a temporary solution:
 * difference from the above one is:
 * does not copy trans
 */
GatingHierarchy * GatingHierarchy::clone(){

	GatingHierarchy * res=new GatingHierarchy();

	res->comp=comp;

	res->tree=tree;

	return res;
}
/**
 * It is mainly used by Rcpp API addTrans to propagate global trans map to each sample
  * @param trans trans_map
 */
void GatingHierarchy::addTransMap(trans_map tm){
	trans.setTransMap(tm);

}
