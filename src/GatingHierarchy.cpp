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

void GatingHierarchy::convertToPb(pb::GatingHierarchy & gh_pb){
	pb::populationTree * ptree = gh_pb.mutable_tree();
	/*
	 * cp tree
	 */
	VertexID_vec verIDs = getVertices();
	for(VertexID_vec::iterator it = verIDs.begin(); it != verIDs.end(); it++){
		VertexID thisVert = *it;
		nodeProperties & np = getNodeProperty(thisVert);

		pb::treeNodes * node = ptree->add_node();
		pb::nodeProperties * pb_node = node->mutable_node();
		bool isRoot = thisVert == 0;
		np.convertToPb(*pb_node, isRoot);
		if(!isRoot){
			node->set_parent(getParent(thisVert));
		}


	}
	//cp comp
	pb::COMP * comp_pb = gh_pb.mutable_comp();
	comp.convertToPb(*comp_pb);
	//cp trans
	pb::trans_local * trans_pb = gh_pb.mutable_trans();
	trans.convertToPb(*trans_pb);
	//cp trans flag
	BOOST_FOREACH(PARAM_VEC::value_type & it, transFlag){
		pb::PARAM * tflag_pb = gh_pb.add_transflag();
		it.convertToPb(*tflag_pb);
	}


}

GatingHierarchy::GatingHierarchy(pb::GatingHierarchy & pb_gh, map<intptr_t, transformation *> & trans_tbl):isLoaded(pb_gh.isloaded()){
	const pb::populationTree & tree_pb =  pb_gh.tree();
	for(int i = 0; i < tree_pb.node_size(); i++){
		const pb::treeNodes & node_pb = tree_pb.node(i);
		const pb::nodeProperties & np_pb = node_pb.node();

		VertexID curChildID = boost::add_vertex(tree);
		tree[curChildID] = nodeProperties(np_pb);

		if(node_pb.has_parent()){
			VertexID parentID = node_pb.parent();
			boost::add_edge(parentID,curChildID,tree);
		}

	}
	//restore comp
	comp = compensation(pb_gh.comp());
	//restore trans flag
	for(int i = 0; i < pb_gh.transflag_size(); i++){
		transFlag.push_back(PARAM(pb_gh.transflag(i)));
	}
	//restore trans local
	trans = trans_local(pb_gh.trans(), trans_tbl);
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
void GatingHierarchy::gating(VertexID u,bool recompute, bool computeTerminalBool)
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
			calgate(u, computeTerminalBool);
	}



	//recursively gate all the descendants of u
	VertexID_vec children=getChildren(u);
	for(VertexID_vec::iterator it=children.begin();it!=children.end();it++)
	{
		//add boost node
		VertexID curChildID = *it;
		gating(curChildID,recompute, computeTerminalBool);
	}

}
void GatingHierarchy::calgate(VertexID u, bool computeTerminalBool)
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
		calgate(pid, computeTerminalBool);
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
		{
			if(computeTerminalBool||getChildren(u).size()>0)
				curIndices=boolGating(u, computeTerminalBool);
			else
				return;

			break;

		}
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

vector<bool> GatingHierarchy::boolGating(VertexID u, bool computeTerminalBool){

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
			calgate(nodeID, computeTerminalBool);
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
vector<bool> GatingHierarchy::boolGating(vector<BOOL_GATE_OP> boolOpSpec, bool computeTerminalBool){

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
			calgate(nodeID, computeTerminalBool);
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
			boost::tie(it_begin,it_end)=boost::vertices(tree);
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

	//prepend root if it is absolute path (starts with /) and root is not yet prepended yet
	if(gatePath[0] == '/' && res.at(0)!= "root")
		res.insert(res.begin(), "root");
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
	VertexID_vec nodeIDs = queryByPath(0,gatePath);
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
 * @param nodeIDs input node IDs
 * @param nDepths the depths of ancestor node. It is used to as the measurement to determine which reference node to win when multiple matches occur.
 * 											   The deeper it is, the nearer the reference is to the boolean node.
 * @return ancestor ID
 */
VertexID GatingHierarchy::getCommonAncestor(VertexID_vec nodeIDs, unsigned & nDepths){

	unsigned nSize = nodeIDs.size();
	vector<VertexID_vec> paths(nSize) ;
	VertexID CommonAncestor = 0;

	/*
	 * calculate the levels of going up in order to find their common ancestor
	 */

	/*
	 * trace each node back to the root
	 */
	unsigned minDepths = numeric_limits<unsigned>::max() ;
	for(unsigned i = 0; i < nSize; i++)
	{
		unsigned counter = 0;
		for(VertexID curNode = nodeIDs.at(i); curNode != 0; curNode = getParent(curNode))
		{
			paths.at(i).push_back(curNode);
			counter++;
		}
		minDepths = min(counter, minDepths);
	}
	/*
	 * walking the paths (top-down) simultaneously and stop at the first diverging node
	 */

	for(nDepths = 0; nDepths < minDepths; nDepths++)
	{
		//check if all nodes are the same at this level
		unsigned j = 0;
		unsigned pos = paths.at(j).size()-nDepths -1;//root is at the end of vector
		VertexID u = paths.at(j).at(pos);
		//loop through the rest nodes to see if they equal to u
		for(j = 1; j < nSize; j++)
		{
			unsigned pos = paths.at(j).size()-nDepths -1;
			if(paths.at(j).at(pos) != u)
				break;
		}

		if(j == nSize)
			CommonAncestor = u;//update result if all are the same
		else
			break;//otherwise, stop the loop at this level
	}

	return CommonAncestor;

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

	/*
	 * to save searching time, try the siblings first(go up one level)
	 * which represents most of scenarios (e.g. cytokine boolean gates in ICS assay
	 */
	VertexID boolParentID = getParent(u);
	VertexID_vec nodeIDs = queryByPath(boolParentID,refPath);
	unsigned nMatches = nodeIDs.size();
	if(nMatches == 1)
		return nodeIDs.at(0);
	else
	{
		/*
		 * if failed to find reference from siblings, then go to more general searching logic (which takes more time)
		 */

		 nodeIDs = queryByPath(0,refPath);
		 nMatches = nodeIDs.size();
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
					VertexID_vec thisNodeVec(2);
					thisNodeVec.at(0) = u;
					thisNodeVec.at(1) = *it;
					VertexID ancestor = getCommonAncestor(thisNodeVec, nAncestorDepths);
					/*
					 * set to minimum when the reference node is the descendant of bool node itself
					 * then this reference node should be excluded since it will cause infinite-loop of self-referral
					 */
					if(ancestor == u)
						nAncestorDepths = 0;
					similarity.push_back(nAncestorDepths);
				}


				vector<unsigned>::iterator maxIt = max_element(similarity.begin(), similarity.end());
				vector<unsigned> matchedInd;
				for(unsigned i = 0; i < similarity.size(); i++){
					if(similarity.at(i) == *maxIt)
						matchedInd.push_back(i);
				}
				/*
				 * try to break the tie when multiple nodes have the same minimum depths
				 * by picking the one with the node depth closest to u
				 */
				unsigned nTie = matchedInd.size();
				vector<unsigned> relativeDepthVec(nTie);
				unsigned boolNodeDepth = getNodeDepths(u);
				unsigned nPos;
				if(nTie > 1){
					for(unsigned i = 0; i < nTie; i ++){
						VertexID thisNode = nodeIDs.at(matchedInd.at(i));
						relativeDepthVec.at(i) = std::abs((int)(getNodeDepths(thisNode) - boolNodeDepth));
					}
					vector<unsigned>::iterator minIt = min_element(relativeDepthVec.begin(), relativeDepthVec.end());
					if(count(relativeDepthVec.begin(), relativeDepthVec.end(), *minIt) > 1)
						throw(domain_error(errMsg + " can't be determined due to the multiple matches with the same distance to boolean node!" ));
					else{
						nPos = matchedInd.at(distance(relativeDepthVec.begin(), minIt));
					}
				}else{

					nPos = matchedInd.at(0);
				}
				return nodeIDs.at(nPos);
			}

		}

	}

}


/**
 * retrieve the VertexIDs by the gating path.
 * This routine allows multiple matches
 * @param ancestorID when gatePath is partial path, this node ID narrow the searching range.
 * @param gatePath input
 * @return node IDs that matches to the query path
 */
VertexID_vec GatingHierarchy::queryByPath(VertexID ancestorID, vector<string> gatePath){
	VertexID_vec res;
	/*
	 * search for the leaf node
	 */
	string leafName=gatePath.at(gatePath.size()-1);
	VertexID_vec leafIDs=getDescendants(ancestorID,leafName);


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
unsigned GatingHierarchy::getNodeDepths(VertexID u){
	unsigned i = 0;
	while(u > 0){
		u = getParent(u);
		i++;
	}

	return i ;
}
/*
 * assume getParent only returns one parent nodeGatingHierarchy
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

		for (boost::tie(in_i, in_end) = in_edges(target,tree);
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

		for (boost::tie(out_i, out_end) = out_edges(source,tree);
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
