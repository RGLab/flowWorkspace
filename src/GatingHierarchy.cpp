/*
 * GatingHierarchy.cpp
 *
 *  Created on: Mar 20, 2012
 *      Author: wjiang2
 */

#include "include/GatingHierarchy.hpp"




/*need to be careful that gate within each node of the GatingHierarchy is
 * dynamically allocated,even it GatingHierarchy gets copied before destroyed
 * these gates are already gone since the gate objects were already freed by
 * this destructor
 */


GatingHierarchy::~GatingHierarchy()
{
	//for each node

//	cout<<"entring the destructor of GatingHierarchy"<<endl;

	VertexID_vec vertices=getVertices(0);
	if(dMode>=GATING_HIERARCHY_LEVEL)
		cout<<"free the node properties from tree"<<endl;
	for (VertexID_vec::iterator it=vertices.begin();it!=vertices.end();it++)
	{
		nodeProperties * np=tree[*it];
		if(np==NULL)
			throw(domain_error("empty node properties!"));
		if(dMode>=POPULATION_LEVEL)
			cout<<"free "<<np->getName()<<endl;
		delete np;//free the property bundle
		np = NULL;
//			cout<<"free the indices"<<endl;
	}

}

//default constructor without argument
GatingHierarchy::GatingHierarchy()
{
	dMode=1;
	isLoaded=false;
//	isGated=false;
//	thisWs=NULL;
//	nc=NULL;
//	gTrans=NULL;
}



/*
 * Constructor that starts from a particular sampleNode from workspace to build a tree
 */
GatingHierarchy::GatingHierarchy(wsSampleNode curSampleNode,workspace * ws,bool isParseGate,trans_global_vec * _gTrans,biexpTrans * _globalBiExpTrans,linTrans * _globalLinTrans,unsigned short _dMode)
{
//	data=NULL;
	dMode=_dMode;
	isLoaded=false;
//	isGated=false;
//	thisWs=ws;
//	nc=_nc;
//	gTrans=_gTrans;
//	cout<<"get root node"<<endl;

	wsRootNode root=ws->getRoot(curSampleNode);
	if(isParseGate)
	{

		if(dMode>=GATING_HIERARCHY_LEVEL)
			cout<<endl<<"parsing compensation..."<<endl;
		comp=ws->getCompensation(curSampleNode);

		if(dMode>=GATING_HIERARCHY_LEVEL)
			cout<<endl<<"parsing trans flags..."<<endl;
		transFlag=ws->getTransFlag(curSampleNode);

		if(dMode>=GATING_HIERARCHY_LEVEL)
			cout<<endl<<"parsing transformation..."<<endl;
		trans=ws->getTransformation(root,comp,transFlag,_gTrans,_globalBiExpTrans,_globalLinTrans);
	}
	if(dMode>=POPULATION_LEVEL)
		cout<<endl<<"parsing populations..."<<endl;
	VertexID pVerID=addRoot(ws->to_popNode(root));
	addPopulation(pVerID,ws,&root,isParseGate);
//	if(isParseGate)
//		gating();

}
/*
 * add root node first before recursively add the other nodes
 * since root node does not have gates as the others do
 */
VertexID GatingHierarchy::addRoot(nodeProperties* rootNode)
{
	// Create  vertices in that graph
	VertexID u = boost::add_vertex(tree);

	tree[u]=rootNode;


	return(u);
}
/*
 * add empty root with only name set as default 'root'
 */
VertexID GatingHierarchy::addRoot(){

	nodeProperties * rootNode=new nodeProperties;
	rootNode->setName("root");
	// Create  vertices in that graph
	VertexID u = boost::add_vertex(tree);

	tree[u]=rootNode;


	return(u);
}

/*
 * recursively append the populations to the tree
 * when the boolean gates are encountered before its reference nodes
 * we still can add it as it is because gating path is stored as population names instead of actual VertexID.
 * Thus we will deal with the the boolean gate in the actual gating process
 */
void GatingHierarchy::addPopulation(VertexID parentID,workspace * ws,wsNode * parentNode,bool isParseGate)
{


	wsPopNodeSet children =ws->getSubPop(parentNode);
	wsPopNodeSet::iterator it;
		for(it=children.begin();it!=children.end();it++)
		{
			//add boost node
			VertexID curChildID = boost::add_vertex(tree);
			wsPopNode curChildNode=(*it);
			//convert to the node format that GatingHierarchy understands
			nodeProperties *curChild=ws->to_popNode(curChildNode,isParseGate);
			if(dMode>=POPULATION_LEVEL)
				cout<<"node created:"<<curChild->getName()<<endl;
			//attach the populationNode to the boost node as property
			tree[curChildID]=curChild;
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

	VertexID curChildID = boost::add_vertex(tree);


	nodeProperties *curChild=new nodeProperties();
	curChild->setName(popName.c_str());
	curChild->setGate(g);
	if(dMode>=POPULATION_LEVEL)
		cout<<"node created:"<<curChild->getName()<<endl;
	//attach the populationNode to the boost node as property
	tree[curChildID]=curChild;
	//add relation between current node and parent node
	boost::add_edge(parentID,curChildID,tree);
	return curChildID;

}
/*
 * remove the node and associated population properities including indices and gates
 */
void GatingHierarchy::removeNode(VertexID nodeID)
{

	typedef boost::graph_traits<populationTree>::vertex_descriptor vertex_t;


	nodeProperties *curNode=getNodeProperty(nodeID);

	if(dMode>=POPULATION_LEVEL)
		cout<<"removing node:"<<curNode->getName()<<endl;
	delete curNode;

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
	cout<<endl<<"get trans from gating hierarchy"<<endl;
	trans_map trans=this->trans.getTransMap();

	for (trans_map::iterator it=trans.begin();it!=trans.end();it++)
	{
		transformation * curTrans=it->second;


		if(!curTrans->isInterpolated())
				throw(domain_error("non-interpolated calibration table:"+curTrans->getName()+curTrans->getChannel()+" from channel"+it->first));


		cout<<it->first<<curTrans->getName()<<" "<<curTrans->getChannel()<<endl;;

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
////	cout<<"reading data from ncdf"<<endl;
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
//	cout<<"reading data from ncdf"<<endl;

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
//		if(dMode>=GATING_HIERARCHY_LEVEL)
//					cout <<"loading data from cdf.."<<endl;
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
		if(dMode>=GATING_HIERARCHY_LEVEL)
					cout <<"loading data from memory.."<<endl;
		fdata=_fdata;
		isLoaded=true;
	}



}

void GatingHierarchy::unloadData()
{

	if(isLoaded)
	{
		if(dMode>=GATING_HIERARCHY_LEVEL)
					cout <<"unloading raw data.."<<endl;
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
	if(dMode>=GATING_HIERARCHY_LEVEL)
		cout <<"start transforming data :"<<fdata.getSampleID()<<endl;
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
			if(dMode>=GATING_HIERARCHY_LEVEL)
				cout<<"transforming "<<curChannel<<" with func:"<<curTrans->getChannel()<<endl;

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
//		if(dMode>=GATING_HIERARCHY_LEVEL)
//			cout<<"saving transformed data to CDF..."<<endl;
//		nc->writeflowData(fdata);
//	}
}
/*
 * extend gates if necessary
 */
void GatingHierarchy::extendGate(float extend_val){
	if(dMode>=GATING_HIERARCHY_LEVEL)
			cout <<endl<<"start extending Gates for:"<<fdata.getSampleID()<<endl;

		if(!isLoaded)
				throw(domain_error("data is not loaded yet!"));

		VertexID_vec vertices=getVertices(0);

		for(VertexID_vec::iterator it=vertices.begin();it!=vertices.end();it++)
		{
			VertexID u=*it;
			nodeProperties * node=getNodeProperty(u);
			if(u!=0)
			{
				gate *g=node->getGate();
				if(g==NULL)
					throw(domain_error("no gate available for this node"));
				if(dMode>=POPULATION_LEVEL)
					cout <<node->getName()<<endl;
				if(g->getType()!=BOOLGATE)
					g->extend(fdata,extend_val,dMode);
			}
		}
}

/*
 * adjust gates by gains
 */
void GatingHierarchy::adjustGate(map<string,float> &gains){
	if(dMode>=GATING_HIERARCHY_LEVEL)
			cout <<endl<<"start rescale Gates by gains for:"<<fdata.getSampleID()<<endl;


		VertexID_vec vertices=getVertices(0);

		for(VertexID_vec::iterator it=vertices.begin();it!=vertices.end();it++)
		{
			VertexID u=*it;
			nodeProperties * node=getNodeProperty(u);
			if(u!=0)
			{
				gate *g=node->getGate();
				if(g==NULL)
					throw(domain_error("no gate available for this node"));
				if(dMode>=POPULATION_LEVEL)
					cout <<node->getName()<<endl;
				if(g->getType()!=BOOLGATE)
					g->gain(gains,dMode);
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


	nodeProperties * node=getNodeProperty(u);
	if(u==0)
	{
		node->setIndices(fdata.getEventsCount());
		node->computeStats();
	}else
	{
		/*
		 * check if current population is already gated (by boolGate)
		 *
		 */
		if(recompute||!node->isGated())
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
	nodeProperties * node=getNodeProperty(u);

	/*
	 * check if parent population is already gated
	 * because the boolgate's parent might be visited later than boolgate itself
	 */

	VertexID pid=getParent(u);

	nodeProperties *parentNode =getNodeProperty(pid);
	if(!parentNode->isGated())
	{
		if(dMode>=POPULATION_LEVEL)
			cout <<"go to the ungated parent node:"<<parentNode->getName()<<endl;
		calgate(pid);
	}



	if(dMode>=POPULATION_LEVEL)
		cout <<"gating on:"<<node->getName()<<endl;

	gate *g=node->getGate();

	if(g==NULL)
		throw(domain_error("no gate available for this node"));

	/*
	 * calculate the indices for the current node
	 */
	vector<bool> curIndices;
	if(g->getType()==BOOLGATE)
	{
		curIndices=boolGating(u);
	}
	else
	{
		/*
		 * transform gates if applicable
		 */

		g->transforming(trans,dMode);
		curIndices=g->gating(fdata);
	}

	//combine with parent indices
	transform (curIndices.begin(), curIndices.end(), parentNode->getIndices().begin(), curIndices.begin(),logical_and<bool>());
//	for(unsigned i=0;i<curIndices.size();i++)
//		curIndices.at(i)=curIndices.at(i)&parentNode->indices.at(i);
	node->setIndices(curIndices);
	node->computeStats();
}


vector<bool> GatingHierarchy::boolGating(VertexID u){

	nodeProperties * node=getNodeProperty(u);
	gate * g=node->getGate();

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
		if(nodePath.size()==1)
		{
			//search ID by nearest ancestor
			nodeID=getNodeID(u,nodePath.at(0));
		}
		else
		{

			nodeID=getNodeID(nodePath);//search ID by path
		}



		nodeProperties * curPop=getNodeProperty(nodeID);
		if(!curPop->isGated())
		{
			if(dMode>=POPULATION_LEVEL)
				cout <<"go to the ungated reference node:"<<curPop->getName()<<endl;
			calgate(nodeID);
		}

		vector<bool> curPopInd=curPop->getIndices();
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

/*
 * retrieve the VertexID by the gating path(could be subpath)
 * this serves as a parser to convert generic gating path into internal node ID
 *
 */
VertexID GatingHierarchy::getNodeID(vector<string> gatePath){

	/*
	 * search for the start node in the path
	 */
	string start=gatePath.at(0);
	VertexID nodeID=getNodeID(0,start);
	/*
	 * then trace down from the start node until the end
	 */
	for(vector<string>::iterator it=gatePath.begin()+1;it!=gatePath.end();it++)
	{
		string nodeNameFromPath=*it;
		/*
		 * search the children node of nodeID
		 */
		nodeID=getChildren(nodeID,nodeNameFromPath);

	}
	return nodeID;

}

VertexID GatingHierarchy::getDescendant(VertexID u,string name){
	VertexID_vec nodesTomatch;
	custom_bfs_visitor vis(nodesTomatch);
	boost::breadth_first_search(tree, u, boost::visitor(vis));
	VertexID_vec::iterator it;
	for(it=nodesTomatch.begin();it!=nodesTomatch.end();it++)
	{
		u=*it;
		if(getNodeProperty(u)->getName().compare(name)==0)
			break;
	}
	if(it==nodesTomatch.end())
	{
		if(dMode>=POPULATION_LEVEL)
			cout<<name<<" not found under the node: "<<boost::lexical_cast<string>(u)<<". returning the root instead."<<endl;;
		u=0;
	}
	return u;
}

/*
 * retrieve VertexID that matches population name
 */
VertexID GatingHierarchy::getNodeID(VertexID u,string popName){



	/*
	 * get the ancestor of 2 level above
	 */
	if(u!=0)
		u=getAncestor(u,1);	/*
							 *  this is the hack for now. it may break when the reference node is under the ancestor of N>=3 level above
							 *  other than root.
							 *  it would be more robust to have a nearest ancestor searching,but it is unclear
							 *  regarding to definition of the distance between nodes at the moment.
							 *
							 */

	/*
	 * top-down searching from that ancestor
	 */
	u=getDescendant(u,popName);

	/*
	 * if not found, try traverse the entire tree (top-down searching from the root)
	 * and assuming there is only on node will be matched to popName in the tree
	 */

	if(u==0)
	{
		if(dMode>=POPULATION_LEVEL)
			cout<<"searching from the root."<<endl;
		u=getDescendant(u,popName);
	}
	/*
	 * still not found, then report the error
	 */
	if(u==0)
	{
		string err="Node not found:";
		err.append(popName);
		throw(domain_error(err));
	}

	return u;

}

/*
 * retrieve population names based on getVertices method
 * isPath flag indicates whether append the ancestor node names
 * the assumption is each node only has one parent
 */
vector<string> GatingHierarchy::getPopNames(unsigned short order,bool isPath,bool showHidden){

	VertexID_vec vertices=getVertices(order);
	vector<string> res;
	for(VertexID_vec::iterator it=vertices.begin();it!=vertices.end();it++)
	{
		VertexID u=*it;
		nodeProperties * np = getNodeProperty(u);

		if(!showHidden&&np->getHiddenFlag())
			continue;

		string nodeName=np->getName();



		/*
		 * append ancestors on its way of tracing back to the root node
		 */
		if(isPath)
		{
			while(u!=0)//if u==0, it is a root vertex
			{
				nodeName="/"+nodeName;
				u=getParent(u);
					if(u>0)//don't append the root node
						nodeName=getNodeProperty(u)->getName()+nodeName;


			}


		}else
		{
			string prefix=boost::lexical_cast<string>(u);

			nodeName=prefix+"."+nodeName;
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

	if(target>=0&&target<=boost::num_vertices(tree)-1)
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
	if(source>=0&&source<=boost::num_vertices(tree)-1)
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
		cout<<"invalid vertexID:"<<source<<endl;
//		res.push_back(0);
	}
	return(res);
}
/*
 * retrieve single child node by parent id and child name
 */
VertexID GatingHierarchy::getChildren(VertexID source,string childName){

	VertexID curNodeID;
	VertexID_vec children=getChildren(source);
	VertexID_vec::iterator it;
	for(it=children.begin();it!=children.end();it++)
	{
		curNodeID=*it;
		if(getNodeProperty(curNodeID)->getName().compare(childName)==0)
			break;
	}
	if(it==children.end())
	{
		string err="Node not found:";
		err.append(childName);
		throw(domain_error(err));
	}


	return(curNodeID);

}
/*
 * returning the reference of the vertex bundle
 * make sure to use this API always since since it is safe way to access tree nodes due to the validity check
 *
 */
nodeProperties * GatingHierarchy::getNodeProperty(VertexID u){


	if(u>=0&&u<=boost::num_vertices(tree)-1)
		return(tree[u]);
	else
	{
		throw(out_of_range("returning empty node due to the invalid vertexID:"+u));

	}
}
//void GatingHierarchy::reset(){
//	isGated=false;
//	isLoaded=false;
//	nc=NULL;
//	thisWs=NULL;
//	fdata.data.resize(0);
//}
/*
 *TODO:to deal with trans copying (especially how to sync with gTrans)
  up to caller to free the memory
 */
GatingHierarchy * GatingHierarchy::clone(const trans_map & _trans,trans_global_vec * _gTrans){

	GatingHierarchy * res=new GatingHierarchy();


	res->trans.setTransMap(_trans);
//	res->gTrans=_gTrans;
	res->comp=comp;

	/*
	 * copy bgl tree and update property bundle pointer for each node
	 * TODO:explore deep copying facility of bgl library,especially for node property bundle as the pointer
	 */
	res->tree=tree;
	VertexID_vec vertices=res->getVertices(0);
	for(VertexID_vec::iterator it=vertices.begin();it!=vertices.end();it++)
	{
		/*
		 * update the pointer for nodeProperties
		 */
		VertexID u=*it;
		nodeProperties * node=res->getNodeProperty(u);
		node=node->clone();//only copy gate definition without copying gating results
		//update the tree node
		res->tree[u]=node;
	}


	return res;
}
/*
 * TODO:this overloading function is a temporary solution:
 * difference from the above one is:
 * 1.does not copy trans
 * 2.copy stats and indices as well for each node
 */
GatingHierarchy * GatingHierarchy::clone(){

	GatingHierarchy * res=new GatingHierarchy();

	res->comp=comp;

	/*
	 * copy bgl tree and update property bundle pointer for each node
	 * TODO:explore deep copying facility of bgl library,especially for node property bundle as the pointer
	 */
	res->tree=tree;
	VertexID_vec vertices=res->getVertices(0);
	for(VertexID_vec::iterator it=vertices.begin();it!=vertices.end();it++)
	{
		/*
		 * update the pointer for nodeProperties
		 */
		VertexID u=*it;
		nodeProperties * node=res->getNodeProperty(u);
		node=node->clone(true);//copy gates results as well
		//update the tree node
		res->tree[u]=node;
	}


	return res;
}
