/*
 * workspace.hpp
 *
 *  Created on: Mar 22, 2012
 *      Author: wjiang2
 */

#ifndef WORKSPACE_HPP_
#define WORKSPACE_HPP_
#include <vector>
#include <string>
#include <libxml/xpath.h>
#include <libxml/parser.h>
#include "wsNode.hpp"
#include <cytolib/GatingSet.hpp>
#include "cytolib/transformation.hpp"
#include <iostream>
#include <algorithm>
#include <fstream>

#define WS_WIN 1
#define WS_MAC 2
#define WS_VX 3
#define WS_MAC_3 4


using namespace std;

/*TODO: so far I have seen the major difference between win and mac workspace is the xpath(like xpath of sample node)
 * if this is the case eventually we can try to use one template class (eliminate two derived classes )
 * with T structure that stores different versions of xpaths for win/mac,for example:
 *
 * struct winWorkspace{
 * xpath_sample=xxx
 * ....
 * }
 *
 * struct macWorkspace{
 * xpath_sample=xxx
 * ....
 * }
 *
 * this may potentially reduce the amount of code
 *
 */
struct xpath{
	string group;
	string sampleRef;
	string sample;
	string sampleNode;
	string popNode;
	string gateDim;
	string gateParam;

	string attrName;
	string compMatName;
	string compMatChName;
	string compMatVal;
	unsigned short sampNloc;//get FCS filename(or sampleName) from either $FIL keyword or name attribute of sampleNode
};



class workspace{
protected:
	unsigned short g_loglevel;// debug print is turned off by default
	bool my_throw_on_error;//can be toggle off to get a partially parsed gating tree for debugging purpose

public:
	 xpath nodePath;
//protected:

	 xmlDoc * doc;

public:
	 workspace():g_loglevel(NO_LOG),my_throw_on_error(true){doc=NULL;};
	 virtual ~workspace()
	 {
			if(doc!=NULL)
			{
				xmlFreeDoc(doc);
				doc = NULL;

				/*
				 *Free the global variables that may
				 *have been allocated by the parser.
				 */
				xmlCleanupParser();
				if(g_loglevel>=GATING_SET_LEVEL)
					COUT<<"xml freed!"<<endl;
			}
	 }
	 void set_loglevel(unsigned short _g_loglevel){g_loglevel = _g_loglevel;};
	 void set_throw_on_error(bool flag){my_throw_on_error = flag;};
	 virtual string xPathSample(string sampleID)=0;
	 virtual PARAM_VEC getTransFlag(wsSampleNode sampleNode)=0;
	 virtual trans_local getTransformation(wsRootNode,const compensation &,PARAM_VEC &,trans_global_vec *, biexpTrans * _globalBiExpTrans, linTrans * _globalLinTrans, bool prefixed)=0;
	 virtual compensation getCompensation(wsSampleNode)=0;
	 virtual trans_global_vec getGlobalTrans()=0;
	 virtual vector <string> getSampleID(unsigned short)=0;
	 virtual string getSampleName(wsSampleNode &)=0;
	 virtual wsRootNode getRoot(wsSampleNode sampleNode)=0;
	 virtual wsPopNodeSet getSubPop(wsNode *)=0;
	 virtual gate * getGate(wsPopNode &)=0;//gate is dynamically allocated within this function,it is currently freed within gate pointer owner object nodeProperties
	 virtual void to_popNode(wsRootNode &, nodeProperties &)=0;
	 virtual void to_popNode(wsPopNode &,nodeProperties &,bool isGating)=0;
	 void toArray(string sCalTable, vector<double> &x, vector<double> &y)
	 {
		 vector<string> stringVec;
		 	boost::split(stringVec,sCalTable,boost::is_any_of(","));
		 	int nLen = stringVec.size()/2;
		 	x.resize(nLen);
		 	y.resize(nLen);
		 	for(unsigned i=0;i<nLen;i++)
		 	{
		 		y[i]=atof(stringVec.at(2*i).c_str());
		 		x[i]=atof(stringVec.at(2*i + 1).c_str());
		 //		COUT<<res[i]<<",";
		 	}
	 }
	 virtual void parseVersionList(){};
	 /*
	  * add root node first before recursively add the other nodes
	  * since root node does not have gates as the others do
	  */
	 VertexID addRoot(populationTree &tree, wsRootNode root)
	 {
	 	// Create  vertices in that graph
	 	VertexID u = boost::add_vertex(tree);
	 	nodeProperties& rootNode=tree[u];
	 	to_popNode(root,rootNode);

	 	return(u);
	 }

	 /*
	  * recursively append the populations to the tree
	  * when the boolean gates are encountered before its reference nodes
	  * we still can add it as it is because gating path is stored as population names instead of actual VertexID.
	  * Thus we will deal with the the boolean gate in the actual gating process
	  */
	 void addPopulation(populationTree &tree, VertexID parentID ,wsNode * parentNode,bool isParseGate)
	 {


	 	wsPopNodeSet children =getSubPop(parentNode);
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
	 				to_popNode(curChildNode,curChild,isParseGate);
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
	 			addPopulation(tree, curChildID,&curChildNode,isParseGate);
	 		}


	 }
	 /*
	  * Constructor that starts from a particular sampleNode from workspace to build a tree
	  */
	 void ws2gh(GatingHierarchy & gh, wsSampleNode curSampleNode,bool isParseGate,trans_global_vec * _gTrans,biexpTrans * _globalBiExpTrans,linTrans * _globalLinTrans)
	 {

	 	wsRootNode root=getRoot(curSampleNode);
	 	if(isParseGate)
	 	{

	 		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
	 			COUT<<endl<<"parsing compensation..."<<endl;
	 		compensation comp=getCompensation(curSampleNode);

	 		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
	 			COUT<<endl<<"parsing trans flags..."<<endl;
	 		PARAM_VEC transFlag=getTransFlag(curSampleNode);

	 		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
	 			COUT<<endl<<"parsing transformation..."<<endl;
	 		//prefixed version
	 		trans_local trans = getTransformation(root,comp,transFlag,_gTrans,_globalBiExpTrans,_globalLinTrans, true);

	 		/*
	 		 * unprefixed version. Both version of trans are added (sometime they are identical)
	 		 * so that the trans defined on uncompensated channel (e.g. SSC-A) can still be valid
	 		 * without being necessarily adding comp prefix.
	 		 * It is mainly to patch the legacy workspace of mac or win where the implicit trans is added for channel
	 		 * when its 'log' keyword is 1.
	 		 * vX doesn't have this issue since trans for each parameter/channel
	 		 * is explicitly defined in transform node.
	 		 */
	 		trans_local trans_raw=getTransformation(root,comp,transFlag,_gTrans,_globalBiExpTrans,_globalLinTrans, false);
	 		//merge raw version of trans map to theprefixed version
	 		trans_map tp = trans_raw.getTransMap();
	 		for(trans_map::iterator it=tp.begin();it!=tp.end();it++)
	 		{
	 			trans.addTrans(it->first, it->second);
	 		}
	 		gh = GatingHierarchy(comp, transFlag, trans);

	 	}

	 	if(g_loglevel>=POPULATION_LEVEL)
	 		COUT<<endl<<"parsing populations..."<<endl;

	 	populationTree &tree = gh.getTree();
	 	VertexID pVerID=addRoot(tree, root);
	 	addPopulation(tree, pVerID,&root,isParseGate);

	 }

	 GatingSet * ws2gs(vector<string> sampleIDs,bool isParseGate, StringVec sampleNames)
	 {
	 	GatingSet * gs=new GatingSet();
	 	gs->set_loglevel(g_loglevel);
	 	 /*
	 	  * parsing global calibration tables
	 	  */
	 	trans_global_vec gTrans;
	 	 if(isParseGate)
	 	 {
	 		 if(g_loglevel>=GATING_SET_LEVEL)
	 			 COUT<<"... start parsing global transformations... "<<endl;
	 		 gTrans=getGlobalTrans();

	 	 }

	 	unsigned nSample = sampleNames.size();
	 	if(nSample!=sampleIDs.size())
	 		throw(domain_error("Sizes of sampleIDs and sampleNames are not equal!"));
	 	//contruct gating hiearchy for each sampleID
	 	for(unsigned i = 0; i < nSample; i++)
	 	{
	 		string sampleID = sampleIDs.at(i);
	 		string sampleName = sampleNames.at(i);
	 		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
	 			COUT<<endl<<"... start parsing sample: "<< sampleID <<"... "<<endl;
	 		wsSampleNode curSampleNode=getSample(sampleID);

	 		GatingHierarchy & gh = gs->addGatingHierarchy(sampleName);
	 		gh.set_loglevel(g_loglevel);
	 		ws2gh(gh,curSampleNode,isParseGate,&gTrans,gs->get_globalBiExpTrans(),gs->get_globalLinTrans());

	 		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
	 			COUT<<"Gating hierarchy created: "<<sampleName<<endl;
	 	}
	 	gs->set_gTrans(gTrans);
	 	return gs;
	 }


	 wsSampleNode getSample(string sampleID){

	 		string xpath=xPathSample(sampleID);

	 		wsNode docRoot(xmlDocGetRootElement(doc));

	 		xmlXPathObjectPtr res=docRoot.xpathInNode(xpath);
	 		if(res->nodesetval->nodeNr>1)
	 		{
	 //			COUT<<sampleID<<" is not unique within this group!"<<endl;
	 			xmlXPathFreeObject(res);
	 			throw(domain_error("non-unique sampleID within the group!"));
	 		}

	 		wsSampleNode sample(res->nodesetval->nodeTab[0]);
	 		xmlXPathFreeObject(res);
	 		return sample;
	 }
};


#endif /* WORKSPACE_HPP_ */

