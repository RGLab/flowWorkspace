/*
 * workspace.hpp
 *
 *  Created on: Mar 22, 2012
 *      Author: wjiang2
 */

#ifndef WORKSPACE_HPP_
#define WORKSPACE_HPP_
#include "workspace_type.hpp"
#include <vector>
#include <string>

#include <cytolib/GatingSet.hpp>
#include "cytolib/transformation.hpp"
#include <iostream>
#include <algorithm>
#include <fstream>


namespace flowWorkspace
{
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
	SAMPLE_NAME_LOCATION sample_name_location;//get FCS filename(or sampleName) from either $FIL keyword or name attribute of sampleNode
};


class workspace{
public:
	 xpath nodePath;


	 xmlDoc * doc;
	 wsNode doc_root;
public:
	 workspace(){doc=NULL;};
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
	 virtual string xPathSample(string sampleID)=0;
	 virtual PARAM_VEC getTransFlag(wsSampleNode sampleNode)=0;
	 virtual trans_local getTransformation(wsRootNode,const compensation &,PARAM_VEC &,const trans_global_vec &, bool prefixed)=0;
	 virtual compensation getCompensation(wsSampleNode)=0;
	 virtual trans_global_vec getGlobalTrans()=0;
	 virtual wsRootNode getRoot(wsSampleNode sampleNode)=0;
	 virtual wsPopNodeSet getSubPop(wsNode *)=0;
	 virtual gate * getGate(wsPopNode &)=0;//gate is dynamically allocated within this function,it is currently freed within gate pointer owner object nodeProperties
	 virtual void to_popNode(wsRootNode &, nodeProperties &)=0;
	 virtual void to_popNode(wsPopNode &,nodeProperties &,bool isGating)=0;
	 virtual bool is_fix_slash_in_channel_name(){return false;}
	 void toArray(string sCalTable, vector<double> &x, vector<double> &y)
	 {
		 vector<string> stringVec;
		 	boost::split(stringVec,sCalTable,boost::is_any_of(","));
		 	unsigned nLen = stringVec.size()/2;
		 	x.resize(nLen);
		 	y.resize(nLen);
		 	for(unsigned i=0;i<nLen;i++)
		 	{
		 		y[i]=atof(stringVec.at(2*i).c_str());
		 		x[i]=atof(stringVec.at(2*i + 1).c_str());
		 //		COUT<<res[i]<<",";
		 	}
	 }


	 unordered_set<string> get_derivedparameters(wsSampleNode sampleNode){
		 unordered_set<string> derived;
		 xmlXPathObjectPtr res=sampleNode.xpathInNode("*/DerivedParameter");
		 for(int i = 0; i < res->nodesetval->nodeNr; i++)
		 {
			 wsNode curP(res->nodesetval->nodeTab[i]);
			 derived.insert(curP.getProperty("name"));
		 }
		xmlXPathFreeObject(res);
		return derived;
	 }
	 /*
	  * recursively append the populations to the tree
	  * when the boolean gates are encountered before its reference nodes
	  * we still can add it as it is because gating path is stored as population names instead of actual VertexID.
	  * Thus we will deal with the the boolean gate in the actual gating process
	  */
	 void addPopulation(populationTree &tree, VertexID parentID ,wsNode * parentNode,bool isParseGate, const unordered_set<string> & derived_params)
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

	 			/*
	 			 * check if the pop uses derived parameters
	 			 */
	 			bool is_use_derived = false;
	 			gate * g = curChild.getGate();
	 			int gtype = g->getType();
	 			if(gtype!=LOGICALGATE&&gtype!=BOOLGATE&&gtype!=CLUSTERGATE)
	 			{
	 				for(const auto & p : g->getParamNames())
	 				{
	 					if(derived_params.find(p)!=derived_params.end())
	 					{
	 						is_use_derived = true;
	 						break;
	 					}
	 				}
	 			}
	 			if(is_use_derived)//skip the node that uses derived parameters
	 			{
	 				if(g_loglevel>=GATING_SET_LEVEL)
						COUT<<"skip the node that uses derived parameters:"<<curChild.getName()<<endl;
	 				boost::remove_vertex(curChildID, tree);
	 			}
	 			else
	 			{
	 				//add relation between current node and parent node
					boost::add_edge(parentID,curChildID,tree);
					//update the node map for the easy query by pop name
					//recursively add its descendants
					addPopulation(tree, curChildID,&curChildNode,isParseGate, derived_params);

	 			}
			}


	 }


	 wsSampleNode get_sample_node(string sampleID){

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

};
#endif /* WORKSPACE_HPP_ */

