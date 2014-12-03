/*
 * flowJoWorkspace.cpp
 *
 *  Created on: Mar 15, 2012
 *      Author: wjiang2
 */
#include "include/flowJoWorkspace.hpp"



flowJoWorkspace::flowJoWorkspace(xmlDoc * doc){

	nodePath.group="/Workspace/Groups/GroupNode";// abs path
	nodePath.sampleRef=".//SampleRef";//relative GroupNode
	nodePath.sample="/Workspace/SampleList/Sample";//abs path
	nodePath.sampleNode="./SampleNode";//relative to sample

	nodePath.attrName = "name";
	nodePath.compMatName = "name";
	nodePath.compMatChName = "name";
	nodePath.compMatVal = "value";

	this->doc=doc;

}

/*get a vector of sampleID by the given groupID
 * keep the returned results in char * format in case the non-numeric sampleID is stored
 * make sure to free the memory of xmlChar * outside of the call
 *
 * used by GatingSet::parseWorkspace(unsigned short groupID,bool isParseGate)
 */
vector<string> flowJoWorkspace::getSampleID(unsigned short groupID)
{

		xmlXPathContextPtr context = xmlXPathNewContext(doc);
		xmlXPathObjectPtr result = xmlXPathEval((xmlChar *)nodePath.group.c_str(), context);
		if(xmlXPathNodeSetIsEmpty(result->nodesetval)){
			xmlXPathFreeObject(result);
			xmlXPathFreeContext(context);
//	                COUT<<"No Groups"<<endl;;
	         throw(domain_error("No Groups infomation!"));
		}

		if(groupID== 0||groupID>=result->nodesetval->nodeNr)
		{
			xmlXPathFreeObject(result);
			xmlXPathFreeContext(context);
			 throw(invalid_argument("invalid GroupID provided!"));
		}
		xmlNodePtr cur=result->nodesetval->nodeTab[groupID];
		context->node=cur;
		xmlXPathObjectPtr sids=xmlXPathEval((xmlChar *)nodePath.sampleRef.c_str(),context);
		vector<string> sampleID;
		xmlNodeSetPtr nodeSet=sids->nodesetval;
		int size=nodeSet->nodeNr;

		for(int i=0;i<size;i++)
		{
			xmlNodePtr curNode=nodeSet->nodeTab[i];
			xmlChar * curSampleID= xmlGetProp(curNode,(xmlChar *)"sampleID");
			//to avoid memory leaking,store a copy of returned characters in string vector so that the dynamically allocated memory
			//can be freed right away in stead of later on.
			string sSampleID=(const char *)curSampleID;
			sampleID.push_back(sSampleID.c_str());
			xmlFree(curSampleID);
		}
//			;

		xmlXPathFreeObject (result);
		xmlXPathFreeContext(context);
		xmlXPathFreeObject (sids);
		return(sampleID);
}



/*
 * .
 * By default sampleName is fetched from keywords/$FIL
 *  However the $FIL may not be consistent with the sampleNode "name" attribute
 *   and the physical FCS filename,so optionally we allow parsing it
 *   from name attribute from SampleNode
 *
 */
string flowJoWorkspace::getSampleName(wsSampleNode & node){
	string filename;
	switch(nodePath.sampNloc)
	{
		case 1:
		{
			xmlXPathObjectPtr res=node.xpathInNode("Keywords/Keyword[@name='$FIL']");
			if(res->nodesetval->nodeNr!=1){
				xmlXPathFreeObject(res);
				throw(domain_error("$FIL keyword not found!"));
			}

			wsNode kwNode(res->nodesetval->nodeTab[0]);
			xmlXPathFreeObject(res);
			filename=kwNode.getProperty("value");
			break;
		}
		case 2:
		{
			xmlXPathObjectPtr res=node.xpathInNode("SampleNode");//get sampleNode
			wsNode sampleNode(res->nodesetval->nodeTab[0]);
			xmlXPathFreeObject(res);

			filename=sampleNode.getProperty(nodePath.attrName);//get property name from sampleNode
			break;
		}

		default:
			throw(domain_error("unknown sampleName Location!It should be either 'keyword' or 'sampleNode'."));
	}

	if(filename.empty())
		throw(domain_error("$FIL value is empty!"));
	return filename;
}

/*
 * get transformation for one particular sample node
 */



//xquery the "SampleNode" within "sample" context
wsRootNode flowJoWorkspace::getRoot(wsSampleNode sample)
{
//	COUT<<"parsing root node"<<endl;
	xmlXPathObjectPtr res=sample.xpathInNode(nodePath.sampleNode);
	wsRootNode node(res->nodesetval->nodeTab[0]);
	xmlXPathFreeObject(res);
//	COUT<<nodePath.sampleNode<<endl;
	return node;
}


wsPopNodeSet flowJoWorkspace::getSubPop(wsNode * node)
{

	xmlXPathObjectPtr res=node->xpathInNode(nodePath.popNode);
	int nChildren=res->nodesetval->nodeNr;
//	wsPopNodeSet childenNodes(res->nodesetval->nodeTab,nChildren);
	wsPopNodeSet childenNodes;
	for(int i=0;i<nChildren;i++)
	{
		childenNodes.push_back(wsPopNode(res->nodesetval->nodeTab[i]));
	}

	xmlXPathFreeObject(res);

	return childenNodes;

}

/*
 * this is for windows version currently because windows version does not have parameter nodes,
 * Not sure whether to get "Range" info for windows though
 *
 *
 */
PARAM_VEC flowJoWorkspace::getTransFlag(wsSampleNode sampleNode){
	PARAM_VEC res;

	/*
	 * get total number of channels
	 */
	string path="Keywords/*[@name='$PAR']";
	xmlXPathObjectPtr parRes=sampleNode.xpathInNode(path);
	wsNode parNode(parRes->nodesetval->nodeTab[0]);
	xmlXPathFreeObject(parRes);
	unsigned short nPar=atoi(parNode.getProperty("value").c_str());

	/*
	 * get info about whether channel should be transformed
	 */

	for(unsigned i=1;i<=nPar;i++)
	{
		PARAM curParam;

		/*
		 * get curernt param name
		 */
		stringstream ss(stringstream::in | stringstream::out);
		ss << "Keywords/*[@name='$P"<< i<<"N']";
		path=ss.str();
		xmlXPathObjectPtr parN=sampleNode.xpathInNode(path);
		wsNode curPNode(parN->nodesetval->nodeTab[0]);
		xmlXPathFreeObject(parN);
		string pName=curPNode.getProperty("value");

		/*
		 * get current display flag
		 */
		stringstream ss1(stringstream::in | stringstream::out);
		ss1 << "Keywords/*[@name='P"<<i<<"DISPLAY']";
		path=ss1.str();
		xmlXPathObjectPtr parDisplay=sampleNode.xpathInNode(path);
		wsNode curDisplayNode(parDisplay->nodesetval->nodeTab[0]);
		xmlXPathFreeObject(parDisplay);
		string curFlag=curDisplayNode.getProperty("value");

		curParam.param=pName;
		curParam.log=curFlag.compare("LOG")==0;
		curParam.range=4096;//not sure how to get this value from win workspaces

		if(g_loglevel>=GATING_SET_LEVEL)
			COUT<<pName<<":"<<curFlag<<endl;
		res.push_back(curParam);
	}
	return res;
}
/*
 *Note: nodeProperties is dynamically allocated and up to caller to free it
 */
void flowJoWorkspace::to_popNode(wsRootNode & node, nodeProperties & np){



	/*
	 * in order to make the pop names comparable accross samples
	 * force the root node name as "root" (it was stored as fcs filenames in some fj ws)
	 */

	np.setName("root");

	POPSTATS fjStats;
	fjStats["count"]=atoi(node.getProperty("count").c_str());
	np.setStats(fjStats,false);


}

void flowJoWorkspace::to_popNode(wsPopNode &node,nodeProperties & np,bool isParseGate=false){



	//add pop name
	np.setName(node.getProperty(nodePath.attrName).c_str());

	if(g_loglevel>=POPULATION_LEVEL)
			COUT<<"parse the population Node:"<<np.getName()<<endl;
	//add pop counts
	POPSTATS fjStats;
	string sCount = node.getProperty("count");
	//set the empty stats to -1
	fjStats["count"] = sCount.empty()?-1:atoi(sCount.c_str());
	np.setStats(fjStats,false);

	try
	{
		if(isParseGate)np.setGate(getGate(node));
	}
	catch (int e) {
		COUT<<"extracting gate failed:"<<np.getName()<<endl;
	}


}

void flowJoWorkspace::parseVersionList(){
	wsNode root(this->doc->children);
	xmlXPathObjectPtr res = root.xpath("/Workspace");
	wsNode curNode(res->nodesetval->nodeTab[0]);
	xmlXPathFreeObject(res);
	this->versionList=curNode.getProperty("versionList");

}
/*
 * get the minimum initial digit from the version list string
 */
unsigned short flowJoWorkspace::getVersionMin(){
	int res=numeric_limits<int>::max();
	vector<string> vlist;
	boost::split(vlist,versionList,boost::is_any_of(";"));
	for(vector<string>::iterator it=vlist.begin();it!=vlist.end();it++)
	{
		string curVer=*it;
		boost::erase_all(curVer,"Pre");
		vector<string> digits;
		boost::split(digits,curVer,boost::is_any_of("."));
		curVer=digits.at(0).c_str();
		boost::algorithm::trim((curVer));
		if(!curVer.empty())
		{
			int toCompare=boost::lexical_cast<int>(curVer);
			res=min(res,toCompare);
		}

	}
	return res;
}
