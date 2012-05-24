/*
 * flowJoWorkspace.cpp
 *
 *  Created on: Mar 15, 2012
 *      Author: wjiang2
 */
#include "include/flowJoWorkspace.hpp"




/*get a vector of sampleID by the given groupID
 * keep the returned results in char * format in case the non-numeric sampleID is stored
 * make sure to free the memory of xmlChar * outside of the call
 */
vector<string> flowJoWorkspace::getSampleID(unsigned short groupID)
{

		xmlXPathContextPtr context = xmlXPathNewContext(doc);
		xmlXPathObjectPtr result = xmlXPathEval((xmlChar *)nodePath.group.c_str(), context);
		if(xmlXPathNodeSetIsEmpty(result->nodesetval)){
			xmlXPathFreeObject(result);
			xmlXPathFreeContext(context);
//	                cout<<"No Groups"<<endl;;
	         throw(domain_error("No Groups infomation!"));
		}

		if(groupID<0||groupID>=result->nodesetval->nodeNr)
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
 * .sampleName may not always be stored in sampleNode
 * ,so now it is fetched from keywords/$FIL
 *  to assure correct sample name is parsed
 *  However the $FIL may not be consistent with the sampleNode "name" attribute and the physical FCS filename
 *  TODO:so extra efforts may be needed in future to avoid this conflicts
 */
string flowJoWorkspace::getSampleName(wsSampleNode & node){

//	xmlXPathObjectPtr res=node.xpathInNode("SampleNode");//get sampleNode
//	wsNode sampleNode(res->nodesetval->nodeTab[0]);
//	xmlXPathFreeObject(res);
//
//	return sampleNode.getProperty("name");//get property name from sampleNode

	xmlXPathObjectPtr res=node.xpathInNode("Keywords/Keyword[@name='$FIL']");
	if(res->nodesetval->nodeNr!=1)
		throw(domain_error("$FIL keyword not found!"));
	wsNode kwNode(res->nodesetval->nodeTab[0]);
	xmlXPathFreeObject(res);
	string filename=kwNode.getProperty("value");
	if(filename.empty())
		throw(domain_error("$FIL value is empty!"));
	return filename;
}

//bool matchName(calibrationTable calTbl){
//
//	return calTbl.name.find("Acquisition-defined")==string::npos;
//}
/*
 * get transformation for one particular sample node
 */



//xquery the "SampleNode" within "sample" context
wsRootNode flowJoWorkspace::getRoot(wsSampleNode sample)
{
//	cout<<"parsing root node"<<endl;
	xmlXPathObjectPtr res=sample.xpathInNode(nodePath.sampleNode);
	wsRootNode node(res->nodesetval->nodeTab[0]);
	xmlXPathFreeObject(res);
//	cout<<nodePath.sampleNode<<endl;
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
 * TODO:it may be more accurate to use parameter nodes to get flag for mac version
 * since keywords may not contain the $PnDISPLAY in some cases
 */
isTransMap flowJoWorkspace::getTransFlag(wsSampleNode sampleNode){
	isTransMap res;

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
		pair<string,bool> curPair;
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
		res[pName]=(curFlag.compare("LOG")==0);

		if(dMode>=GATING_SET_LEVEL)
			cout<<pName<<":"<<curFlag<<endl;
	}
	return res;
}
/*
 *Note: nodeProperties is dynamically allocated and up to caller to free it
 */
nodeProperties* flowJoWorkspace::to_popNode(wsRootNode & node){

	nodeProperties * pNode=new nodeProperties;

	pNode->setName(node.getProperty("name").c_str());

	pNode->fjStats["count"]=atoi(node.getProperty("count").c_str());

	pNode->dMode=dMode;
	return pNode;
}

nodeProperties* flowJoWorkspace::to_popNode(wsPopNode &node,bool isParseGate=false){


	nodeProperties * pNode=new nodeProperties;
	//add pop name
	pNode->setName(node.getProperty("name").c_str());

	if(dMode>=POPULATION_LEVEL)
			cout<<"parse the population Node:"<<pNode->getName()<<endl;
	//add pop counts
	pNode->fjStats["count"]=atoi(node.getProperty("count").c_str());


	try
	{
		if(isParseGate)pNode->setGate(getGate(node));
	}
	catch (int e) {
		cout<<"extracting gate failed:"<<pNode->getName()<<endl;
	}
	pNode->dMode=dMode;
	return pNode;
}
