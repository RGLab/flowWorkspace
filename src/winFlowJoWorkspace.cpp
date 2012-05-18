/*
 * winFlowJoWorkspace.cpp
 *
 *  Created on: May 15, 2012
 *      Author: wjiang2
 */

#include "include/winFlowJoWorkspace.hpp"
#include <sstream>


winFlowJoWorkspace::winFlowJoWorkspace(xmlDoc * doc){
	cout<<"windows version of flowJo workspace recognized."<<endl;
//	xpath_sample="/Workspace/SampleList/Sample/DataSet";
	nodePath.group="/Workspace/Groups/GroupNode";// abs path
	nodePath.sampleRef=".//SampleRef";//relative GroupNode
	nodePath.sample="/Workspace/SampleList/Sample";//abs path
	nodePath.sampleNode="./SampleNode";//relative to sample
	nodePath.popNode="./*/Population";//relative to sampleNode
	this->doc=doc;

}


string winFlowJoWorkspace::xPathSample(string sampleID){
			string xpath=nodePath.sample;
			xpath.append("/DataSet[@sampleID='");
			xpath.append(sampleID);
			xpath.append("']/..");
			return xpath;

}
/*
 * choose the trans from global trans vector to attach to current sample
 */
trans_local winFlowJoWorkspace::getTransformation(wsRootNode root,string cid,isTransMap transFlag,trans_global_vec * gTrans){

	trans_local res;
	unsigned sampleID=atoi(root.getProperty("sampleID").c_str());
	string sPrefix="Comp-";

	/*
	 *  save trans back to trans_local for each channel
	 */

	map<string,transformation *> *trs=&(res.transformations);
	for(trans_global_vec::iterator it=gTrans->begin();it!=gTrans->end();it++)
	{
		vector<int> sampleVec=it->sampleIDs;
		vector<int>::iterator sampleRes=find(sampleVec.begin(),sampleVec.end(),sampleID);
		/*
		 * when sampleID matched, then get trans from current global trans group
		 */
		if(sampleRes!=sampleVec.end())
		{


			for(isTransMap::iterator isTransIt=transFlag.begin();isTransIt!=transFlag.end();isTransIt++)
			{
				string curChName=isTransIt->first;
				bool curTranFlag=isTransIt->second;
				if(curTranFlag)
				{
					string curCmpChName=sPrefix+curChName;//append prefix
					transformation * curTrans=(it->trans)[curCmpChName];
					if(curTrans==NULL)
						curTrans=(it->trans)["*"];

					(*trs)[curCmpChName]=curTrans;
				}

			}

			/*
			 * assume this sampleID will not appear in any other global trans group
			 */
			break;
		}


	}


	return res;
}
isTransMap winFlowJoWorkspace::getTransFlag(wsSampleNode sampleNode){
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
	}
	return res;
}
/*
 *parsing transformations from CompensationEditor node and
 *store in global container within gs
 *
 */
trans_global_vec winFlowJoWorkspace::getGlobalTrans(){


	trans_global_vec res;


	/*
	 * get CompensationEditor node
	 */

	string path="/Workspace/CompensationEditor";
	xmlXPathContextPtr context = xmlXPathNewContext(doc);
	xmlXPathObjectPtr CompEdres = xmlXPathEval((xmlChar *)path.c_str(), context);
	if(xmlXPathNodeSetIsEmpty(CompEdres->nodesetval))
	{
		cout<<"no CompensationEditor found!"<<endl;
		xmlXPathFreeObject(CompEdres);
		xmlXPathFreeContext(context);
		return(res);
	}
	wsNode compEdNode(CompEdres->nodesetval->nodeTab[0]);

	/*
	 * parse all Compensation nodes and store them in global trans container
	 */
	path="/Workspace/CompensationEditor/Compensation";
	wsNode node(doc->children);
	xmlXPathObjectPtr compNodeRes =node.xpath(path);
	unsigned short nCompNodes=compNodeRes->nodesetval->nodeNr;
	if(nCompNodes<=0)
	{
		cout<<"compensation not found!"<<endl;
		xmlXPathFreeObject(compNodeRes);
		return(res);
	}
	for(unsigned i =0;i<nCompNodes;i++)
	{
		wsNode compNode(compNodeRes->nodesetval->nodeTab[i]);
		string compName=compNode.getProperty("name");

		trans_global curTg;

		/*
		 * parse transformations for current compNode
		 */
		map<string,transformation *> * curTransMap=&(curTg.trans);
		path=".//*[local-name()='logicle']";//get logicle(actually it is biexp)
		xmlXPathObjectPtr TransRes=compNode.xpathInNode(path);
		for(int j=0;j<TransRes->nodesetval->nodeNr;j++)
		{
			wsNode transNode(TransRes->nodesetval->nodeTab[j]);

			string pname=transNode.getProperty("parameter");
			/*
			 * when channel name is not specified
			 * try to parse it as the generic trans that is applied to channels
			 * that do not have channel-specific trans defined in workspace
			 * ,
			 */
			if(pname.empty())
				pname="*";

			string transType=(const char*)transNode.thisNode->name;
			if(transType.compare("logicle")==0)
			{

				if(dMode>=GATING_SET_LEVEL)
					cout<<"parsing logicle tranformation:"<<":"<<pname<<endl;
				biexpTrans *curTran=new biexpTrans();
				curTran->name=compName;

	//			/*TODO:detect prefix automatically instead of hard-coded
	//			 * extract channel name without prefix
	//			 */
	//			size_t nPrefix=pname.find("Comp-");
	//			if((nPrefix==string::npos))
	//				continue;

				curTran->channel=pname;
				curTran->pos=atof(transNode.getProperty("T").c_str());
				curTran->neg=atof(transNode.getProperty("w").c_str());
				curTran->widthBasis=atof(transNode.getProperty("m").c_str());
				/*
				 * calculate calibration table from the function
				 */
				curTran->computCalTbl();

				if(dMode>=GATING_SET_LEVEL)
							cout<<"spline interpolating..."<<curTran->name<<endl;
				curTran->calTbl->interpolate();
				(*curTransMap)[curTran->channel]=curTran;
			}
			else
				throw(domain_error("unknown tranformation type!"));

		}
		xmlXPathFreeObject(TransRes);
		/*
		 * parse sample list
		 */
		path="Samples/Sample";//get logicle(actually it is biexp)
		xmlXPathObjectPtr sampleRes=compNode.xpathInNode(path);
		unsigned nSample=sampleRes->nodesetval->nodeNr;
		vector<int> sampleIDs;
		for(unsigned j=0;j<nSample;j++)
		{
			wsNode curNode(sampleRes->nodesetval->nodeTab[j]);
			string curSampleID=curNode.getProperty("sampleID");
			sampleIDs.push_back(atoi(curSampleID.c_str()));
		}
		curTg.sampleIDs=sampleIDs;
		xmlXPathFreeObject(sampleRes);

		/*
		 * push the tg object to global container
		 */
		res.push_back(curTg);
	}
	xmlXPathFreeObject(compNodeRes);


	return res;
}


compensation winFlowJoWorkspace::getCompensation(wsSampleNode sampleNode)
{
	compensation comp;

	xmlXPathObjectPtr res=sampleNode.xpathInNode("*[local-name()='spilloverMatrix']");
	if(res->nodesetval->nodeNr!=1)
		throw(domain_error("not valid compensation node!"));

	wsNode node(res->nodesetval->nodeTab[0]);
	xmlXPathFreeObject(res);

	comp.cid=node.getProperty("id");
	comp.prefix=node.getProperty("prefix");
	/*
	 * -1:Acquisition-defined,to be computed from data
	 * -2:None
	 * empty:data is compensated already,spillover matrix can be read from keyword node or empty
	 * other:the spillover matrix is stored at special compensation node,
	 * 			and this cid serves as id to index that node. in pc version, we observe it is also stored at curent
	 * 			sampleNode,to keep the parsing consistent,we still look for it from the special compensation node within the context of xml root
	 */
	if(comp.cid.compare("-1")==0)
	{
		comp.comment="Acquisition-defined";
		comp.prefix="Comp-";
	}
	else if(comp.cid.compare("-2")==0)
		comp.comment="none";
	else if(comp.cid.empty())
		throw(domain_error("empty cid not supported yet!"));
	else
	{
		/*
		 * directly look for comp from spilloverMatrix node under sampleNode
		 */
		string path="*[local-name()='spillover']";
		xmlXPathObjectPtr resX=node.xpathInNode(path);

		/*
		 * deprecated:look for comp from global comp node.
		 * currently this is done by matching cid,yet it has been proved to be wrong,
		 * instead,should look for sampleNode to match sampleID
		 */
//		string path="/Workspace/CompensationEditor/Compensation[@name='"+comp.cid+"']/*[local-name()='spilloverMatrix']/*[local-name()='spillover']";
////			cout<<path<<endl;
//		xmlXPathObjectPtr resX=node.xpath(path);
		unsigned nX=resX->nodesetval->nodeNr;
		for(unsigned i=0;i<nX;i++)
		{
			wsNode curMarkerNode_X(resX->nodesetval->nodeTab[i]);
			comp.marker.push_back(curMarkerNode_X.getProperty("parameter"));
			xmlXPathObjectPtr resY=curMarkerNode_X.xpathInNode("*[local-name()='coefficient']");
			unsigned nY=resY->nodesetval->nodeNr;
			if(nX!=nY)
				throw(domain_error("not the same x,y dimensions in spillover matrix!"));
			for(unsigned j=0;j<nY;j++)
			{
				wsNode curMarkerNode_Y(resY->nodesetval->nodeTab[j]);
				string sValue=curMarkerNode_Y.getProperty("value");
				comp.spillOver.push_back(atof(sValue.c_str()));
			}
			xmlXPathFreeObject(resY);
		}
		xmlXPathFreeObject(resX);

	}
	if(dMode>=GATING_HIERARCHY_LEVEL)
			cout<<"parsing compensation matrix.."<<endl;
	return comp;
}

polygonGate* winFlowJoWorkspace::getGate(wsEllipseGateNode & node){
	throw(domain_error("ellipse gate is not supported yet"));
	return NULL;
}
polygonGate* winFlowJoWorkspace::getGate(wsPolyGateNode & node){
			polygonGate * gate=new polygonGate();
			//get the negate flag
			gate->isNegate=node.getProperty("eventsInside")=="0";

			//TODO:get parameter name(make sure the the order of x,y are correct)
			xmlXPathObjectPtr resPara=node.xpathInNode("*[local-name()='dimension']/*[local-name()='parameter']");
			int nParam=resPara->nodesetval->nodeNr;
			if(nParam!=2)
			{
//				cout<<"the dimension of the polygon gate:"<<nParam<<" is invalid!"<<endl;
				throw(domain_error("invalid dimension of the polygon gate!"));
			}
			for(int i=0;i<nParam;i++)
			{
				wsNode curPNode(resPara->nodesetval->nodeTab[i]);
				string curParam=curPNode.getProperty("name");
				gate->params.push_back(curParam);
			}
			xmlXPathFreeObject(resPara);

			//get vertices
			xmlXPathObjectPtr resVert=node.xpathInNode("*[local-name()='vertex']");
			for(int i=0;i<resVert->nodesetval->nodeNr;i++)
			{
				wsNode curVNode(resVert->nodesetval->nodeTab[i]);

				/*for each vertice node
				**get one pair of coordinates
				*/
				xmlXPathObjectPtr resCoord=curVNode.xpathInNode("*[local-name()='coordinate']");

				xmlNodeSetPtr nodeSet=resCoord->nodesetval;
				int nCoord=nodeSet->nodeNr;
				if(nCoord!=2)
				{
//					cout<<"the number of coordinates:"<<nCoord<<" is invalid!"<<endl;
					throw(domain_error("invalid  number of coordinates!"));
				}
				//get the coordinates values from the property of respective node
				coordinate pCoord;
	//			wsNode xcord(nodeSet->nodeTab[0]);
	//			string sXcord=xcord.getProperty("value");
	//			pCoord.first=atof(sXcord.c_str());
				pCoord.x=atof(wsNode(nodeSet->nodeTab[0]).getProperty("value").c_str());
				pCoord.y=atof(wsNode(nodeSet->nodeTab[1]).getProperty("value").c_str());
				//and push to the vertices vector of the gate object
				gate->vertices.push_back(pCoord);

				xmlXPathFreeObject(resCoord);
			}
			xmlXPathFreeObject(resVert);
			return gate;
}

polygonGate* winFlowJoWorkspace::getGate(wsRectGateNode & node){
			polygonGate * g=new polygonGate();
			//get the negate flag
			g->isNegate=node.getProperty("eventsInside")=="0";

			//get parameter name
			xmlXPathObjectPtr resPara=node.xpathInNode("*[local-name()='dimension']");
			int nParam=resPara->nodesetval->nodeNr;
			if(nParam!=2)
			{
//				cout<<"the dimension of the rectangle gate:"<<nParam<<" is invalid!"<<endl;
				throw(domain_error("invalid  dimension of the rectangle gate!"));
			}
			vector<pRange> r(2);
			for(int i=0;i<nParam;i++)
			{
				wsNode curPNode(resPara->nodesetval->nodeTab[i]);

				//get coordinates from properties

				string sMin=curPNode.getProperty("min");
				r.at(i).min=sMin.empty()?numeric_limits<double>::min():atof(sMin.c_str());

				string sMax=curPNode.getProperty("max");
				r.at(i).max=sMax.empty()?numeric_limits<double>::max():atof(sMax.c_str());

				//get parameter name from the children node
				xmlXPathObjectPtr resPName=curPNode.xpathInNode("*[local-name()='parameter']");
				r.at(i).name=wsNode(resPName->nodesetval->nodeTab[0]).getProperty("name");
				xmlXPathFreeObject(resPName);


			}
			/*
			 * convert pRanges to polygon data structure
			 */
			g->params.push_back(r.at(0).name);//x
			g->params.push_back(r.at(1).name);//y


			coordinate lb,lt,rb,rt;//left bottom,left top,right top,right top
			lb.x=r.at(0).min;
			lb.y=r.at(1).min;

			lt.x=r.at(0).min;
			lt.y=r.at(1).max;

			rb.x=r.at(0).max;
			rb.y=r.at(1).min;

			rt.x=r.at(0).max;
			rt.y=r.at(1).max;

			/*
			 * since rectangle gate in windows version defined differently from mac (simply as 4-point polygon)
			 * so make sure the order of vertices is correct during the conversion to polygon here
			 * lb->lt->rt->rb
			 */
			g->vertices.push_back(lb);
			g->vertices.push_back(lt);
			g->vertices.push_back(rt);
			g->vertices.push_back(rb);

			xmlXPathFreeObject(resPara);
			return g;
}
gate* winFlowJoWorkspace::getGate(wsPopNode & node){


	xmlXPathObjectPtr resGate=node.xpathInNode("Gate/*");
	wsNode gNode(resGate->nodesetval->nodeTab[0]);
	xmlXPathFreeObject(resGate);
	const xmlChar * gateType=gNode.thisNode->name;
	if(xmlStrEqual(gateType,(const xmlChar *)"PolygonGate"))
	{
		wsPolyGateNode pGNode(gNode.thisNode);
		if(dMode>=GATE_LEVEL)
			cout<<"parsing PolygonGate.."<<endl;
		return(getGate(pGNode));

	}
	else if(xmlStrEqual(gateType,(const xmlChar *)"RectangleGate"))
	{
		wsRectGateNode rGNode(gNode.thisNode);
		if(dMode>=GATE_LEVEL)
			cout<<"parsing RectangleGate.."<<endl;
		return(getGate(rGNode));
	}
	else
	{
//		cout<<"gate type: "<<gateType<<" is not supported!"<<endl;
		throw(domain_error("invalid  gate type!"));
	}

}
