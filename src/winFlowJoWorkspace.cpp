/*
 * winFlowJoWorkspace.cpp
 *
 *  Created on: May 15, 2012
 *      Author: wjiang2
 */

#include "include/winFlowJoWorkspace.hpp"



winFlowJoWorkspace::winFlowJoWorkspace(xmlDoc * doc){
	cout<<"windows version of flowJo workspace recognized."<<endl;
	nodePath.group="/Workspace/Groups/GroupNode";// abs path
	nodePath.sampleRef=".//SampleRef";//relative GroupNode
	nodePath.sample="/Workspace/SampleList/Sample";//abs path
	nodePath.sampleNode="./SampleNode";//relative to sample
	nodePath.popNode="./*/Population";//relative to sampleNode
	nodePath.gateDim="*[local-name()='dimension']";//relative to gateNode
	nodePath.gateParam="*[local-name()='parameter']";//relative to dimNode
	this->doc=doc;

}

xFlowJoWorkspace::xFlowJoWorkspace(xmlDoc * _doc):winFlowJoWorkspace(_doc){
	nodePath.gateParam="*[local-name()='fcs-dimension']";
}

string winFlowJoWorkspace::xPathSample(string sampleID){
			string xpath=nodePath.sample;
			xpath.append("/DataSet[@sampleID='");
			xpath.append(sampleID);
			xpath.append("']/..");
			return xpath;

}
trans_local xFlowJoWorkspace::getTransformation(wsRootNode root,const compensation & comp, PARAM_VEC & transFlag,trans_global_vec * gTrans,biexpTrans * _globalBiExpTrans,linTrans * _globalLinTrans){

	trans_local res;

	/*
	 * get transformations node
	 */
	xmlXPathObjectPtr transParentNodeRes=root.xpathInNode("../Transformations");
	unsigned short nTransParentNodes=transParentNodeRes->nodesetval->nodeNr;
	if(nTransParentNodes<=0)
	{
		cout<<"compensation not found!"<<endl;
		xmlXPathFreeObject(transParentNodeRes);
		return(res);
	}else if(nTransParentNodes>1){
		throw(domain_error("More than one 'Transformations' node found!"));
	}

	wsNode transParentNode(transParentNodeRes->nodesetval->nodeTab[0]);

	/*
	 * parse each individual transformation
	 */
	xmlXPathObjectPtr transRes=transParentNode.xpathInNode("child::*");

	trans_map curTp=res.getTransMap();

	for(int j=0;j<transRes->nodesetval->nodeNr;j++)
	{
		wsNode transNode(transRes->nodesetval->nodeTab[j]);

		//parse the parameter name
		string pname;
		xmlXPathObjectPtr paramRes=transNode.xpathInNode("*[local-name()='parameter']");
		if(paramRes->nodesetval->nodeNr!=1)
			pname="";
		else{
			wsNode paramNode(paramRes->nodesetval->nodeTab[0]);
			pname=paramNode.getProperty("name");
			}


		xmlXPathFreeObject(paramRes);
		/*
		 * when channel name is not specified
		 * try to parse it as the generic trans that is applied to channels
		 * that do not have channel-specific trans defined in workspace
		 * ,
		 */
		if(pname.empty())
			pname="*";

		string transType=(const char*)transNode.getNodePtr()->name;
		if(transType.compare("biex")==0)
		{

			if(dMode>=GATING_SET_LEVEL)
				cout<<"biex func:"<<pname<<endl;
			biexpTrans *curTran=new biexpTrans();
			curTran->setName("");
			curTran->setChannel(pname);
			curTran->pos=atof(transNode.getProperty("pos").c_str());
			curTran->neg=atof(transNode.getProperty("neg").c_str());
			curTran->widthBasis=atof(transNode.getProperty("width").c_str());
			curTran->maxValue=atof(transNode.getProperty("maxRange").c_str());
			unsigned short thisLen=atoi(transNode.getProperty("length").c_str());
			if(thisLen!=256)
				throw(domain_error("length is not 256 for biex transformation!"));
			/*
			 * do the lazy calibration table calculation and interpolation
			 * when it gets saved in gh
			 */
			curTp[curTran->getChannel()]=curTran;
		}else if(transType.compare("linear")==0){
//			if(dMode>=GATING_SET_LEVEL)
//				cout<<"flin func:"<<pname<<endl;
//			double minRange=atof(transNode.getProperty("minRange").c_str());
//			double maxRange=atof(transNode.getProperty("maxRange").c_str());
//			flinTrans *curTran=new flinTrans(minRange,maxRange);
//			curTran->setName("");
//			curTran->setChannel(pname);
//
//			curTp[curTran->getChannel()]=curTran;
			//do nothing for linear trans
		}else if(transType.compare("log")==0){
			if(dMode>=GATING_SET_LEVEL)
				cout<<"flog func:"<<pname<<endl;
			double offset=atof(transNode.getProperty("offset").c_str());
			double decade=atof(transNode.getProperty("decades").c_str());
			logTrans *curTran=new logTrans(offset,decade);
			curTran->setName("");
			curTran->setChannel(pname);

			curTp[curTran->getChannel()]=curTran;
		}
		else
			throw(domain_error("unknown tranformation type!"));

	}
	xmlXPathFreeObject(transRes);
	xmlXPathFreeObject(transParentNodeRes);

	res.setTransMap(curTp);
	return res;
}
/*
 * choose the trans from global trans vector to attach to current sample
 */
trans_local winFlowJoWorkspace::getTransformation(wsRootNode root,const compensation & comp, PARAM_VEC & transFlag,trans_global_vec * gTrans,biexpTrans * _globalBiExpTrans,linTrans * _globalLinTrans){

	trans_local res;
	unsigned sampleID=atoi(root.getProperty("sampleID").c_str());
	string sPrefix=comp.prefix;

	/*
	 *  save trans back to trans_local for each channel
	 */

	trans_map tp=res.getTransMap();
	for(trans_global_vec::iterator it=gTrans->begin();it!=gTrans->end();it++)
	{
		vector<int> sampleVec=it->getSampleIDs();
		vector<int>::iterator sampleRes=find(sampleVec.begin(),sampleVec.end(),sampleID);
		/*
		 * when sampleID matched, then get trans from current global trans group
		 */
		if(sampleRes!=sampleVec.end())
		{


			for(PARAM_VEC::iterator isTransIt=transFlag.begin();isTransIt!=transFlag.end();isTransIt++)
			{
				string curChName=isTransIt->param;
				if(isTransIt->log)
				{
					//TODO:check the logic here(compare with mac version)
					string curCmpChName=sPrefix+curChName;//append prefix
					trans_map curtp=it->getTransMap();
					trans_map::iterator resIt=curtp.find(curCmpChName);
					transformation * curTrans;
					if(resIt==curtp.end())
						curTrans=curtp["*"];
					else
						curTrans=resIt->second;

					tp[curCmpChName]=curTrans;

					cout<<curCmpChName<<":"<<curTrans->getName()<<" "<<curTrans->getChannel()<<endl;

					/*
					 * calculate calibration table from the function
					 */
					if(!curTrans->computed())
					{
						if(dMode>=GATING_SET_LEVEL)
							cout<<"computing calibration table..."<<endl;
						curTrans->computCalTbl();
					}

					if(!curTrans->isInterpolated())
					{
						if(dMode>=GATING_SET_LEVEL)
							cout<<"spline interpolating..."<<endl;
						curTrans->interpolate();
					}

				}

			}

			/*
			 * assume this sampleID will not appear in any other global trans group
			 */
			break;
		}


	}

	res.setTransMap(tp);
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
		curTg.setGroupName(compName);

		if(dMode>=GATING_SET_LEVEL)
			cout<<"group:"<<compName<<endl;
		/*
		 * parse transformations for current compNode
		 */
		trans_map curTp=curTg.getTransMap();
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

			string transType=(const char*)transNode.getNodePtr()->name;
			if(transType.compare("logicle")==0)
			{

				if(dMode>=GATING_SET_LEVEL)
					cout<<"logicle func:"<<pname<<endl;
				biexpTrans *curTran=new biexpTrans();
				curTran->setName(compName);


				curTran->setChannel(pname);
				curTran->pos=atof(transNode.getProperty("T").c_str());
				curTran->neg=atof(transNode.getProperty("w").c_str());
				curTran->widthBasis=atof(transNode.getProperty("m").c_str());
				/*
				 * do the lazy calibration table calculation and interpolation
				 * when it gets saved in gh
				 */
				curTp[curTran->getChannel()]=curTran;
			}
			else
			{
				xmlXPathFreeObject(TransRes);
				xmlXPathFreeObject(compNodeRes);
				throw(domain_error("unknown tranformation type!"));
			}


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
		curTg.setSampleIDs(sampleIDs);
		xmlXPathFreeObject(sampleRes);
		curTg.setTransMap(curTp);
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
	{
		xmlXPathFreeObject(res);
		throw(domain_error("not valid compensation node!"));
	}


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
			{
				xmlXPathFreeObject(resX);
				xmlXPathFreeObject(resY);
				throw(domain_error("not the same x,y dimensions in spillover matrix!"));
			}

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

	return comp;
}

polygonGate* winFlowJoWorkspace::getGate(wsEllipseGateNode & node){
	throw(domain_error("ellipse gate is not supported yet"));
	return NULL;
}
polygonGate* winFlowJoWorkspace::getGate(wsPolyGateNode & node){
	/*
	 * not sure what is the criteria for using ellipseGate
	 * since it is of the same format of polygonGate
	 */
	//	ellipseGate * gate=new ellipseGate();
			polygonGate * gate=new polygonGate();
			//get the negate flag
			gate->setNegate(node.getProperty("eventsInside")=="0");
			paramPoly p;
			vector<coordinate> v;
			vector<string> pn;

			//TODO:get parameter name(make sure the the order of x,y are correct)
			string polyGateDim=nodePath.gateDim+"/"+nodePath.gateParam;
			xmlXPathObjectPtr resPara=node.xpathInNode(polyGateDim);
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
				pn.push_back(curParam);
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
					xmlXPathFreeObject(resVert);
					xmlXPathFreeObject(resCoord);
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
				v.push_back(pCoord);

				xmlXPathFreeObject(resCoord);
			}
			xmlXPathFreeObject(resVert);
//			gate->setAntipodal(v);
			p.setName(pn);
			p.setVertices(v);
			gate->setParam(p);
			return gate;
}

gate * winFlowJoWorkspace::getGate(wsRectGateNode & node){
			gate * thisGate = NULL;
			//get parameter name
			xmlXPathObjectPtr resPara=node.xpathInNode(nodePath.gateDim);
			int nParam=resPara->nodesetval->nodeNr;
			/*
			 * parse the parameters
			 */
			vector<paramRange> r;
			for(int i=0;i<nParam;i++)
			{
				wsNode curPNode(resPara->nodesetval->nodeTab[i]);
				paramRange thisR;
				//get coordinates from properties
				/*
				 * be aware that numeric_limits<double>::min() return the minimum positive value
				 * instead of negative,so we have to use -max()
				 */
				string sMin=curPNode.getProperty("min");
				thisR.setMin(sMin.empty()?-numeric_limits<double>::max():atof(sMin.c_str()));

				string sMax=curPNode.getProperty("max");
				thisR.setMax(sMax.empty()?numeric_limits<double>::max():atof(sMax.c_str()));

				//get parameter name from the children node
				xmlXPathObjectPtr resPName=curPNode.xpathInNode(nodePath.gateParam);
				thisR.setName(wsNode(resPName->nodesetval->nodeTab[0]).getProperty("name"));
				xmlXPathFreeObject(resPName);
				r.push_back(thisR);

			}
			if(nParam==1){
				/*
				 * parse as rangeGate
				 */
				rangeGate * g=new rangeGate();
				if(dMode>=GATE_LEVEL)
					cout<<"constructing rangeGate.."<<endl;
				//get the negate flag
				g->setNegate(node.getProperty("eventsInside")=="0");
				g->setParam(r.at(0));
				thisGate=g;

			}else if(nParam==2){
				rectGate * g=new rectGate();
				if(dMode>=GATE_LEVEL)
					cout<<"constructing rectGate.."<<endl;
				//get the negate flag
				g->setNegate(node.getProperty("eventsInside")=="0");

				/*
				 * convert pRanges to polygon data structure
				 */
				paramPoly p;
				vector<coordinate> v;
				vector<string> pn;
				pn.push_back(r.at(0).getName());//x
				pn.push_back(r.at(1).getName());//y


				coordinate lb,lt,rb,rt;//left bottom,left top,right top,right top
				lb.x=r.at(0).getMin();
				lb.y=r.at(1).getMin();

//				lt.x=r.at(0).getMin();
//				lt.y=r.at(1).getMax();

//				rb.x=r.at(0).getMax();
//				rb.y=r.at(1).getMin();

				rt.x=r.at(0).getMax();
				rt.y=r.at(1).getMax();

				/*
				 * since rectangle gate in windows version defined differently from mac (simply as 4-point polygon)
				 * so make sure the order of vertices is correct during the conversion to polygon here
				 * lb->lt->rt->rb
				 */
				v.push_back(lb);
//				v.push_back(lt);
				v.push_back(rt);
//				v.push_back(rb);

				p.setVertices(v);
				p.setName(pn);
				g->setParam(p);
				thisGate=g;

			}else if(nParam!=2)
			{
				xmlXPathFreeObject(resPara);
				throw(domain_error("invalid  dimension of the rectangle gate!"));
			}


			xmlXPathFreeObject(resPara);


			return thisGate;
}
gate* winFlowJoWorkspace::getGate(wsPopNode & node){


	xmlXPathObjectPtr resGate=node.xpathInNode("Gate/*");
	wsNode gNode(resGate->nodesetval->nodeTab[0]);
	xmlXPathFreeObject(resGate);
	const xmlChar * gateType=gNode.getNodePtr()->name;
	if(xmlStrEqual(gateType,(const xmlChar *)"PolygonGate"))
	{
		wsPolyGateNode pGNode(gNode.getNodePtr());
		if(dMode>=GATE_LEVEL)
			cout<<"parsing PolygonGate.."<<endl;
		return(getGate(pGNode));

	}
	else if(xmlStrEqual(gateType,(const xmlChar *)"RectangleGate"))
	{
		wsRectGateNode rGNode(gNode.getNodePtr());
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
