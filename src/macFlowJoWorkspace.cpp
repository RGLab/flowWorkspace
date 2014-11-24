/*
 * macFlowJoWorkspace.cpp
 *
 *  Created on: May 15, 2012
 *      Author: wjiang2
 */


#include "include/macFlowJoWorkspace.hpp"

//#define PI 3.141592653589793238462

/*constructors of flowJoWorkspace for mac and win derived classes
 *
 */

macFlowJoWorkspace::macFlowJoWorkspace(xmlDoc * doc):flowJoWorkspace(doc){
	COUT<<"mac version of flowJo workspace recognized."<<endl;

	nodePath.popNode="./Population";

}

macFlowJoWorkspace_3::macFlowJoWorkspace_3(xmlDoc * doc):macFlowJoWorkspace(doc){

	nodePath.sample="/Workspace/Samples/Sample";

	nodePath.attrName = "nodeName";
	nodePath.compMatName = "matrixName";
	nodePath.compMatChName = "fluorName";
	nodePath.compMatVal = "spillValue";

}
string macFlowJoWorkspace::xPathSample(string sampleID){
			string xpath=nodePath.sample;
			xpath.append("[@sampleID='");
			xpath.append(sampleID);
			xpath.append("']");
			return xpath;

}

trans_global_vec::iterator findTransGroup(trans_global_vec & tGVec, string name){
	trans_global_vec::iterator it;
	for(it=tGVec.begin();it!=tGVec.end();it++)
	{
//		COUT<<it->groupName<<it->trans.size()<<endl;
		if(it->getGroupName().find(name)!=string::npos)
			break;
	}
	return it;
}

/*
 * we overwrite getTransFlag function for mac version to  use parameter nodes for log flags
 * and keywords may not contain the $PnDISPLAY in some cases
 * also we need to get transformed Range info and $PnR only contains the raw scale
 */
PARAM_VEC macFlowJoWorkspace::getTransFlag(wsSampleNode sampleNode){
	PARAM_VEC res;


	string path="Parameter";
	xmlXPathObjectPtr parRes=sampleNode.xpathInNode(path);
	unsigned short nPar=parRes->nodesetval->nodeNr;

	/*
	 * get info about whether channel should be transformed
	 */

	for(unsigned i=0;i<nPar;i++)
	{
		PARAM curParam;
		wsNode parNode(parRes->nodesetval->nodeTab[i]);

		// get curernt param name
		curParam.param=parNode.getProperty("name");

		// get current display flag
		curParam.log=parNode.getProperty("log").compare("1")==0;

		// get current range
		curParam.range=atoi(parNode.getProperty("range").c_str());

		// get current range
		curParam.highValue=atoi(parNode.getProperty("highValue").c_str());

		// get current range
		curParam.calibrationIndex=atoi(parNode.getProperty("calibrationIndex").c_str());


		if(g_loglevel>=GATING_SET_LEVEL)
			COUT<<curParam.param<<":"<<curParam.log<<":"<<curParam.range<<endl;
		res.push_back(curParam);
	}
	xmlXPathFreeObject(parRes);
	return res;
}

/*
 * get transformation for one particular sample node
 * TODO:deal with the case when cid!=-2 &version<9  & transLog==false,
 * we need to use calibrationIndex instead of comp name to search for cal tables
 *
 * also need to double check the use case :cid==-2 && version<9
 * since the current c logic may not be consistent with R code
 */
trans_local macFlowJoWorkspace::getTransformation(wsRootNode root,const compensation & comp, PARAM_VEC & transFlag,trans_global_vec * gTrans,biexpTrans * _globalBiExpTrans,linTrans * _globalLinTrans){


	trans_local res;

	string cid=comp.cid;

	/*
	 * get the pointer to the result local trans map
	 */
	trans_map trs=res.getTransMap();

	string tGName;

	trans_global_vec::iterator genericTgIt = findTransGroup(*gTrans, "Generic");
	trans_global_vec::iterator tgIt;
	if(cid.compare("-2")==0)
		tgIt=gTrans->end();//does not do the trans group name match at all
	else
	{
		/*
		 * try to look for the trans group associated with the current comp name
		 */

//		if(cid.compare("-1")==0)
//			tGName="Acquisition-defined";
//		else
			tGName=comp.name;

		tgIt=findTransGroup(*gTrans,tGName);
	}
	bool isTransGropuFound=(tgIt!=gTrans->end());
	if(isTransGropuFound)//no matched trans group
	{
		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
			COUT<<"flowJo transformation group matched:"<<tGName<<endl;

	}
	else
	{
		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
			COUT<<"no flowJo transformation group matched:"<<tGName<<endl;
	}

	for(PARAM_VEC::iterator it=transFlag.begin();it!=transFlag.end();it++)
	{
		string curChnl=it->param;
		unsigned calInd = it->calibrationIndex;
		string sCalInd = boost::lexical_cast<string>(calInd);
		string transChName;
		transformation * curTran=0;

		if(it->log)
		{
			transChName=comp.prefix+curChnl+comp.suffix;//append prefix
			/*
			 * assign source trans map from the matched trans group
			 * if no matched trans group,use the first one by default
			 */
			trans_map trans;
			if(isTransGropuFound)
			{
				trans=tgIt->getTransMap();
				/*
				 * try to search by channel name within the source map
				 */
				trans_map::iterator resIt=trans.find(transChName);
				if(resIt!=trans.end())
				{
					/*
					 * found the appropriate trans for this particular channel
					 */
					curTran=resIt->second;
					if(g_loglevel>=GATING_HIERARCHY_LEVEL)
						COUT<<transChName<<":"<<curTran->getName()<<" "<<curTran->getChannel()<<endl;
				}
				else
				{

					/*
					 * if no channel matched, try log transform
					 */
					if(it->range<=4096)
					{
						/*
						 * if no channel name matched, try 'Generic' group with calibrationIndex
						 */
						if(calInd > 0)
						{
							curTran = genericTgIt->getTran(sCalInd);
							if(g_loglevel>=GATING_HIERARCHY_LEVEL)
								COUT<<"apply transformation by calibrationIndex "<<sCalInd<<": "<<curChnl<<endl;
						}
						else
						{

							if(g_loglevel>=GATING_HIERARCHY_LEVEL)
								COUT<<"apply the biexpTrans transformation:"<<curChnl<<endl;

							/*
							 * use bioexp trans instead of logTrans
							 * since log doesn't handle the negative value well
							 */
							curTran=_globalBiExpTrans;
						}
					}

					else
					{
						string err="no valid global trans found for:";
						err.append(curChnl);
						throw(domain_error(err));
					}

				}
			}
			else
			{
				/*
				 * first use log transform for the channel that has range<=4096
				 */
				if(it->range<=4096)
				{
					/*
					 * use calibrationIndex to fetch trans from Generic tg group
					 * when comp is not defined
					 */

					if(comp.cid == "-2" && calInd > 0)
					{
						curTran = genericTgIt->getTran(sCalInd);
						if(g_loglevel>=GATING_HIERARCHY_LEVEL)
							COUT<<"apply transformation by calibrationIndex "<<sCalInd<<": "<<curChnl<<endl;
					}
					else
					{
						if(g_loglevel>=GATING_HIERARCHY_LEVEL)
							COUT<<"apply the biexpTrans transformation:"<<curChnl<<endl;

						curTran=_globalBiExpTrans;
					}


				}
				else
				{
					/*
					 * for those has range>4096,try to generic cal tables from flowJo
					 */
//					if(!gTrans->empty())
//						trans=gTrans->at(0).getTransMap();
//					/*
//					 * try to search by channel name within the source map
//					 */
//					trans_map::iterator resIt=trans.find(transChName);
//
//					/*
//					 * if no channel name matched,continue to try "Generic" group
//					 */
//					if(resIt==trans.end())
//					{
//					if(calInd > 0)
//					{
//						curTran = genericTgIt->getTran(sCalInd);
//						if(g_loglevel>=GATING_HIERARCHY_LEVEL)
//							COUT<<"apply transformation by calibrationIndex "<<sCalInd<<": "<<curChnl<<endl;
//					}
//
//					if(resIt!=trans.end())
//					{
//						/*
//						 * found the appropriate trans for this particular channel
//						 */
//						curTran=resIt->second;
//						if(g_loglevel>=GATING_HIERARCHY_LEVEL)
//							COUT<<transChName<<":"<<curTran->getName()<<" "<<curTran->getChannel()<<endl;
//					}
//					else
//					{
						string err="no valid global trans found for:";
						err.append(curChnl);
						throw(domain_error(err));
//					}

				}

			}



		}
		else
		{
			transChName=curChnl;
			int version=getVersionMin();
			if(version<9)
			{

				if(it->highValue==4096)
				{
					if(g_loglevel>=GATING_HIERARCHY_LEVEL)
						COUT<<"apply the linear transformation for gates only:"<<curChnl<<endl;
					/*
					 * some flowJo workspace somehow have highvalue inappropriately set to 4096
					 * to FSC/SSC-like channels, so here we explicitly skip them
					 */
					boost::regex ex("[FS]SC-[AWH]");
					if(!boost::regex_match(transChName,ex))
						curTran=_globalLinTrans;
				}
			}
		}
		if(curTran!=NULL)
		{
			/*
			 * assign matched global trans to the local map
			 */
			trs[transChName]=curTran;
			if(g_loglevel>=GATING_HIERARCHY_LEVEL)
				COUT<<"adding "<<curTran->getName()<<":"<<transChName<<endl;
			/*
			 * calculate and interpolate the cal table if applicable
			 */
			if(!curTran->computed())
				curTran->computCalTbl();
			if(!curTran->isInterpolated())
			{
				if(g_loglevel>=GATING_HIERARCHY_LEVEL)
					COUT<<"spline interpolating..."<<curTran->getName()<<endl;
				curTran->interpolate();

			}
		}

	}

	res.setTransMap(trs);

	return res;
}



trans_global_vec macFlowJoWorkspace::getGlobalTrans(){

	trans_global_vec tgVec;

	string path="/Workspace/CalibrationTables/Table";
	xmlXPathContextPtr context = xmlXPathNewContext(doc);
	xmlXPathObjectPtr result = xmlXPathEval((xmlChar *)path.c_str(), context);
	if(xmlXPathNodeSetIsEmpty(result->nodesetval))
	{
		COUT<<"no calibration Tables found!"<<endl;
		return(tgVec);
	}
	/*
	 * during the traversing of the calibration table list,
	 * we try to split these tables into groups by their prefix names(which should match the compensation names defined in CompensationMatrices node)
	 */
	for(int i=0;i<result->nodesetval->nodeNr;i++)
	{
		wsNode calTblNode(result->nodesetval->nodeTab[i]);

		transformation *curTran=new transformation();
		calibrationTable caltbl("flowJo",2);
		string tname=calTblNode.getProperty("name");
		if(tname.empty())
			throw(domain_error("empty name for calibration table"));

		if(g_loglevel>=GATING_SET_LEVEL)
			COUT<<"parsing calibrationTable:"<<tname<<endl;
		/*
		 * parse the string from tname to extract channel name
		 */
		size_t nPrefix=tname.find("<");
		size_t nsuffix=tname.find(">");
		bool isGeneric=(nPrefix==string::npos)|(nsuffix==string::npos);
		string transGroupName;
		if(isGeneric)
		{
			/*
			 * generic cal table (non-channel-specific)
			 */
			curTran->setName("Generic");
			//record the index of this caltbl instead
			curTran->setChannel(boost::lexical_cast<std::string>(i+1));
			transGroupName="Generic";
		}
		else
		{
			/*
			 * channel-specific cal table
			 */
			curTran->setName(boost::trim_copy((tname.substr(0,nPrefix))));
			curTran->setChannel(tname.substr(nPrefix,tname.length()-nPrefix));
			transGroupName=curTran->getName();
		}

		string sTbl=calTblNode.getContent();
		/*
		 * parse the stream to x,y double arrays
		 */
		valarray<double> tbl(toArray(sTbl));
		unsigned nX=tbl.size()/2;

		caltbl.init(nX);

		caltbl.setY(tbl[slice(0,nX,2)]);
		caltbl.setX(tbl[slice(1,nX,2)]);

		curTran->setCalTbl(caltbl);

		/*since it is base class of transformation,which means the caltbl is already given by workspace
		 * no need to compute later on. so set this flag to be true to assure the subsequent interpolation can be performed
		 */
		curTran->setComputeFlag(true);

		/*Find the respective reference(iterator) by name from the trans_global_vec
		 * If not found,push back a new entry in the vector and return its reference
		 */
		trans_global_vec::iterator tRes=findTransGroup(tgVec,curTran->getName());

		if(tRes==tgVec.end())//if not exsit yet, then push back the new instance
		{
			if(g_loglevel>=GATING_SET_LEVEL)
				COUT<<"creating new transformation group:"<<transGroupName<<endl;
			trans_global newTg;
			newTg.setGroupName(transGroupName);
			tgVec.push_back(newTg);
			tgVec.back().addTrans(curTran->getChannel(),curTran);
		}
		else
			//if already exists, then save the current transformation into the respective transGroup
			tRes->addTrans(curTran->getChannel(),curTran);

	}

	xmlXPathFreeObject(result);
	xmlXPathFreeContext(context);

	return tgVec;
}
compensation macFlowJoWorkspace::getCompensation(wsSampleNode sampleNode)
{
	compensation comp;
	comp.cid=sampleNode.getProperty("compensationID");


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
		comp.name="Acquisition-defined";
		comp.comment="Acquisition-defined";
		comp.prefix="<";
		comp.suffix=">";
	}
	else if(comp.cid.compare("-2")==0)
	{
		comp.prefix="<";
		comp.suffix=">";
		comp.comment="none";
		comp.name="none";
	}
	else if(comp.cid.empty())
	{
		/*
		 * empty cid may have gate parameters defined without prefix
		 */
		comp.cid="-2";
		comp.prefix="";
		comp.suffix="";
		comp.comment="none";
		comp.name="none";
	}
	else
	{
		/*
		 * look for CompensationMatrix nodes
		 */
		string path="/Workspace/CompensationMatrices/CompensationMatrix";
		xmlXPathObjectPtr resMat=sampleNode.xpath(path);

		if(resMat->nodesetval->nodeNr<=0)
		{
			xmlXPathFreeObject(resMat);
			throw(domain_error("no CompensationMatrix found!"));
		}

		/*
		 * look for the particular CompensationMatrix for current sampleNode by cid
		 */
		unsigned short cid=atoi(comp.cid.c_str())-1;//make sure to convert to C indexing convention.
		wsNode curMatNode(resMat->nodesetval->nodeTab[cid]);

		xmlXPathFreeObject(resMat);
		comp.prefix=curMatNode.getProperty("prefix");
		comp.suffix=curMatNode.getProperty("suffix");
		comp.name=curMatNode.getProperty(nodePath.compMatName);

		xmlXPathObjectPtr resX=curMatNode.xpathInNode("Channel");
		unsigned nX=resX->nodesetval->nodeNr;
		for(unsigned i=0;i<nX;i++)
		{
			wsNode curMarkerNode_X(resX->nodesetval->nodeTab[i]);
			comp.marker.push_back(curMarkerNode_X.getProperty("name"));
			xmlXPathObjectPtr resY=curMarkerNode_X.xpathInNode("ChannelValue");
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
				string sValue=curMarkerNode_Y.getProperty(nodePath.compMatVal);
				comp.spillOver.push_back(atof(sValue.c_str()));
			}
			xmlXPathFreeObject(resY);
		}
		xmlXPathFreeObject(resX);

	}


	return comp;
}




rangeGate* macFlowJoWorkspace::getGate(wsRangeGateNode & node){
	/*
	 * using the same routine of polygon gate to parse ellipse
	 */
	wsPolyGateNode pGNode(node.getNodePtr());
	polygonGate * g1=getGate(pGNode);
	/*
	 * convert to the rangeGate data structure after the preliminary parsing step
	 */
	rangeGate *g=new rangeGate();


	vector<coordinate> v=g1->getParam().getVertices();
	if(v.size()!=2)
			throw(domain_error("fail to convert to Range Gate since the vertices number is not 2!"));

	paramRange pRange;
	pRange.setName(g1->getParam().getNameArray().at(0));
	coordinate p1=v.at(0);
	coordinate p2=v.at(1);
	if(p1.x!=p2.x)
	{
		pRange.setMin(min(p1.x,p2.x));
		pRange.setMax(max(p1.x,p2.x));
	}
	else
	{
		pRange.setMin(min(p1.y,p2.y));
		pRange.setMax(max(p1.y,p2.y));
	}
	g->setParam(pRange);
	delete g1;
	return(g);

}




ellipseGate* macFlowJoWorkspace::getGate(wsEllipseGateNode & node){
	/*
	 * using the same routine of polygon gate to parse 4 ellipse coordinates
	 */
	wsPolyGateNode pGNode(node.getNodePtr());
	polygonGate * pg=getGate(pGNode);
	vector<coordinate> v=pg->getParam().getVertices();
	paramPoly pPoly;

	/*
	 * copy four coordinates
	 */
	if(v.size()!=4)
		throw(domain_error("invalid number of antipode pionts of ellipse gate!"));
	ellipseGate * g=new ellipseGate(v);
	pPoly.setName(pg->getParam().getNameArray());
	g->setParam(pPoly);

	delete pg;

	return(g);

}
/*
 * TODO:query gate node and param node by name instead of by positions
 */
polygonGate* macFlowJoWorkspace::getGate(wsPolyGateNode & node){
			polygonGate * gate=new polygonGate();
			/*
			 * re-fetch the children node from the current pop node
			 */
			xmlXPathObjectPtr resGate=node.xpathInNode("PolygonGate/*");
//			wsNode pNode(resGate->nodesetval->nodeTab[0]);//gate dimensions
			wsNode gNode(resGate->nodesetval->nodeTab[2]);//gate type and vertices
			xmlXPathFreeObject(resGate);

			//get the negate flag
			gate->setNegate(!gNode.getProperty("negated").empty());


			paramPoly p;
			vector<coordinate> v;
			vector<string> pn;
			string xAxis=gNode.getProperty("xAxisName");
			pn.push_back(xAxis);
			string yAxis=gNode.getProperty("yAxisName");
			if(!yAxis.empty())
				pn.push_back(yAxis);

			//get vertices
			xmlXPathObjectPtr resVert=gNode.xpathInNode("Polygon/Vertex");
			for(int i=0;i<resVert->nodesetval->nodeNr;i++)
			{
				wsNode curVNode(resVert->nodesetval->nodeTab[i]);

				/*for each vertice node
				**get one pair of coordinates
				*/

				//get the coordinates values from the property
				coordinate pCoord;
				pCoord.x=atof(curVNode.getProperty("x").c_str());
				pCoord.y=atof(curVNode.getProperty("y").c_str());
				//and push to the vertices vector of the gate object
				v.push_back(pCoord);

			}
			xmlXPathFreeObject(resVert);
			p.setVertices(v);
			p.setName(pn);
			gate->setParam(p);
			return gate;
}

/*
 * the workspace (and its derived classes) are designed to be generic in the way
 * that only talks to gatingHierarchy (and related classes) through its APIs, So that
 * gatingHierarchy is not bind to a specific xml structure, while the change of gatingHierarchy class
 * doesn't affect workspace routines.
 *
 * Thus  gatingHierarchy tree structure is invisible to this gate parsing function,
 * and we can't use the VertexID here to refer to the reference nodes for bool gate
 * instead, we store a full gating path, and  gatingHierarchy has the routine to further parse it
 * into VertexID. and this keeps the generic design of classes intact because the gating path is not
 * xml-structure specific concept.
 */
boolGate* macFlowJoWorkspace::getGate(wsBooleanGateNode & node){
	boolGate * gate=new boolGate();

	//get the negate flag
	gate->setNegate(!node.getProperty("negated").empty());

	/*
	 * get specification string
	 */
	string specs=node.getProperty("specification");

	//get string vector of gating paths
	xmlXPathObjectPtr resPaths=node.xpathInNode(".//String");
	vector<string> gPaths;
	for(int i=0;i<resPaths->nodesetval->nodeNr;i++)
	{
		wsNode curGPNode(resPaths->nodesetval->nodeTab[i]);
		gPaths.push_back(curGPNode.getContent());

	}
	xmlXPathFreeObject(resPaths);

	gate->boolOpSpec=parseBooleanSpec(specs,gPaths);

	return gate;

}

vector<BOOL_GATE_OP> macFlowJoWorkspace::parseBooleanSpec(string specs,vector<string> gPaths){

	vector<BOOL_GATE_OP> res;

	/*
	 * parse the spec strings to get logical operations among populations
	 */
//	vector<unsigned short> op_vec;
	boost::replace_all(specs,"! G","!G");
	vector<string> tokens;
	boost::split(tokens, specs, boost::is_any_of(" "));
	unsigned short nTokens=tokens.size();
	unsigned short nPopulations=(nTokens+1)/2;
	if(nPopulations!=gPaths.size())
	{
		throw(domain_error("the logical operators and the gating paths do not pair correctly!"));
	}

	vector<string> popTokens;
	vector<string> opTokens;

	popTokens.push_back(tokens.at(0));//get the first G

	for(unsigned i=1;i<nPopulations;i++)
	{
		opTokens.push_back(tokens.at(i*2-1));//operators: !, &
		popTokens.push_back(tokens.at(i*2));//like G0, G1...

	}


	for(unsigned j=0;j<nPopulations;j++)
	{

		BOOL_GATE_OP gOpObj;


		string curPopToken=popTokens.at(j);
		string curOpToken;
		if(j==0)
			curOpToken="&";//assign and operation to the first token and actually it is not used
		else
			curOpToken=opTokens.at(j-1);
		/*
		 * extract number from token
		 */
		string sIndex=boost::erase_all_copy(curPopToken,"!");
		boost::erase_all(sIndex,"G");
		unsigned short index=atoi(sIndex.c_str());
		/*
		 * select respective gating path string and split it into vector
		 */
		string curPath=gPaths.at(index);
		boost::split(gOpObj.path,curPath,boost::is_any_of("/"));
		/*
		 * the path could either one of the two types:
		 * 1) /pop1
		 * 2) /pop1/pop2
		 * the first case indicate it is a single reference node name,which requires nearest ancestor search
		 * the second is a full path or partial path of the nodes,requires a path matching
		 * EDIT:turns out the second case is also partial path.
		 */
		if(gOpObj.path.at(0).empty())
			gOpObj.path.erase(gOpObj.path.begin());//remove the first empty string


		// parse negate operator
		gOpObj.isNot=curPopToken.find("!")!=string::npos;


		/*
		 * if not |,we assume it as & by skipping the actual matching with "&"
		 * since it stores as &amp; in xml
		 */
		gOpObj.op=boost::iequals(curOpToken,"|")?'|':'&';

		/*
		 * push the parsed gating path vector and operator into result vector
		 */
		res.push_back(gOpObj);

	}
	return res;

}

gate* macFlowJoWorkspace::getGate(wsPopNode & node){

	/*
	 * try BooleanGate first
	 */
	xmlXPathObjectPtr resGate=node.xpathInNode("BooleanGate");
	if(resGate->nodesetval->nodeNr==1)
	{
		wsBooleanGateNode bGNode(resGate->nodesetval->nodeTab[0]);
		if(g_loglevel>=GATE_LEVEL)
			COUT<<"parsing BooleanGate.."<<endl;
		xmlXPathFreeObject(resGate);
		return(getGate(bGNode));

	}
	else
	{
		xmlXPathFreeObject(resGate);
	}


	/*
	 * if not BooleanGate,then try PolygonGate
	 */

	resGate=node.xpathInNode("PolygonGate/*");
	if(resGate->nodesetval->nodeNr!=3)
	{
		xmlXPathFreeObject(resGate);
		throw(domain_error("invalid gate node(less than 3 children)"));
	}

	wsNode gNode(resGate->nodesetval->nodeTab[2]);//gate type and vertices
	xmlXPathFreeObject(resGate);


	const xmlChar * gateType=gNode.getNodePtr()->name;
	if(xmlStrEqual(gateType,(const xmlChar *)"Polygon"))
	{
		wsPolyGateNode pGNode(node.getNodePtr());
		if(g_loglevel>=GATE_LEVEL)
			COUT<<"parsing PolygonGate.."<<endl;
		return(getGate(pGNode));
	}
	else if(xmlStrEqual(gateType,(const xmlChar *)"PolyRect"))//parse rect as polygon gate
	{
		wsPolyGateNode pGNode(node.getNodePtr());
		if(g_loglevel>=GATE_LEVEL)
			COUT<<"parsing RectangleGate.."<<endl;
		return(getGate(pGNode));
	}
	else if(xmlStrEqual(gateType,(const xmlChar *)"Ellipse"))
	{
		wsEllipseGateNode eGNode(node.getNodePtr());
		if(g_loglevel>=GATE_LEVEL)
			COUT<<"parsing EllipseGate.."<<endl;
		return(getGate(eGNode));
	}
	else if(xmlStrEqual(gateType,(const xmlChar *)"Range"))
	{
		wsRangeGateNode rnGNode(node.getNodePtr());

		if(g_loglevel>=GATE_LEVEL)
			COUT<<"parsing RangeGate.."<<endl;
		return(getGate(rnGNode));
	}
	else
	{
//		COUT<<"gate type: "<<gateType<<" is not supported!"<<endl;
		throw(domain_error("invalid  gate type!"));
	}
	return NULL;
}
