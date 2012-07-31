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

macFlowJoWorkspace::macFlowJoWorkspace(xmlDoc * doc){
	cout<<"mac version of flowJo workspace recognized."<<endl;

	nodePath.group="/Workspace/Groups/GroupNode";
	nodePath.sampleRef=".//SampleRef";
	nodePath.sample="/Workspace/SampleList/Sample";
	nodePath.sampleNode="./SampleNode";
	nodePath.popNode="./Population";
	this->doc=doc;

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
//		cout<<it->groupName<<it->trans.size()<<endl;
		if(it->groupName.compare(name)==0)
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

		if(dMode>=GATING_SET_LEVEL)
			cout<<curParam.param<<":"<<curParam.log<<":"<<curParam.range<<endl;
		res.push_back(curParam);
	}
	xmlXPathFreeObject(parRes);
	return res;
}

/*
 * get transformation for one particular sample node
 */
trans_local macFlowJoWorkspace::getTransformation(wsRootNode root,const compensation & comp, PARAM_VEC & transFlag,trans_global_vec * gTrans){


	trans_local res;

	string cid=comp.cid;

	/*
	 * get the pointer to the result local trans map
	 */
	map<string,transformation *> *trs=&(res.transformations);

	string tGName;
	trans_global_vec::iterator tgIt;
	if(cid.compare("-2")==0)
		tgIt=gTrans->end();//does not do the trans group name match at all
	else
	{
		/*
		 * try to look for the trans group associated with the current comp name
		 */

		if(cid.compare("-1")==0)
			tGName="Acquisition-defined";
		else
			tGName=comp.name;

		tgIt=findTransGroup(*gTrans,tGName);
	}
	bool isTransGropuFound=(tgIt!=gTrans->end());
	if(isTransGropuFound)//no matched trans group
	{
		if(dMode>=GATING_HIERARCHY_LEVEL)
			cout<<"flowJo transformation group matched:"<<tGName<<endl;

	}
	else
	{
		if(dMode>=GATING_HIERARCHY_LEVEL)
			cout<<"no flowJo transformation group matched:"<<tGName<<endl;
	}

	for(PARAM_VEC::iterator it=transFlag.begin();it!=transFlag.end();it++)
	{
		string curChnl=it->param;
		string curCmpChName=comp.prefix+curChnl+comp.suffix;//append prefix
		transformation * curTran;
		if(it->log)
		{
			/*
			 * assign source trans map from the matched trans group
			 * if no matched trans group,use the first one by default
			 */
			trans_map trans;
			if(isTransGropuFound)
			{
				trans=tgIt->trans;
				/*
				 * try to search by channel name within the source map
				 */
				trans_map::iterator resIt=trans.find(curCmpChName);
				/*
				 * if no channel name matched,continue to try "*"
				 */
				if(resIt==trans.end())
					resIt=trans.find("*");


				if(resIt!=trans.end())
				{
					/*
					 * found the appropriate trans for this particular channel
					 */
					curTran=resIt->second;
					if(dMode>=GATING_HIERARCHY_LEVEL)
						cout<<curCmpChName<<":"<<curTran->name<<" "<<curTran->channel<<endl;
				}
				else
				{

					/*
					 * if no channel matched, try log transform
					 */
					if(it->range<=4096)
					{
						if(dMode>=GATING_HIERARCHY_LEVEL)
							cout<<"apply the log10 transformation:"<<curChnl<<endl;

						curTran=new logTrans();
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
					if(dMode>=GATING_HIERARCHY_LEVEL)
						cout<<"apply the log10 transformation:"<<curChnl<<endl;

					curTran=new logTrans();
				}
				else
				{
					/*
					 * for those has range>4096,try to generic cal tables from flowJo
					 */
					if(!gTrans->empty())
						trans=gTrans->at(0).trans;
					/*
					 * try to search by channel name within the source map
					 */
					trans_map::iterator resIt=trans.find(curCmpChName);

					/*
					 * if no channel name matched,continue to try "*"
					 */
					if(resIt==trans.end())
						resIt=trans.find("*");

					if(resIt!=trans.end())
					{
						/*
						 * found the appropriate trans for this particular channel
						 */
						curTran=resIt->second;
						if(dMode>=GATING_HIERARCHY_LEVEL)
							cout<<curCmpChName<<":"<<curTran->name<<" "<<curTran->channel<<endl;
					}
					else
					{
						string err="no valid global trans found for:";
						err.append(curChnl);
						throw(domain_error(err));
					}

				}

			}


			/*
			 * assign matched global trans to the local map
			 */
			(*trs)[curCmpChName]=curTran;
			if(dMode>=GATING_HIERARCHY_LEVEL)
				cout<<"adding "<<curTran->name<<":"<<curCmpChName<<endl;
			/*
			 * calculate and interpolate the cal table if applicable
			 */
			if(!curTran->isComputed)
				curTran->computCalTbl();
			if(!curTran->calTbl.isInterpolated)
			{
				if(dMode>=GATING_HIERARCHY_LEVEL)
					cout<<"spline interpolating..."<<curTran->name<<endl;
				curTran->calTbl.interpolate();

			}
		}
	}




//		trans_map trans=tgIt->trans;
//		for(trans_map::iterator it=trans.begin();it!=trans.end();it++)
//		{
//			transformation* curTrans=it->second;
//			string curChnl=curTrans->channel;
//			PARAM_VEC::iterator paramIt=findTransFlag(transFlag,curChnl);
//			if(paramIt==transFlag.end())
//			{
//				string err="no log flag found for this parameter!";
//				err.append(curChnl);
//				throw(domain_error(err.c_str()));
//			}
//			bool isTrans=paramIt->log&(paramIt->range<=4096);
//			if(isTrans)
//			{
//				if(curTrans->name.find(tGName)!=string::npos)
//				{
//
//					(*trs)[curChnl]=curTrans;
//					if(dMode>=GATING_HIERARCHY_LEVEL)
//						cout<<"adding "<<curTrans->name<<":"<<curChnl<<endl;
//
//					if(!curTrans->isComputed)
//						curTrans->computCalTbl();
//					if(!curTrans->calTbl.isInterpolated)
//					{
//						if(dMode>=GATING_HIERARCHY_LEVEL)
//						{
//							cout<<"spline interpolating..."<<curTrans->name<<endl;
//						}
//
//						curTrans->calTbl.interpolate();
//
//					}
//				}
//				else
//				{
//					/*
//					 * TODO:try the log trans
//					 */
//					throw(domain_error("TODO:try the log trans"));
//				}
//			}
//		}






	return res;
}



trans_global_vec macFlowJoWorkspace::getGlobalTrans(){

	trans_global_vec tgVec;

	string path="/Workspace/CalibrationTables/Table";
	xmlXPathContextPtr context = xmlXPathNewContext(doc);
	xmlXPathObjectPtr result = xmlXPathEval((xmlChar *)path.c_str(), context);
	if(xmlXPathNodeSetIsEmpty(result->nodesetval))
	{
		cout<<"no calibration Tables found!"<<endl;
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

		if(dMode>=GATING_SET_LEVEL)
			cout<<"parsing calibrationTable:"<<tname<<endl;
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
			curTran->name=tname;
			curTran->channel="*";
			transGroupName="Generic";
		}
		else
		{
			/*
			 * channel-specific cal table
			 */
			curTran->name=boost::trim_copy((tname.substr(0,nPrefix)));
			curTran->channel=tname.substr(nPrefix,tname.length()-nPrefix);
			transGroupName=curTran->name;
		}

		string sTbl=calTblNode.getContent();
		/*
		 * parse the stream to x,y double arrays
		 */
		valarray<double> tbl=toArray(sTbl);
		unsigned nX=tbl.size()/2;

		caltbl.init(nX);

		caltbl.y=tbl[slice(0,nX,2)];
		caltbl.x=tbl[slice(1,nX,2)];

		curTran->calTbl=caltbl;
		/*since it is base class of transformation,which means the caltbl is already given by workspace
		 * no need to compute later on. so set this flag to be true to assure the subsequent interpolation can be performed
		 */
		curTran->isComputed=true;

		/*Find the respective reference(iterator) by name from the trans_global_vec
		 * If not found,push back a new entry in the vector and return its reference
		 */
		trans_global_vec::iterator tRes=findTransGroup(tgVec,curTran->name);

		if(tRes==tgVec.end())//if not exsit yet, then push back the new instance
		{
			if(dMode>=GATING_SET_LEVEL)
				cout<<"creating new transformation group:"<<transGroupName<<endl;
			trans_global newTg;
			newTg.groupName=transGroupName;
			tgVec.push_back(newTg);
			tgVec.back().trans[curTran->channel]=curTran;
		}
		else
			//if already exists, then save the current transformation into the respective transGroup
			tRes->trans[curTran->channel]=curTran;

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
		comp.comment="Acquisition-defined";
		comp.prefix="<";
		comp.suffix=">";
	}
	else if(comp.cid.compare("-2")==0)
		comp.comment="none";
	else if(comp.cid.empty())
		{
			comp.comment="none";
			comp.cid="-2";
		}
	else
	{
		/*
		 * look for CompensationMatrix nodes
		 */
		string path="/Workspace/CompensationMatrices/CompensationMatrix";
		xmlXPathObjectPtr resMat=sampleNode.xpath(path);

		if(resMat->nodesetval->nodeNr<=0)
			throw(domain_error("no CompensationMatrix found!"));
		/*
		 * look for the particular CompensationMatrix for current sampleNode by cid
		 */
		unsigned short cid=atoi(comp.cid.c_str())-1;//make sure to convert to C indexing convention.
		wsNode curMatNode(resMat->nodesetval->nodeTab[cid]);

		xmlXPathFreeObject(resMat);
		comp.prefix=curMatNode.getProperty("prefix");
		comp.suffix=curMatNode.getProperty("suffix");
		comp.name=curMatNode.getProperty("name");

		xmlXPathObjectPtr resX=curMatNode.xpathInNode("Channel");
		unsigned nX=resX->nodesetval->nodeNr;
		for(unsigned i=0;i<nX;i++)
		{
			wsNode curMarkerNode_X(resX->nodesetval->nodeTab[i]);
			comp.marker.push_back(curMarkerNode_X.getProperty("name"));
			xmlXPathObjectPtr resY=curMarkerNode_X.xpathInNode("ChannelValue");
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


	return comp;
}




rangegate* macFlowJoWorkspace::getGate(wsRangeGateNode & node){
	/*
	 * using the same routine of polygon gate to parse ellipse
	 */
	wsPolyGateNode pGNode(node.thisNode);
	polygonGate * g1=getGate(pGNode);
	/*
	 * convert to the rangeGate data structure after the preliminary parsing step
	 */
	rangegate *g=new rangegate();

	if(g1->vertices.size()!=2)
			throw(domain_error("fail to convert to Range Gate since the vertices number is not 2!"));


	g->param.name=g1->params.at(0);
	coordinate p1=g1->vertices.at(0);
	coordinate p2=g1->vertices.at(1);
	if(p1.x!=p2.x)
	{
		g->param.min=min(p1.x,p2.x);
		g->param.max=max(p1.x,p2.x);
	}
	else
	{
		g->param.min=min(p1.y,p2.y);
		g->param.max=max(p1.y,p2.y);
	}
	delete g1;
	return(g);

}

bool compare_x(coordinate i, coordinate j) { return i.x<j.x; }
bool compare_y(coordinate i, coordinate j) { return i.y<j.y; }


polygonGate* macFlowJoWorkspace::getGate(wsEllipseGateNode & node){
	/*
	 * using the same routine of polygon gate to parse ellipse
	 */
	wsPolyGateNode pGNode(node.thisNode);
	polygonGate * g=getGate(pGNode);
	/*
	 * using 4 vertices to fit polygon points
	 */
	vector<coordinate> v=g->vertices;
	g->vertices.clear();//reset the vertices

	coordinate R=*max_element(v.begin(),v.end(),compare_x);
	coordinate L=*min_element(v.begin(),v.end(),compare_x);

	coordinate T=*max_element(v.begin(),v.end(),compare_y);
	coordinate B=*min_element(v.begin(),v.end(),compare_y);

	coordinate E;
	E.x=hypot(L.x-R.x,L.y-R.y)/2;
	E.y=hypot(T.x-B.x,T.y-B.y)/2;

	double phi=tan((R.y-L.y)/(R.x-L.x));
	double CY=(B.y+T.y)/2;
	double CX=(R.x+L.x)/2;


	double delta=2*PI/100;
	for(unsigned short i=0;i<100;i++)
	{
		double S=i*delta;
		coordinate p;
		p.x=CX+E.x*cos(S)*cos(phi)-E.y*sin(S)*sin(phi);
		p.y=CY+E.x*cos(S)*sin(phi)+E.y*sin(S)*cos(phi);
		g->vertices.push_back(p);
	}

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
			gate->isNegate=!gNode.getProperty("negated").empty();

			//get parameter name
//			xmlXPathObjectPtr resPara=pNode.xpathInNode("StringArray/String");
//			int nParam=resPara->nodesetval->nodeNr;
//			if(nParam!=2)
//			{
////				cout<<"the dimension of the polygon gate:"<<nParam<<" is invalid!"<<endl;
//				throw(domain_error("invalid dimension of the polygon gate!"));
//			}
//			for(int i=0;i<nParam;i++)
//			{
//				wsNode curPNode(resPara->nodesetval->nodeTab[i]);
//				string curParam=curPNode.getContent();
//				gate->params.push_back(curParam);
//			}
//			xmlXPathFreeObject(resPara);


			string xAxis=gNode.getProperty("xAxisName");
			gate->params.push_back(xAxis);
			string yAxis=gNode.getProperty("yAxisName");
			if(!yAxis.empty())
				gate->params.push_back(yAxis);

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
				gate->vertices.push_back(pCoord);

			}
			xmlXPathFreeObject(resVert);
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
	gate->isNegate=!node.getProperty("negated").empty();

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
			curOpToken="&";//assign and operation to the first token
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
		boost::split(gOpObj.fullpath,curPath,boost::is_any_of("/"));

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
		if(dMode>=GATE_LEVEL)
			cout<<"parsing BooleanGate.."<<endl;
		return(getGate(bGNode));
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


	const xmlChar * gateType=gNode.thisNode->name;
	if(xmlStrEqual(gateType,(const xmlChar *)"Polygon"))
	{
		wsPolyGateNode pGNode(node.thisNode);
		if(dMode>=GATE_LEVEL)
			cout<<"parsing PolygonGate.."<<endl;
		return(getGate(pGNode));
	}
	else if(xmlStrEqual(gateType,(const xmlChar *)"PolyRect"))//parse rect as polygon gate
	{
		wsPolyGateNode pGNode(node.thisNode);
		if(dMode>=GATE_LEVEL)
			cout<<"parsing RectangleGate.."<<endl;
		return(getGate(pGNode));
	}
	else if(xmlStrEqual(gateType,(const xmlChar *)"Ellipse"))
	{
		wsEllipseGateNode eGNode(node.thisNode);
		if(dMode>=GATE_LEVEL)
			cout<<"parsing EllipseGate.."<<endl;
		return(getGate(eGNode));
	}
	else if(xmlStrEqual(gateType,(const xmlChar *)"Range"))
	{
		wsRangeGateNode rnGNode(node.thisNode);

		if(dMode>=GATE_LEVEL)
			cout<<"parsing RangeGate.."<<endl;
		return(getGate(rnGNode));
	}
	else
	{
//		cout<<"gate type: "<<gateType<<" is not supported!"<<endl;
		throw(domain_error("invalid  gate type!"));
	}
	return NULL;
}
