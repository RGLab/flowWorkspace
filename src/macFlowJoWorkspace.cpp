/*
 * macFlowJoWorkspace.cpp
 *
 *  Created on: May 15, 2012
 *      Author: wjiang2
 */


#include "include/macFlowJoWorkspace.hpp"
const double PI  =3.141592653589793238462;

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






//bool matchName(calibrationTable calTbl){
//
//	return calTbl.name.find("Acquisition-defined")==string::npos;
//}
/*
 * get transformation for one particular sample node
 */
trans_local macFlowJoWorkspace::getTransformation(wsRootNode root,string cid,trans_vec * trans){

//	if(dMode>=GATING_HIERARCHY_LEVEL)
//			cout<<"parsing transformation..."<<endl;

	trans_local res;
	map<string,transformation *> *trs=&(res.transformations);
	if(cid.compare("-1")==0)
	{
		/*
		 * look for Acquisition-defined witin global calitbls
		 */
		for(trans_vec::iterator it=trans->begin();it!=trans->end();it++)
		{
			transformation* curTbl=*it;
			if(curTbl->name.find("Acquisition-defined")!=string::npos)
			{

				(*trs)[curTbl->channel]=curTbl;
				if(dMode>=GATING_HIERARCHY_LEVEL)
					cout<<"adding "<<curTbl->name<<":"<<curTbl->channel<<endl;
			}
		}
//		CALTBS::iterator it=find(calTbls->begin(),calTbls->end(),matchName);
//		if(it==calTbls->end())
//			throw(domain_error("can't find Acquisition-defined calibration table"));

	}
	else if(cid.compare("-2")==0)
	{

	}
	else
	{

	}
	return res;
}


trans_global_vec macFlowJoWorkspace::getGlobalTrans(){

	trans_global_vec res1;
	trans_global tg;
	trans_vec  res;

	string path="/Workspace/CalibrationTables/Table";
	xmlXPathContextPtr context = xmlXPathNewContext(doc);
	xmlXPathObjectPtr result = xmlXPathEval((xmlChar *)path.c_str(), context);
	if(xmlXPathNodeSetIsEmpty(result->nodesetval))
	{
		cout<<"no calibration Tables found!"<<endl;
		return(res);
	}
	for(int i=0;i<result->nodesetval->nodeNr;i++)
	{
		wsNode calTblNode(result->nodesetval->nodeTab[i]);

		transformation *curTran=new transformation();
		calibrationTable *t=new calibrationTable("flowJo",2);
		string tname=calTblNode.getProperty("name");
		if(tname.empty())
			throw(domain_error("empty name for calibration table"));
		/*
		 * parse the string from tname to extract channel name
		 */
		size_t nPrefix=tname.find("<");
		size_t nsuffix=tname.find(">");
		if((nPrefix==string::npos)|(nsuffix==string::npos))
			continue;//skip the tables without channel info

		curTran->name=tname.substr(0,nPrefix);
		curTran->channel=tname.substr(nPrefix,tname.length()-nPrefix);

		if(dMode>=GATING_SET_LEVEL)
				cout<<"parsing calibrationTable:"<<curTran->name<<":"<<curTran->channel<<endl;

//		t.biExpDecades=atof(calTblNode.getProperty("biexponentialDecades").c_str());
//		t.biExpNegDecades=atof(calTblNode.getProperty("biexponentialNegDecades").c_str());
//		t.w=atof(calTblNode.getProperty("biexponentialWidth").c_str());

		string sTbl=calTblNode.getContent();
		/*
		 * parse the stream to x,y double arrays
		 */
		valarray<double> tbl(toArray(sTbl));
		unsigned nX=tbl.size()/2;


		t->init(nX);

		t->y=tbl[slice(0,nX,2)];
		t->x=tbl[slice(1,nX,2)];

		/*
		 * output to text for testing
		 */
//		ofstream xOutput("../output/c++/x.csv");
//		ofstream yOutput("../output/c++/y.csv");
//		for(unsigned i=0;i<nX;i++)
//		{
//			xOutput<<t->x[i]<<",";
//			yOutput<<t->y[i]<<",";
//		}


		if(dMode>=GATING_SET_LEVEL)
			cout<<"spline interpolating..."<<curTran->name<<endl;

		t->interpolate();

		curTran->calTbl=t;

		res.push_back(curTran);

	}

	xmlXPathFreeObject(result);
	xmlXPathFreeContext(context);
	return res1;
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
				throw(domain_error("empty cid not supported yet!"));
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
		unsigned short cid=atoi(comp.cid.c_str());
		wsNode curMatNode(resMat->nodesetval->nodeTab[cid]);

		xmlXPathFreeObject(resMat);
		comp.prefix=curMatNode.getProperty("prefix");
		comp.suffix=curMatNode.getProperty("suffix");

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

	if(dMode>=GATING_HIERARCHY_LEVEL)
			cout<<"parsing compensation matrix.."<<endl;
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



gate* macFlowJoWorkspace::getGate(wsPopNode & node){


	xmlXPathObjectPtr resGate=node.xpathInNode("PolygonGate/*");
	if(resGate->nodesetval->nodeNr!=3)
	{
		xmlXPathFreeObject(resGate);
		throw(domain_error("invalid gate node(less than 3 children)"));
	}
//	wsNode pNode(resGate->nodesetval->nodeTab[0]);//gate dimensions
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
