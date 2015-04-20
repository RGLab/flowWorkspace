/*
 * R_GatingSet.cpp
 *
 *these are R APIs
 *
 *  Created on: Mar 30, 2012
 *      Author: wjiang2
 */

/*
 * can't use module for exposing overloaded methods and non-standard wrap/as type of the constructor
 * Also each GatingHierarchy object is created by GatingSet method within c++
 * thus it is not initialized by Rcpp module as S4 class within R. So have to use this tedious way to
 * write R API
 */

#include "include/R_GatingHierarchy.hpp"
#include "include/R_GatingSet.hpp"
#include <stdexcept>
#include "include/gate.hpp"
#include "include/transformation.hpp"
using namespace std;

/*
 * only expose gating set pointer to R to avoid gc() by R
 */
RcppExport SEXP R_plotGh(SEXP _gsPtr,SEXP _sampleName,SEXP _output) {
BEGIN_RCPP



	GatingSet *	gs=getGsPtr(_gsPtr);

	string sampleName=as<string>(_sampleName);

	GatingHierarchy *gh=gs->getGatingHierarchy(sampleName);

	string output=as<string>(_output);
 	gh->drawGraph(output);

END_RCPP
}


/*
 * return node names as a character vector
 */
RcppExport SEXP R_getNodes(SEXP _gsPtr,SEXP _sampleName,SEXP _order,SEXP _fullPath, SEXP _showHidden){
BEGIN_RCPP

	GatingSet *	gs=getGsPtr(_gsPtr);

	string sampleName=as<string>(_sampleName);

	GatingHierarchy* gh=gs->getGatingHierarchy(sampleName);
	unsigned short order=as<unsigned short>(_order);
	bool fullPath=as<bool>(_fullPath);
	bool showHidden=as<bool>(_showHidden);

	return wrap(gh->getPopPaths(order,fullPath,showHidden));
END_RCPP
}

/*
 * query by path
 */
RcppExport SEXP R_getNodeID(SEXP _gsPtr,SEXP _sampleName,SEXP _gatePath){
BEGIN_RCPP

	GatingSet *	gs=getGsPtr(_gsPtr);
	string sampleName=as<string>(_sampleName);
	GatingHierarchy* gh=gs->getGatingHierarchy(sampleName);
	string gatePath=as<string>(_gatePath);
	return wrap((NODEID)gh->getNodeID(gatePath));


END_RCPP
}
RcppExport SEXP R_getParent(SEXP _gsPtr,SEXP _sampleName,SEXP _gatePath){
BEGIN_RCPP



	GatingSet *	gs=getGsPtr(_gsPtr);

	string sampleName=as<string>(_sampleName);
	GatingHierarchy *gh=gs->getGatingHierarchy(sampleName);
	string gatePath=as<string>(_gatePath);
	NODEID u = gh->getNodeID(gatePath);
	return wrap((NODEID)gh->getParent(u));
END_RCPP
}

RcppExport SEXP R_getChildren(SEXP _gsPtr,SEXP _sampleName,SEXP _gatePath, SEXP _showHidden){
BEGIN_RCPP



	GatingSet *	gs=getGsPtr(_gsPtr);
	bool showHidden = as<bool>(_showHidden);
	string sampleName=as<string>(_sampleName);
	GatingHierarchy* gh=gs->getGatingHierarchy(sampleName);
	string gatePath=as<string>(_gatePath);
	NODEID u = gh->getNodeID(gatePath);
	VertexID_vec childrenID = gh->getChildren(u);
	vector<NODEID> res;
	for(VertexID_vec::iterator it=childrenID.begin(); it!=childrenID.end();it++){
		NODEID thisNodeID = *it;
		bool isHidden = gh->getNodeProperty(thisNodeID).getHiddenFlag();
		if(showHidden||(!isHidden))
			res.push_back(thisNodeID);
	}

	return wrap(res);
END_RCPP
}

RcppExport SEXP R_getPopStats(SEXP _gsPtr,SEXP _sampleName,SEXP _gatePath){
BEGIN_RCPP



	GatingSet *	gs=getGsPtr(_gsPtr);

	string sampleName=as<string>(_sampleName);
	GatingHierarchy *gh=gs->getGatingHierarchy(sampleName);
	string gatePath=as<string>(_gatePath);
	NODEID u = gh->getNodeID(gatePath);
	nodeProperties &node=gh->getNodeProperty(u);

	return List::create(Named("FlowCore",node.getStats(true))
						,Named("FlowJo",node.getStats(false))
						);

END_RCPP
}



RcppExport SEXP R_getCompensation(SEXP _gsPtr,SEXP _sampleName){
BEGIN_RCPP



	GatingSet *	gs=getGsPtr(_gsPtr);

	string sampleName=as<string>(_sampleName);

	GatingHierarchy* gh=gs->getGatingHierarchy(sampleName);
	compensation comp=gh->getCompensation();
	return(List::create(Named("cid",comp.cid)
						,Named("prefix",comp.prefix)
						,Named("suffix",comp.suffix)
						,Named("comment",comp.comment)
						,Named("parameters",comp.marker)
						,Named("spillOver",comp.spillOver))
			);

END_RCPP
}



RcppExport SEXP R_getTransformations(SEXP _gsPtr,SEXP _sampleName, SEXP _inverse){
BEGIN_RCPP


	GatingSet *	gs=getGsPtr(_gsPtr);

	string sampleName=as<string>(_sampleName);
	bool inverse=as<bool>(_inverse);

	GatingHierarchy* gh=gs->getGatingHierarchy(sampleName);
	trans_map trans=gh->getLocalTrans().getTransMap();
	List res;

	for (trans_map::iterator it=trans.begin();it!=trans.end();it++)
	{
		transformation * curTrans=it->second;
		if(curTrans==NULL)
			throw(domain_error("empty transformation for channel"+it->first));
		boost::shared_ptr<transformation> inverseTrans;//must declare outside of if block to make it life cycle through end of loop
		if(inverse){
			inverseTrans = curTrans->getInverseTransformation();
			curTrans = inverseTrans.get();//not safe, make sure not to delete it since it belongs to shared_ptr
		}


		string transName=curTrans->getName()+" "+it->first;

		switch(curTrans->getType())
		{

			case LOG:
			{
				logTrans * thisTrans = dynamic_cast<logTrans *>(curTrans);
				res.push_back(List::create(Named("type","log")
											,Named("decade",thisTrans->decade)
											,Named("offset",thisTrans->offset)
											)
								,transName
								);
				break;
			}
			case LIN:
			{

				res.push_back(List::create(Named("type","lin"))
								,transName
								);
				break;
			}
			case CALTBL:
			{
				if(!curTrans->computed()){
					curTrans->computCalTbl();
				}
				if(!curTrans->isInterpolated()){
					curTrans->interpolate();
				}

				Spline_Coefs obj=curTrans->getSplineCoefs();

				res.push_back(List::create(Named("z",obj.coefs)
											,Named("method",obj.method)
											,Named("type", "caltbl")
											)
								,transName
								);
				break;
			}
			case BIEXP:
			{
				biexpTrans * thisTrans = dynamic_cast<biexpTrans *>(curTrans);
				/*
				 * do all the CALTBL operation
				 */
				if(!curTrans->computed()){
					curTrans->computCalTbl();
				}
				if(!curTrans->isInterpolated()){
					curTrans->interpolate();
				}

				Spline_Coefs obj=curTrans->getSplineCoefs();

				/*
				 * in addition, output the 5 arguments
				 */
				res.push_back(List::create(Named("z",obj.coefs)
											,Named("method",obj.method)
											,Named("type","biexp")
											, Named("channelRange", thisTrans->channelRange)
											, Named("maxValue", thisTrans->maxValue)
											, Named("neg", thisTrans->neg)
											, Named("pos", thisTrans->pos)
											, Named("widthBasis", thisTrans->widthBasis)
											)
								,transName
								);

				break;
			}
			case FASINH:
			{
				fasinhTrans * thisTrans = dynamic_cast<fasinhTrans *>(curTrans);

				res.push_back(List::create(Named("type","fasinh")
											, Named("A", thisTrans->A)
											, Named("M", thisTrans->M)
											, Named("T", thisTrans->T)
											, Named("length", thisTrans->length)
											, Named("maxRange", thisTrans->maxRange)
											)
											,transName
							);

				break;
			}
			default:
				throw(domain_error("unknown transformation in R_getTransformations!"));
		}

	}
	return (res);



END_RCPP
}

/*
 * compute gates(i.e. extending, adjust, transfroming) without doing the actual gating
 * mainly used for extacting gates from workspace only
 */
RcppExport SEXP R_computeGates(SEXP _gsPtr,SEXP _sampleName,SEXP _gains, SEXP _extend_val, SEXP _extend_to){
BEGIN_RCPP

	GatingSet *	gs=getGsPtr(_gsPtr);
	string sampleName=as<string>(_sampleName);
	GatingHierarchy* gh=gs->getGatingHierarchy(sampleName);
	float extend_val = as<float>(_extend_val);
	float extend_to = as<float>(_extend_to);
	map<string,float> gains;
	NumericVector gainsVec= as<NumericVector>(_gains);
	vector<string> chnlNames = gainsVec.names();
	for(vector<string>::iterator it=chnlNames.begin();it<chnlNames.end();it++){
		gains[*it]=gainsVec[*it];
	}
	gh->extendGate(extend_val, extend_to);
	gh->adjustGate(gains);
	gh->transformGate();

END_RCPP
}


RcppExport SEXP R_gating(SEXP _gsPtr,SEXP _mat,SEXP _sampleName,SEXP _gains, SEXP _nodeInd,SEXP _recompute, SEXP _extend_val, SEXP _ignore_case, SEXP _computeTerminalBool){
BEGIN_RCPP



	GatingSet *	gs=getGsPtr(_gsPtr);

	string sampleName=as<string>(_sampleName);

	unsigned short nodeInd=as<unsigned short>(_nodeInd);
	bool recompute=as<bool>(_recompute);
	bool ignore_case=as<bool>(_ignore_case);
	bool computeTerminalBool=as<bool>(_computeTerminalBool);
	GatingHierarchy* gh=gs->getGatingHierarchy(sampleName);

	Rcpp::NumericMatrix orig(_mat);
	unsigned sampleID=numeric_limits<unsigned>::max();//dummy sample index
	flowData fdata(orig,sampleID,ignore_case);

	gh->loadData(fdata);
	if(!recompute)
	{
		float extend_val = as<float>(_extend_val);

		map<string,float> gains;
		NumericVector gainsVec= as<NumericVector>(_gains);
		vector<string> chnlNames = gainsVec.names();
		for(vector<string>::iterator it=chnlNames.begin();it<chnlNames.end();it++){
			gains[*it]=gainsVec[*it];
		}
		gh->extendGate(extend_val);
		gh->adjustGate(gains);
		gh->transformGate();
		gh->transforming();
	}

	gh->gating(nodeInd,recompute, computeTerminalBool);

	if(!recompute)
	{
		/*
		 * copy the transformed data from gh before unload it
		 */
		valarray<double> updatedMat(gh->getData(0).getData());


		/*
		 * update the _mat
		 */
		for(int j=0;j<orig.ncol()*orig.nrow();j++)
			orig[j]=updatedMat[j];
	}
	gh->unloadData();

END_RCPP
}


RcppExport SEXP R_getGate(SEXP _gsPtr,SEXP _sampleName,SEXP _gatePath){
BEGIN_RCPP



	GatingSet *	gs=getGsPtr(_gsPtr);

	string sampleName=as<string>(_sampleName);
	GatingHierarchy* gh=gs->getGatingHierarchy(sampleName);
	string gatePath=as<string>(_gatePath);
	NODEID u = gh->getNodeID(gatePath);
	if(u<0)
		throw(domain_error("not valid vertexID!"));
	if(u==0)
		throw(domain_error("no gate associated with root node."));
	nodeProperties & node = gh->getNodeProperty(u);
	gate *g = node.getGate();
	string nodeName = node.getName();
	unsigned short gType=g->getType();
	if(gType==RECTGATE)
		gType=POLYGONGATE;
	switch(gType)
	{
		case ELLIPSEGATE:
				{
					ellipseGate * thisG = dynamic_cast<ellipseGate*>(g);
					coordinate mu=thisG->getMu();
					double dist=thisG->getDist();
					vector<coordinate> cov = thisG->getCovarianceMat();
					NumericMatrix covMat(2,2);
					for(unsigned i =0; i < 2; i++){
						covMat(i,0) = cov.at(i).x;
						covMat(i,1) = cov.at(i).y;
					}

					 List ret=List::create(Named("parameters",thisG->getParamNames())
							 	 	 	 	 ,Named("mu", NumericVector::create(mu.x, mu.y))
							 	 	 	 	 ,Named("cov", covMat)
							 	 	 	 	 ,Named("dist", dist)
							 	 	 	 	 ,Named("type",ELLIPSEGATE)
							 	 	 	 	 , Named("filterId", nodeName)
							 	 	 	 	 );
					return ret;
				}
		case POLYGONGATE:
			{
				vertices_vector vert=g->getVertices().toVector();

				 List ret=List::create(Named("parameters",g->getParamNames())
						 	 	 	 	 ,Named("x",vert.x),Named("y",vert.y)
						 	 	 	 	 ,Named("type",POLYGONGATE)
						 	 	 	 	 , Named("filterId", nodeName)
						 	 	 	 	 );
				return ret;
			}

		case RANGEGATE:
			{
				vertices_vector vert=g->getVertices().toVector();

				List ret=List::create(Named("parameters",g->getParamNames())
									 ,Named("range",vert.x)
									 ,Named("type",RANGEGATE)
									 , Named("filterId", nodeName)
									 );
				return ret;
			}
		case BOOLGATE:
			{
			  boolGate * bg=dynamic_cast<boolGate*>(g);
			  vector<BOOL_GATE_OP> boolOpSpec=bg->getBoolSpec();
			  vector<string> v;
			  vector<char>v2;
			  vector<vector<string> >ref;
			  for(vector<BOOL_GATE_OP>::iterator it=boolOpSpec.begin();it!=boolOpSpec.end();it++)
			  {
				  v.push_back(it->isNot?"!":"");
				  v2.push_back(it->op);
				  ref.push_back(it->path);
			  }

			  List ret=List::create(Named("v",v)
									 ,Named("v2",v2)
									 ,Named("ref",ref)
									 ,Named("type",BOOLGATE)
									 , Named("filterId", nodeName)
									 );
			  return ret;

			}
		case LOGICALGATE:
		{
			boolGate * bg=dynamic_cast<boolGate*>(g);
		  vector<BOOL_GATE_OP> boolOpSpec=bg->getBoolSpec();
		  vector<string> v;
		  vector<char>v2;
		  vector<vector<string> >ref;
		  for(vector<BOOL_GATE_OP>::iterator it=boolOpSpec.begin();it!=boolOpSpec.end();it++)
		  {
			  v.push_back(it->isNot?"!":"");
			  v2.push_back(it->op);
			  ref.push_back(it->path);
		  }

		  List ret=List::create(Named("v",v)
								 ,Named("v2",v2)
								 ,Named("ref",ref)
								 ,Named("type",LOGICALGATE)
								 , Named("filterId", nodeName)
								 );
		  return ret;

		}
		default:
		{
//			COUT<<g->getType()<<endl;
			throw(domain_error("unknown gate thrown by R_getGate!"));
		}

	}

END_RCPP
}

RcppExport SEXP R_getIndices(SEXP _gsPtr,SEXP _sampleName,SEXP _gatePath){
BEGIN_RCPP


	GatingSet *	gs=getGsPtr(_gsPtr);

	string sampleName=as<string>(_sampleName);


	GatingHierarchy* gh=gs->getGatingHierarchy(sampleName);
	string gatePath=as<string>(_gatePath);
	NODEID u = gh->getNodeID(gatePath);
	if(u<0)throw(domain_error("not valid vertexID!"));
	nodeProperties & node = gh->getNodeProperty(u);
	//gate for this particular node in case it is not gated(e.g. indices of bool gate is not archived, thus needs the lazy-gating)
	if(u>0&&!node.isGated())
		gh->calgate(u);
	return wrap(node.getIndices());

END_RCPP
}

RcppExport SEXP R_setIndices(SEXP _gsPtr,SEXP _sampleName,SEXP _i, SEXP _ind){
BEGIN_RCPP


	GatingSet *	gs=getGsPtr(_gsPtr);

	string sampleName=as<string>(_sampleName);
	int u=as<int>(_i);
	if(u<0)throw(domain_error("not valid vertexID!"));

	GatingHierarchy* gh=gs->getGatingHierarchy(sampleName);
	BoolVec ind = as<BoolVec>(_ind);
	nodeProperties & node = gh->getNodeProperty(u);
	node.setIndices(ind);
	node.computeStats();

END_RCPP
}


RcppExport SEXP R_getGateFlag(SEXP _gsPtr,SEXP _sampleName,SEXP _gatePath){
BEGIN_RCPP


	GatingSet *	gs=getGsPtr(_gsPtr);

	string sampleName=as<string>(_sampleName);
	GatingHierarchy* gh=gs->getGatingHierarchy(sampleName);
	string gatePath=as<string>(_gatePath);
	NODEID u = gh->getNodeID(gatePath);
	if(u<0)throw(domain_error("not valid vertexID!"));
	return wrap(gh->getNodeProperty(u).isGated());

END_RCPP
}

RcppExport SEXP R_getNegateFlag(SEXP _gsPtr,SEXP _sampleName,SEXP _gatePath){
BEGIN_RCPP


	GatingSet *	gs=getGsPtr(_gsPtr);

	string sampleName=as<string>(_sampleName);
	GatingHierarchy* gh=gs->getGatingHierarchy(sampleName);
	string gatePath=as<string>(_gatePath);
	NODEID u = gh->getNodeID(gatePath);
	if(u<0)throw(domain_error("not valid vertexID!"));
	return wrap(gh->getNodeProperty(u).getGate()->isNegate());

END_RCPP
}


vector<BOOL_GATE_OP> boolFilter_R_to_C(List filter){


			/*
			 * get specification from R
			 */
			StringVec refs=as<StringVec>(filter["refs"]);
			StringVec op=as<StringVec>(filter["op"]);
			BoolVec isNot=as<BoolVec>(filter["isNot"]);

			/*
			 * convert to c class
			 */
			vector<BOOL_GATE_OP> res;
			for(unsigned i=0;i<refs.size();i++)
			{

				BOOL_GATE_OP gOpObj;

				boost::split(gOpObj.path,refs.at(i),boost::is_any_of("/"));
				if(gOpObj.path.at(0).empty())
					gOpObj.path.erase(gOpObj.path.begin());//remove the first empty string

				gOpObj.isNot=isNot.at(i);
				gOpObj.op=boost::iequals(op.at(i),"|")?'|':'&';

				res.push_back(gOpObj);
			}
			return (res);
}
/*
 * convert R filter to specific gate class
 * Note: up to caller to free the dynamically allocated gate object
 */
gate * newGate(List filter){

	StringVec names=filter.names();

	unsigned short gateType=as<unsigned short>(filter["type"]);

	bool isNeg=as<bool>(filter["negated"]);
	gate * g;

	switch(gateType)
	{
		case RANGEGATE:
		{
			StringVec params=as<StringVec>(filter["params"]);
			rangeGate * rg=new rangeGate();
			rg->setNegate(isNeg);

			DoubleVec p=as<DoubleVec>(filter["range"]);

			paramRange pRange;
			pRange.setName(params.at(0));
			pRange.setMin(p.at(0));
			pRange.setMax(p.at(1));

			rg->setParam(pRange);

			g=rg;

			break;
		}
		case POLYGONGATE:
		{
			StringVec params=as<StringVec>(filter["params"]);
			polygonGate * pg=new polygonGate();

			pg->setNegate(isNeg);

			paramPoly pp;
			pp.setName(params);

			vector<coordinate> v;
			NumericMatrix boundaries=as<NumericMatrix>(filter["boundaries"]);
			for(int i=0;i<boundaries.nrow();i++)
			{
				coordinate pCoord;
				pCoord.x=boundaries(i,0);
				pCoord.y=boundaries(i,1);
				v.push_back(pCoord);

			}
			pp.setVertices(v);

			pg->setParam(pp);

			g=pg;

			break;
		}
		case RECTGATE:
		{
			StringVec params=as<StringVec>(filter["params"]);
			rectGate * rectg=new rectGate();

			rectg->setNegate(isNeg);

			paramPoly pp;
			pp.setName(params);

			vector<coordinate> v;
			NumericMatrix boundaries=as<NumericMatrix>(filter["boundaries"]);
			for(int i=0;i<boundaries.nrow();i++)
			{
				coordinate pCoord;
				pCoord.x=boundaries(i,0);
				pCoord.y=boundaries(i,1);
				v.push_back(pCoord);

			}
			pp.setVertices(v);

			rectg->setParam(pp);

			g=rectg;
			break;

		}
		case BOOLGATE:
		{
			boolGate * bg=new boolGate();

			bg->setNegate(isNeg);
			bg->boolOpSpec = boolFilter_R_to_C(filter);
			g=bg;
			break;

		}
		case LOGICALGATE:
		{
			logicalGate * lg = new logicalGate();
			lg->setNegate(isNeg);
			g = lg;
			break;
		}
		case ELLIPSEGATE:
		{



			//parse the mean
			DoubleVec mean=as<DoubleVec>(filter["mu"]);
			coordinate mu(mean.at(0), mean.at(1));
			double dist = as<double>(filter["dist"]);

			//parse cov mat
			vector<coordinate> cov;
			NumericMatrix covMat=as<NumericMatrix>(filter["cov"]);
			for(int i=0;i<covMat.nrow();i++)
			{
				coordinate p;
				p.x=covMat(i,0);
				p.y=covMat(i,1);
				cov.push_back(p);

			}

			ellipseGate * eg = new ellipseGate(mu, cov,dist);
			eg->setNegate(isNeg);

			// parse the parameter names
			StringVec params=as<StringVec>(filter["params"]);
			paramPoly pp;
			pp.setName(params);
			eg->setParam(pp);

			g=eg;

			break;
		}
		default:
			throw(domain_error("unsupported gate type!valid types: POLYGONGATE(1),RANGEGATE(2),BOOLGATE(3),RECTGATE(5),LOGICALGATE(6)"));

	}
	g->setTransformed(TRUE);
	return g;

}

RcppExport SEXP R_addGate(SEXP _gsPtr,SEXP _sampleName,SEXP _filter,SEXP _gatePath,SEXP _popName) {
BEGIN_RCPP


		GatingSet *	gs=getGsPtr(_gsPtr);
		string sampleName=as<string>(_sampleName);
		GatingHierarchy* gh=gs->getGatingHierarchy(sampleName);

		string gatePath=as<string>(_gatePath);
		NODEID u = gh->getNodeID(gatePath);
		string popName=as<string>(_popName);
		List filter=as<List>(_filter);
		gate * g=newGate(filter);


		VertexID nodeID=gh->addGate(g,u,popName);


		return wrap((NODEID)nodeID);



END_RCPP
}
/**
 * mainly used for openCyto rectRef gate which first being added as a rectangle gate
 * and then gated as boolean filter
 */
RcppExport SEXP R_boolGating(SEXP _gsPtr,SEXP _sampleName,SEXP _filter,SEXP _nodeID) {
BEGIN_RCPP


		GatingSet *	gs=getGsPtr(_gsPtr);
		string sampleName=as<string>(_sampleName);
		GatingHierarchy* gh=gs->getGatingHierarchy(sampleName);

		unsigned nodeID=as<unsigned>(_nodeID);

		List filter=as<List>(_filter);

		nodeProperties & node=gh->getNodeProperty(nodeID);
		//parse boolean expression from R data structure into c++
		vector<BOOL_GATE_OP> boolOp = boolFilter_R_to_C(filter);
		//perform bool gating
		vector<bool> curIndices= gh->boolGating(boolOp, true);
		//combine with parent indices
		nodeProperties & parentNode=gh->getNodeProperty(gh->getParent(nodeID));
		transform (curIndices.begin(), curIndices.end(), parentNode.getIndices().begin(), curIndices.begin(),logical_and<bool>());
		//save the indices
		node.setIndices(curIndices);
		node.computeStats();

END_RCPP
}


RcppExport SEXP R_setGate(SEXP _gsPtr,SEXP _sampleName,SEXP _gatePath,SEXP _filter) {
BEGIN_RCPP


		GatingSet *	gs=getGsPtr(_gsPtr);
		string sampleName=as<string>(_sampleName);
		GatingHierarchy* gh=gs->getGatingHierarchy(sampleName);

		string gatePath=as<string>(_gatePath);
		NODEID u = gh->getNodeID(gatePath);

		List filter=as<List>(_filter);

		gate * g=newGate(filter);

		nodeProperties & node=gh->getNodeProperty(u);
		gate * old_g = node.getGate();
		delete old_g;
		old_g=NULL;
		node.setGate(g);

END_RCPP
}

RcppExport SEXP R_removeNode(SEXP _gsPtr,SEXP _sampleName,SEXP _gatePath) {
BEGIN_RCPP


		GatingSet *	gs=getGsPtr(_gsPtr);
		string sampleName=as<string>(_sampleName);
		GatingHierarchy* gh=gs->getGatingHierarchy(sampleName);


		string gatePath=as<string>(_gatePath);
		NODEID u = gh->getNodeID(gatePath);

		gh->removeNode(u);

END_RCPP
}

RcppExport SEXP R_setNodeName(SEXP _gsPtr,SEXP _sampleName,SEXP _gatePath, SEXP _newNodeName) {
BEGIN_RCPP


		GatingSet *	gs=getGsPtr(_gsPtr);
		string sampleName=as<string>(_sampleName);
		string newNodeName=as<string>(_newNodeName);
		GatingHierarchy* gh=gs->getGatingHierarchy(sampleName);


		string gatePath=as<string>(_gatePath);
		NODEID u = gh->getNodeID(gatePath);

		nodeProperties &node=gh->getNodeProperty(u);
		node.setName(newNodeName.c_str());


END_RCPP
}

RcppExport SEXP R_setNodeFlag(SEXP _gsPtr,SEXP _sampleName,SEXP _gatePath, SEXP _hidden) {
BEGIN_RCPP


		GatingSet *	gs=getGsPtr(_gsPtr);
		string sampleName=as<string>(_sampleName);
		bool hidden=as<bool>(_hidden);
		GatingHierarchy* gh=gs->getGatingHierarchy(sampleName);


		string gatePath=as<string>(_gatePath);
		NODEID u = gh->getNodeID(gatePath);

		nodeProperties &node=gh->getNodeProperty(u);
		node.setHiddenFlag(hidden);


END_RCPP
}

RcppExport SEXP R_getSingleCellExpression(SEXP _gsPtr,SEXP _sampleName,SEXP _nodeIDs, SEXP _data, SEXP _markers, SEXP _threshold) {
BEGIN_RCPP

	//get indices from each node
	GatingSet *	gs=getGsPtr(_gsPtr);
	string sampleName=as<string>(_sampleName);
	GatingHierarchy* gh=gs->getGatingHierarchy(sampleName);

	bool threshold = as<bool>(_threshold);

	NODEID_vec nodeIDs = as<NODEID_vec>(_nodeIDs);
	unsigned nNodes = nodeIDs.size();
	vector<BoolVec> indexList(nNodes);
	for(unsigned i =0; i < nNodes; i++){
		VertexID u = nodeIDs.at(i);
		indexList.at(i)=gh->getNodeProperty(u).getIndices();
	}


	// or operation among these indices
	NumericMatrix data = as<NumericMatrix>(_data);
	unsigned n = data.nrow();
	unsigned k = data.ncol();
	if(k!= nNodes && threshold)
		stop("when 'threshold = TRUE' , the number of nodes and the columns of the input data matrix must be the same!");
	BoolVec ind = indexList.at(0);
	for(vector<BoolVec>::iterator it = indexList.begin() + 1; it!=indexList.end(); it++)
		transform (ind.begin(), ind.end(), it->begin(), ind.begin(),logical_or<bool>());

	// grab and mask those rows
	int lgl_n = count(ind.begin(),ind.end(),true);
	NumericMatrix output(lgl_n, k);
	int counter = 0;
	for (unsigned i=0; i < n; ++i) {
	if (ind.at(i)) {
	  for (unsigned j=0; j < k; ++j) {
		  if(threshold){//if threshold is true, then only record the intensity that is above the gate threshold
			  if (indexList.at(j).at(i))
				  output(counter, j) = data(i, j);
		  }
		  else
		  {
			  output(counter, j) = data(i, j);
		  }

	  }
	  ++counter;
	}
	}


	CharacterVector markers = as<CharacterVector>(_markers);
	Rcpp::List dimnms =  Rcpp::List::create(CharacterVector::create(),markers);

	output.attr("dimnames") = dimnms;

	return output;
END_RCPP
}
