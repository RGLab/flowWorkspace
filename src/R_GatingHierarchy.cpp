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

#include "cytolib/GatingHierarchy.hpp"
#include "cytolib/GatingSet.hpp"
#include <stdexcept>
#include "cytolib/gate.hpp"
#include "cytolib/transformation.hpp"
using namespace std;

#include "cytolib/flowData.hpp"
#include <Rcpp.h>
using namespace Rcpp;

flowData mat2flowData(NumericMatrix mat,unsigned _sampleID, bool _ignore_case){
	flowData fd;
	List dimnames=mat.attr("dimnames");
	fd.params=as<vector<string> >(dimnames[1]);

	fd.nEvents=mat.nrow();
	fd.sampleID=_sampleID;
	fd.ignore_case = _ignore_case;

	fd.data = REAL(mat.get__());

	return fd;
}

/*
 * only expose gating set pointer to R to avoid gc() by R
 */
//[[Rcpp::export(name=".cpp_plotGh")]]
void plotGh(XPtr<GatingSet> gs,string sampleName,string output) {

	GatingHierarchy & gh=gs->getGatingHierarchy(sampleName);
  gh.drawGraph(output);

}


/*
 * return node names as a character vector
 */
//[[Rcpp::export(name=".cpp_getNodes")]]
StringVec getNodes(XPtr<GatingSet> gs,string sampleName
                  ,unsigned short order
                  ,bool fullPath
                  , bool showHidden){

	GatingHierarchy & gh=gs->getGatingHierarchy(sampleName);

	return gh.getNodePaths(order,fullPath,showHidden);

}

//[[Rcpp::export]]
string getNodePath(XPtr<GatingSet> gs,string sampleName,NODEID id){
	GatingHierarchy & gh=gs->getGatingHierarchy(sampleName);
	return gh.getNodePath(id);
}
/*
 * query by path
 */
//[[Rcpp::export(name=".cpp_getNodeID")]]
NODEID getNodeID(XPtr<GatingSet> gs,string sampleName,string gatePath){

	GatingHierarchy & gh=gs->getGatingHierarchy(sampleName);

	return (NODEID)gh.getNodeID(gatePath);

}
//[[Rcpp::export(name=".cpp_getParent")]]
NODEID getParent(XPtr<GatingSet> gs,string sampleName,string gatePath){

	GatingHierarchy & gh=gs->getGatingHierarchy(sampleName);
	NODEID u = gh.getNodeID(gatePath);
	return (NODEID)gh.getParent(u);

}

//[[Rcpp::export(name=".cpp_getChildren")]]
vector<NODEID> getChildren(XPtr<GatingSet> gs,string sampleName
                             ,string gatePath, bool showHidden){

	GatingHierarchy & gh=gs->getGatingHierarchy(sampleName);

	NODEID u = gh.getNodeID(gatePath);
	VertexID_vec childrenID = gh.getChildren(u);
	vector<NODEID> res;
	for(VertexID_vec::iterator it=childrenID.begin(); it!=childrenID.end();it++){
		NODEID thisNodeID = *it;
		bool isHidden = gh.getNodeProperty(thisNodeID).getHiddenFlag();
		if(showHidden||(!isHidden))
			res.push_back(thisNodeID);
	}

	return res;

}

//[[Rcpp::export(name=".cpp_getPopStats")]]
List getPopStats(XPtr<GatingSet> gs,string sampleName
                     ,string gatePath){

	GatingHierarchy & gh=gs->getGatingHierarchy(sampleName);
	NODEID u = gh.getNodeID(gatePath);
	nodeProperties &node=gh.getNodeProperty(u);

	return List::create(Named("FlowCore",node.getStats(true))
						,Named("FlowJo",node.getStats(false))
						);

}



//[[Rcpp::export(name=".cpp_getCompensation")]]
List getCompensation(XPtr<GatingSet> gs,string sampleName){
  GatingHierarchy & gh=gs->getGatingHierarchy(sampleName);
  compensation comp=gh.getCompensation();
	return(List::create(Named("cid",comp.cid)
						,Named("prefix",comp.prefix)
						,Named("suffix",comp.suffix)
						,Named("comment",comp.comment)
						,Named("parameters",comp.marker)
						,Named("spillOver",comp.spillOver))
			);


}



//[[Rcpp::export(name=".cpp_getTransformations")]]
List getTransformations(XPtr<GatingSet> gs,string sampleName, bool inverse){


	GatingHierarchy & gh=gs->getGatingHierarchy(sampleName);
	trans_map trans=gh.getLocalTrans().getTransMap();
	List res;

	for (trans_map::iterator it=trans.begin();it!=trans.end();it++)
	{
		transformation * curTrans=it->second;
		if(curTrans==NULL)
			throw(domain_error("empty transformation for channel"+it->first));
		if(!curTrans->gateOnly())
		{
			boost::shared_ptr<transformation> inverseTrans;//must declare outside of if block to make it life cycle through end of loop
			if(inverse){
				inverseTrans = curTrans->getInverseTransformation();
				curTrans = inverseTrans.get();//not safe, make sure not to delete it since it belongs to shared_ptr
			}

			string chnl = it->first;
	//		string transName = curTrans->getName()+" "+chnl;

			switch(curTrans->getType())
			{

				case LOG:
				{
					logTrans * thisTrans = dynamic_cast<logTrans *>(curTrans);
					res.push_back(List::create(Named("type","log")
												,Named("decade",thisTrans->decade)
												,Named("offset",thisTrans->offset)
												,Named("T",thisTrans->T)
												,Named("scale", thisTrans->scale)
												)
									,chnl
									);
					break;
				}
				case LIN:
				{

					res.push_back(List::create(Named("type","lin"))
									,chnl
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
									,chnl
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
									,chnl
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
												,chnl
								);

					break;
				}
				default:
					throw(domain_error("unknown transformation in R_getTransformations!"));
			}
		}
	}
	return (res);
}

/*
 * compute gates(i.e. extending, adjust, transfroming) without doing the actual gating
 * mainly used for extacting gates from workspace only
 */
//[[Rcpp::export(name=".cpp_computeGates")]]
void computeGates(XPtr<GatingSet> gs,string sampleName
                    ,NumericVector gainsVec
                    , float extend_val, float extend_to){

	GatingHierarchy & gh=gs->getGatingHierarchy(sampleName);

	map<string,float> gains;
	vector<string> chnlNames = gainsVec.names();
	for(vector<string>::iterator it=chnlNames.begin();it<chnlNames.end();it++){
		gains[*it]=gainsVec[*it];
	}
	gh.extendGate(extend_val, extend_to);
	gh.adjustGate(gains);
	gh.transformGate();

}


//[[Rcpp::export(name=".cpp_gating")]]
void gating(XPtr<GatingSet> gs
              , NumericMatrix orig
              ,string sampleName
              ,NumericVector gainsVec
              , unsigned short nodeInd
              ,bool recompute, float extend_val
              , bool ignore_case, bool computeTerminalBool, float timestep){

 
	GatingHierarchy & gh=gs->getGatingHierarchy(sampleName);

//	Rcpp::NumericMatrix orig(mat);
	unsigned sampleID=numeric_limits<unsigned>::max();//dummy sample index


	gh.loadData(mat2flowData(orig,sampleID,ignore_case));
	if(!recompute)
	{
	
		map<string,float> gains;
		vector<string> chnlNames = gainsVec.names();
		for(vector<string>::iterator it=chnlNames.begin();it<chnlNames.end();it++){
			gains[*it]=gainsVec[*it];
		}

		gh.adjustGate(gains);
		gh.transformGate();
		gh.transforming(timestep);
		gh.extendGate(extend_val);
	}

	gh.gating(nodeInd,recompute, computeTerminalBool);

	gh.unloadData();


}


//[[Rcpp::export(name=".cpp_getGate")]]
List getGate(XPtr<GatingSet> gs,string sampleName,string gatePath){

	GatingHierarchy & gh=gs->getGatingHierarchy(sampleName);
	NODEID u = gh.getNodeID(gatePath);
	if(u==0)
		throw(domain_error("no gate associated with root node."));
	nodeProperties & node = gh.getNodeProperty(u);
	gate *g = node.getGate();
	string nodeName = node.getName();
	unsigned short gType=g->getType();
	if(gType==RECTGATE||gType == CURLYQUADGATE)
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
			  vector<deque<string> >ref;
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
		  vector<deque<string> >ref;
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
		case CLUSTERGATE:
		{
		  clusterGate * cg=dynamic_cast<clusterGate*>(g);
		  vector<BOOL_GATE_OP> boolOpSpec=cg->getBoolSpec();
		  vector<string> v;
		  vector<char>v2;
		  vector<deque<string> >ref;
		  for(vector<BOOL_GATE_OP>::iterator it=boolOpSpec.begin();it!=boolOpSpec.end();it++)
		  {
			  v.push_back(it->isNot?"!":"");
			  v2.push_back(it->op);
			  ref.push_back(it->path);
		  }

		  List ret=List::create(Named("v",v)
								 ,Named("v2",v2)
								 ,Named("ref",ref)
								 ,Named("type",CLUSTERGATE)
								 , Named("filterId", nodeName)
								 , Named("cluster_method_name", cg->get_cluster_method_name())
								 );
		  return ret;

		}
		default:
		{
//			COUT<<g->getType()<<endl;
			throw(domain_error("unknown gate thrown by R_getGate!"));
		}

	}


}

//[[Rcpp::export(name=".cpp_getIndices")]]
vector<bool> getIndices(XPtr<GatingSet> gs,string sampleName,string gatePath){

	GatingHierarchy & gh=gs->getGatingHierarchy(sampleName);
	NODEID u = gh.getNodeID(gatePath);
	nodeProperties & node = gh.getNodeProperty(u);
	//gate for this particular node in case it is not gated(e.g. indices of bool gate is not archived, thus needs the lazy-gating)
	if(u>0&&!node.isGated())
	{
		if(node.getGate()->getType()!=BOOLGATE)
			throw(domain_error("Event indicies are not available for the ungated non-boolean node: '" + gatePath + "'. \n Please recompute it first!"));
		gh.gating(u);
	}
	return node.getIndices();


}

//[[Rcpp::export(name=".cpp_setIndices")]]
void setIndices(XPtr<GatingSet> gs,string sampleName,int u, BoolVec ind){


	if(u<0)throw(domain_error("not valid vertexID!"));

	GatingHierarchy & gh=gs->getGatingHierarchy(sampleName);

	nodeProperties & node = gh.getNodeProperty(u);
	node.setIndices(ind);
	node.computeStats();

}


//[[Rcpp::export(name=".cpp_getGateFlag")]]
bool getGateFlag(XPtr<GatingSet> gs,string sampleName,string gatePath){

	GatingHierarchy & gh=gs->getGatingHierarchy(sampleName);
	NODEID u = gh.getNodeID(gatePath);
	return gh.getNodeProperty(u).isGated();


}

//[[Rcpp::export(name=".cpp_getNegateFlag")]]
bool getNegateFlag(XPtr<GatingSet> gs,string sampleName,string gatePath){

	GatingHierarchy & gh=gs->getGatingHierarchy(sampleName);
	NODEID u = gh.getNodeID(gatePath);
	return gh.getNodeProperty(u).getGate()->isNegate();

}
//[[Rcpp::export(name=".cpp_getHiddenFlag")]]
bool getHiddenFlag(XPtr<GatingSet> gs,string sampleName,string gatePath){

	GatingHierarchy & gh=gs->getGatingHierarchy(sampleName);
	NODEID u = gh.getNodeID(gatePath);
	return gh.getNodeProperty(u).getHiddenFlag();

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
		case CLUSTERGATE:
		{
			clusterGate * cg = new clusterGate(as<string>(filter["cluster_method_name"]));
			cg->setNegate(isNeg);
			g = cg;
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

//[[Rcpp::export(name=".cpp_addGate")]]
NODEID addGate(XPtr<GatingSet> gs,string sampleName
                   ,List filter
                   ,string gatePath
                   ,string popName) {

		GatingHierarchy & gh=gs->getGatingHierarchy(sampleName);

		NODEID u = gh.getNodeID(gatePath);
		gate * g=newGate(filter);


		VertexID nodeID=gh.addGate(g,u,popName);


		return (NODEID)nodeID;

}
/**
 * mainly used for openCyto rectRef gate which first being added as a rectangle gate
 * and then gated as boolean filter
 */
//[[Rcpp::export(name=".cpp_boolGating")]]
void boolGating(XPtr<GatingSet> gs,string sampleName,List filter,unsigned nodeID) {

		GatingHierarchy & gh=gs->getGatingHierarchy(sampleName);
		nodeProperties & node=gh.getNodeProperty(nodeID);
		//parse boolean expression from R data structure into c++
		vector<BOOL_GATE_OP> boolOp = boolFilter_R_to_C(filter);
		//perform bool gating
		vector<bool> curIndices= gh.boolGating(boolOp, true);
		//combine with parent indices
		nodeProperties & parentNode=gh.getNodeProperty(gh.getParent(nodeID));
		transform (curIndices.begin(), curIndices.end(), parentNode.getIndices().begin(), curIndices.begin(),logical_and<bool>());
		//save the indices
		node.setIndices(curIndices);
		node.computeStats();


}


//[[Rcpp::export(name=".cpp_setGate")]]
void setGate(XPtr<GatingSet> gs,string sampleName
               ,string gatePath,List filter) {

		GatingHierarchy & gh=gs->getGatingHierarchy(sampleName);

		NODEID u = gh.getNodeID(gatePath);

		gate * g=newGate(filter);

		nodeProperties & node=gh.getNodeProperty(u);
		gate * old_g = node.getGate();
		delete old_g;
		old_g=NULL;
		node.setGate(g);


}

//[[Rcpp::export(name=".cpp_removeNode")]]
void removeNode(XPtr<GatingSet> gs,string sampleName
                  ,string gatePath) {

		GatingHierarchy & gh=gs->getGatingHierarchy(sampleName);

		NODEID u = gh.getNodeID(gatePath);

		gh.removeNode(u);

}

//' move a node within the gating tree
//'
//' This is light-weight since it only update the edge in graph and requires user to
//' invoke recompute to update gating
//'
//' @param gsPtr external pointer that points to the C data structure of GatingSet
//' @param sampleName sample name
//' @param node node name
//' @noRd
//[[Rcpp::export(".moveNode")]]
void moveNode(Rcpp::XPtr<GatingSet> gsPtr, string sampleName, string node, string parent){

  GatingHierarchy & gh = gsPtr->getGatingHierarchy(sampleName);

  gh.moveNode(node, parent);


}

//[[Rcpp::export(name=".cpp_setNodeName")]]
void setNodeName(XPtr<GatingSet> gs,string sampleName
                   ,string gatePath, string newNodeName) {

		GatingHierarchy & gh=gs->getGatingHierarchy(sampleName);

		NODEID u = gh.getNodeID(gatePath);

		nodeProperties &node=gh.getNodeProperty(u);
		node.setName(newNodeName.c_str());

}

//[[Rcpp::export(name=".cpp_setNodeFlag")]]
void setNodeFlag(XPtr<GatingSet> gs,string sampleName
                   ,string gatePath, bool hidden) {

		GatingHierarchy & gh=gs->getGatingHierarchy(sampleName);

		NODEID u = gh.getNodeID(gatePath);

		nodeProperties &node=gh.getNodeProperty(u);
		node.setHiddenFlag(hidden);

}

