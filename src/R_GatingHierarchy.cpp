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

#include "cytolib/GatingSet.hpp"
#include <stdexcept>
#include "cytolib/gate.hpp"
#include "cytolib/transformation.hpp"
using namespace std;

#include "flowWorkspace/convert_trans.h"
#include <Rcpp.h>
using namespace Rcpp;
using namespace cytolib;


/*
 * only expose gating set pointer to R to avoid gc() by R
 */
//[[Rcpp::export(name=".cpp_plotGh")]]
void plotGh(XPtr<GatingSet> gs,string sampleName,string output) {

	GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);
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

	GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);

	return gh.getNodePaths(order,fullPath,showHidden);

}

//[[Rcpp::export(name=".cpp_getPhylo")]]
List getPhylo(XPtr<GatingSet> gs,string sampleName,string gatePath){
  GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);
  VertexID u = gh.getNodeID(gatePath);
  phylo gh_phylo = gh.getPhylo(u);
  NumericMatrix edges(gh_phylo.edges.size(), 2);
  for(int i = 0; i < gh_phylo.edges.size(); i++)
    edges(i, Rcpp::_) = NumericVector::create(gh_phylo.edges[i].first, gh_phylo.edges[i].second);
  
  IntegerVector leaf_nodes(gh_phylo.leaf_nodes.size());
  for(int i = 0; i < gh_phylo.leaf_nodes.size(); i++)
    leaf_nodes[i] = gh_phylo.leaf_nodes[i];
  StringVector leaf_names(gh_phylo.leaf_names.size());
  for(int i = 0; i < gh_phylo.leaf_names.size(); i++)
    leaf_names[i] = gh_phylo.leaf_names[i];
  
  return(List::create(Named("edges", edges), 
                      Named("leaf_nodes", leaf_nodes),
                      Named("leaf_names", leaf_names)));
}

//[[Rcpp::export]]
string getNodePath(XPtr<GatingSet> gs,string sampleName,NODEID id){
	GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);
	return gh.getNodePath(id);
}
/*
 * query by path
 */
//[[Rcpp::export(name=".cpp_getNodeID")]]
NODEID getNodeID(XPtr<GatingSet> gs,string sampleName,string gatePath){

	GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);

	return (NODEID)gh.getNodeID(gatePath);

}
//[[Rcpp::export(name=".cpp_getParent")]]
NODEID getParent(XPtr<GatingSet> gs,string sampleName,string gatePath){

	GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);
	NODEID u = gh.getNodeID(gatePath);
	return (NODEID)gh.getParent(u);

}

//[[Rcpp::export(name=".cpp_getChildren")]]
vector<NODEID> getChildren(XPtr<GatingSet> gs,string sampleName
                             ,string gatePath, bool showHidden){

	GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);

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

	GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);
	NODEID u = gh.getNodeID(gatePath);
	nodeProperties &node=gh.getNodeProperty(u);

	return List::create(Named("FlowCore",node.getStats(true))
						,Named("FlowJo",node.getStats(false))
						);

}



//[[Rcpp::export(name=".cpp_getCompensation")]]
List getCompensation(XPtr<GatingSet> gs,string sampleName){
  GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);
  compensation comp=gh.get_compensation();
	return(List::create(Named("cid",comp.cid)
						,Named("prefix",comp.prefix)
						,Named("suffix",comp.suffix)
						,Named("comment",comp.comment)
						,Named("parameters",comp.marker)
						,Named("spillOver",comp.spillOver))
			);


}

//[[Rcpp::export]]
void set_transformations(XPtr<GatingSet> gs,string sampleName, List translist){


	GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);
	trans_map trans = convert_transformer_list(translist);
	gh.addTransMap(trans);
}

//[[Rcpp::export(name=".cpp_getTransformations")]]
List getTransformations(XPtr<GatingSet> gs,string sampleName, bool inverse){


	GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);
	trans_map trans=gh.getLocalTrans().getTransMap();
	List res;

	for (trans_map::iterator it=trans.begin();it!=trans.end();it++)
	{
		TransPtr curTrans=it->second;
		if(curTrans==NULL)
			throw(domain_error("empty transformation for channel"+it->first));
		if(!curTrans->gateOnly())
		{
			if(inverse){
				curTrans = curTrans->getInverseTransformation();
			}

			string chnl = it->first;
	//		string transName = curTrans->getName()+" "+chnl;

			switch(curTrans->getType())
			{

				case LOG:
				{
					shared_ptr<logTrans> thisTrans = dynamic_pointer_cast<logTrans>(curTrans);
					res.push_back(List::create(Named("type","log")
												,Named("decade",thisTrans->decade)
												,Named("offset",thisTrans->offset)
												,Named("T",thisTrans->T)
												,Named("scale",thisTrans->scale)
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
					shared_ptr<biexpTrans> thisTrans = dynamic_pointer_cast<biexpTrans>(curTrans);
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
					shared_ptr<fasinhTrans> thisTrans = dynamic_pointer_cast<fasinhTrans>(curTrans);

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
				case LOGICLE:
				{
					shared_ptr<logicleTrans> thisTrans = dynamic_pointer_cast<logicleTrans>(curTrans);
					logicle_params p = thisTrans->get_params();
					res.push_back(List::create(Named("type","logicle")
												, Named("A", p.A)
												, Named("M", p.M)
												, Named("T", p.T)
												, Named("W", p.W)
												)
												,chnl
								);

					break;
				}
  			case LOGGML2:
  			{
  			  shared_ptr<logGML2Trans> thisTrans = dynamic_pointer_cast<logGML2Trans>(curTrans);
  			  res.push_back(List::create(Named("type","logtGml2")
                                    ,Named("T",thisTrans->T)
                                    ,Named("M",thisTrans->M)
  			                )
                       ,chnl
  			  );
  			  break;
  			}
			case SCALE:
			{
				shared_ptr<scaleTrans> thisTrans = dynamic_pointer_cast<scaleTrans>(curTrans);
				res.push_back(List::create(
								Named("type", "scale"),
								Named("trans_scale", thisTrans->t_scale),
								Named("raw_scale", thisTrans->r_scale),
								Named("scale_factor", thisTrans->scale_factor)
							 ),
						chnl
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

vector<VertexID> retrieve_sibling_quadnodes(GatingHierarchy & gh,  VertexID quadnode)
{
	vector<VertexID> res;
	auto & node = gh.getNodeProperty(quadnode);
	auto g = node.getGate();
	auto gType=g->getType();
	if(gType == QUADGATE)
	{
		quadGate & qg=dynamic_cast<quadGate&>(*g);
		//recollect all the quadrants
		auto uid = qg.get_uid();
		auto pid = gh.getParent(quadnode);
		auto siblings = gh.getChildren(pid);
		for(auto id : siblings)//search all siblings
		{
			nodeProperties & nd = gh.getNodeProperty(id);
			gatePtr g1 = nd.getGate();

			if(g1->getType() == QUADGATE)//if a quad
			{
				quadGate & qg1 = dynamic_cast<quadGate&>(*g1);
				if(qg1.get_uid() == uid)//if belongs to the same quad
				{
					res.push_back(id);

				}
			}

		}
	}
	return res;
}
//[[Rcpp::export(name=".cpp_getGate")]]
List getGate(XPtr<GatingSet> gs,string sampleName,string gatePath){

	GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);
	NODEID u = gh.getNodeID(gatePath);
	if(u==0)
		throw(domain_error("no gate associated with root node."));
	nodeProperties & node = gh.getNodeProperty(u);
	gatePtr g = node.getGate();
	string nodeName = node.getName();
	unsigned short gType=g->getType();
	vector<string> quadpops;
	vector<unsigned> quadrants;
	coordinate quadintersection;
	if(gType == QUADGATE)
	{
		auto siblings = retrieve_sibling_quadnodes(gh, u);
		quadGate & qg=dynamic_cast<quadGate&>(*g);
		quadintersection = qg.get_intersection();
		for(auto id : siblings)//collect all quadrants info
		{
			nodeProperties & nd = gh.getNodeProperty(id);
			gatePtr g1 = nd.getGate();
			quadGate & qg1 = dynamic_cast<quadGate&>(*g1);
			quadpops.push_back(nd.getName());
			quadrants.push_back(qg1.get_quadrant());

		}
		g.reset(new rectGate(qg.to_rectgate()));
		gType=POLYGONGATE;
	}
	if(gType==RECTGATE||gType == CURLYQUADGATE)
		gType=POLYGONGATE;

	switch(gType)
	{
		case ELLIPSEGATE:
				{
					ellipseGate & thisG = dynamic_cast<ellipseGate&>(*g);
					coordinate mu=thisG.getMu();
					double dist=thisG.getDist();
					vector<coordinate> cov = thisG.getCovarianceMat();
					NumericMatrix covMat(2,2);
					for(unsigned i =0; i < 2; i++){
						covMat(i,0) = cov.at(i).x;
						covMat(i,1) = cov.at(i).y;
					}

					 List ret=List::create(Named("parameters",thisG.getParamNames())
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
				 auto pn = g->getParamNames();
				 List ret=List::create(Named("parameters", pn)
						 	 	 	 	 ,Named("x",vert.x),Named("y",vert.y)
						 	 	 	 	 ,Named("type",POLYGONGATE)
						 	 	 	 	 , Named("filterId", nodeName)
						 	 	 	 	 );
				 if(quadpops.size() > 0)
				 {

					 NumericVector inter = NumericVector::create(quadintersection.x, quadintersection.y);
					 inter.attr("names") = pn;
					 ret["quadintersection"] = inter;
					 ret["quadrants"] = quadrants;
					 ret["quadpops"] = quadpops;

				 }
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
			  boolGate & bg=dynamic_cast<boolGate&>(*g);
			  vector<BOOL_GATE_OP> boolOpSpec=bg.getBoolSpec();
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
			boolGate & bg=dynamic_cast<boolGate&>(*g);
		  vector<BOOL_GATE_OP> boolOpSpec=bg.getBoolSpec();
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
		  clusterGate & cg=dynamic_cast<clusterGate&>(*g);
		  vector<BOOL_GATE_OP> boolOpSpec=cg.getBoolSpec();
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
								 , Named("cluster_method_name", cg.get_cluster_method_name())
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

	GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);
	NODEID u = gh.getNodeID(gatePath);
	nodeProperties & node = gh.getNodeProperty(u);
	//gate for this particular node in case it is not gated(e.g. indices of bool gate is not archived, thus needs the lazy-gating)
	if(u>0&&!node.isGated())
	{
		if(node.getGate()->getType()!=BOOLGATE)
			throw(domain_error("Event indicies are not available for the ungated non-boolean node: '" + gatePath + "'. \n Please recompute it first!"));
		MemCytoFrame fr;
		gh.gating(fr, u);
	}
	return node.getIndices();


}

//[[Rcpp::export(name=".cpp_setIndices")]]
void setIndices(XPtr<GatingSet> gs,string sampleName,int u, BoolVec ind){


	if(u<0)throw(domain_error("not valid vertexID!"));

	GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);

	nodeProperties & node = gh.getNodeProperty(u);
	node.setIndices(ind);
	node.computeStats();

}


//[[Rcpp::export(name=".cpp_getGateFlag")]]
bool getGateFlag(XPtr<GatingSet> gs,string sampleName,string gatePath){

	GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);
	NODEID u = gh.getNodeID(gatePath);
	return gh.getNodeProperty(u).isGated();


}

//[[Rcpp::export(name=".cpp_getNegateFlag")]]
bool getNegateFlag(XPtr<GatingSet> gs,string sampleName,string gatePath){

	GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);
	NODEID u = gh.getNodeID(gatePath);
	return gh.getNodeProperty(u).getGate()->isNegate();

}
//[[Rcpp::export(name=".cpp_getHiddenFlag")]]
bool getHiddenFlag(XPtr<GatingSet> gs,string sampleName,string gatePath){

	GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);
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
gatePtr  newGate(List filter){

	StringVec names=filter.names();

	unsigned short gateType=as<unsigned short>(filter["type"]);

	bool isNeg=as<bool>(filter["negated"]);
	gatePtr  g;

	switch(gateType)
	{
		case RANGEGATE:
		{
			StringVec params=as<StringVec>(filter["params"]);
			unique_ptr<rangeGate> rg(new rangeGate());
			rg->setNegate(isNeg);

			DoubleVec p=as<DoubleVec>(filter["range"]);

			paramRange pRange;
			pRange.setName(params.at(0));
			pRange.setMin(p.at(0));
			pRange.setMax(p.at(1));

			rg->setParam(pRange);

			g.reset(rg.release());

			break;
		}
		case POLYGONGATE:
		{
			StringVec params=as<StringVec>(filter["params"]);
			unique_ptr<polygonGate> pg(new polygonGate());

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

			g.reset(pg.release());

			break;
		}
		case RECTGATE:
		{
			StringVec params=as<StringVec>(filter["params"]);
			unique_ptr<rectGate> rectg(new rectGate());

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

			g.reset(rectg.release());
			break;

		}
		case BOOLGATE:
		{
			unique_ptr<boolGate> bg(new boolGate());

			bg->setNegate(isNeg);
			bg->boolOpSpec = boolFilter_R_to_C(filter);
			g.reset(bg.release());
			break;

		}
		case LOGICALGATE:
		{
			unique_ptr<logicalGate> lg(new logicalGate());
			lg->setNegate(isNeg);
			g.reset(lg.release());
			break;
		}
		case CLUSTERGATE:
		{
			unique_ptr<clusterGate> cg(new clusterGate(as<string>(filter["cluster_method_name"])));
			cg->setNegate(isNeg);
			g.reset(cg.release());
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

			unique_ptr<ellipseGate> eg(new ellipseGate(mu, cov,dist));
			eg->setNegate(isNeg);

			// parse the parameter names
			StringVec params=as<StringVec>(filter["params"]);
			paramPoly pp;
			pp.setName(params);
			eg->setParam(pp);

			g.reset(eg.release());

			break;
		}
		case QUADGATE:
		{
			StringVec params=as<StringVec>(filter["params"]);
			auto mu = as<DoubleVec>(filter["mu"]);

			paramPoly intersect;
			intersect.setName(params);
			intersect.setVertices({coordinate(mu[0], mu[1])});

			string uid = as<string>(filter["uid"]);
			QUAD quadrant = static_cast<QUAD>(as<int>(filter["quad"]));
			unique_ptr<quadGate> qg(new quadGate(intersect, uid, quadrant));

			g.reset(qg.release());

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

		GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);

		NODEID u = gh.getNodeID(gatePath);
		gatePtr  g=newGate(filter);


		VertexID nodeID=gh.addGate(g,u,popName);


		return (NODEID)nodeID;

}
/**
 * mainly used for openCyto rectRef gate which first being added as a rectangle gate
 * and then gated as boolean filter
 */
//[[Rcpp::export(name=".cpp_boolGating")]]
void boolGating(XPtr<GatingSet> gs,string sampleName,List filter,unsigned nodeID) {

		GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);
		nodeProperties & node=gh.getNodeProperty(nodeID);
		//parse boolean expression from R data structure into c++
		vector<BOOL_GATE_OP> boolOp = boolFilter_R_to_C(filter);
		//perform bool gating
		MemCytoFrame fr;
		vector<bool> curIndices= gh.boolGating(fr, boolOp, true);//pass dummy frame since boolgating doesn't need it in openCyto where all the ref nodes are guaranteed to be gated

		//combine with parent indices
		nodeProperties & parentNode=gh.getNodeProperty(gh.getParent(nodeID));
		transform (curIndices.begin(), curIndices.end(), parentNode.getIndices().begin(), curIndices.begin(),logical_and<bool>());
		//save the indices
		node.setIndices(curIndices);
		node.computeStats();


}

//[[Rcpp::export]]
void set_quadgate(XPtr<GatingSet> gs,string sampleName,string gatePath, vector<double> inter) {

	if(inter.size()!=2)
		throw(domain_error("invalid intersection values!"));

	GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);

	NODEID u = gh.getNodeID(gatePath);
	auto siblings = retrieve_sibling_quadnodes(gh, u);
	for(auto id : siblings)
	{
		auto& nd = gh.getNodeProperty(id);
		auto g = nd.getGate();
		quadGate & qg=dynamic_cast<quadGate&>(*g);
		paramPoly param = qg.getParam();
		param.setVertices({coordinate(inter[0], inter[1])});
		qg.setParam(param);
	}

}

//[[Rcpp::export(name=".cpp_setGate")]]
void setGate(XPtr<GatingSet> gs,string sampleName
               ,string gatePath,List filter) {

		GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);

		NODEID u = gh.getNodeID(gatePath);

		gatePtr  g=newGate(filter);

		nodeProperties & node=gh.getNodeProperty(u);
		node.setGate(g);


}

//[[Rcpp::export(name=".cpp_removeNode")]]
void removeNode(XPtr<GatingSet> gs,string sampleName
                  ,string gatePath, bool recursive = false) {

		GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);

		if(recursive)
		{
			gh.removeNode(gatePath);
		}
		else
		{
			NODEID u = gh.getNodeID(gatePath);
			gh.removeNode(u);
		}


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

  GatingHierarchy & gh = *gsPtr->getGatingHierarchy(sampleName);

  gh.moveNode(node, parent);


}

//[[Rcpp::export(name=".cpp_setNodeName")]]
void setNodeName(XPtr<GatingSet> gs,string sampleName
                   ,string gatePath, string newNodeName) {

		GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);

		NODEID u = gh.getNodeID(gatePath);

		nodeProperties &node=gh.getNodeProperty(u);
		node.setName(newNodeName.c_str());

}

//[[Rcpp::export(name=".cpp_setNodeFlag")]]
void setNodeFlag(XPtr<GatingSet> gs,string sampleName
                   ,string gatePath, bool hidden) {

		GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);

		NODEID u = gh.getNodeID(gatePath);

		nodeProperties &node=gh.getNodeProperty(u);
		node.setHiddenFlag(hidden);

}

