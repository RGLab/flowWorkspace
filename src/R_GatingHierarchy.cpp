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
#include <cpp11.hpp>

#include "cytolib/GatingSet.hpp"
#include <stdexcept>
#include "cytolib/gate.hpp"
#include "cytolib/transformation.hpp"
using namespace std;

#include "flowWorkspace/convert_trans.h"
using namespace cytolib;


/*
 * only expose gating set pointer to R to avoid gc() by R
 */
[[cpp11::register]]
void cpp_plotGh(cpp11::external_pointer<GatingSet> gs,string sampleName,string output) {

	GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);
  gh.drawGraph(output);

}


/*
 * return node names as a character vector
 */
[[cpp11::register]]
StringVec cpp_getNodes(cpp11::external_pointer<GatingSet> gs,string sampleName
                  ,int order
                  ,bool fullPath
                  , bool showHidden){

	GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);

	return gh.getNodePaths(order,fullPath,showHidden);

}

[[cpp11::register]]
string getNodePath(cpp11::external_pointer<GatingSet> gs,string sampleName,int id){
	GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);
	return gh.getNodePath(unsigned(id));
}
/*
 * query by path
 */

[[cpp11::register]]
int cpp_getNodeID(cpp11::external_pointer<GatingSet> gs,string sampleName,string gatePath){

	GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);

	return gh.getNodeID(gatePath);

}

[[cpp11::register]]
int cpp_getParent(cpp11::external_pointer<GatingSet> gs,string sampleName,string gatePath){

	GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);
	NODEID u = gh.getNodeID(gatePath);
	return gh.getParent(u);

}

[[cpp11::register]]
vector<int> cpp_getChildren(cpp11::external_pointer<GatingSet> gs,string sampleName
                             ,string gatePath, bool showHidden){

	GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);

	NODEID u = gh.getNodeID(gatePath);
	auto childrenID = gh.getChildren(u);
	vector<int> res;
	for(VertexID_vec::iterator it=childrenID.begin(); it!=childrenID.end();it++){
		auto thisNodeID = *it;
		bool isHidden = gh.getNodeProperty(thisNodeID).getHiddenFlag();
		if(showHidden||(!isHidden))
			res.push_back(int(thisNodeID));
	}

	return res;

}

[[cpp11::register]]
cpp11::writable::list cpp_getPopStats(cpp11::external_pointer<GatingSet> gs,string sampleName
                     ,string gatePath){

	GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);
	NODEID u = gh.getNodeID(gatePath);
	nodeProperties &node=gh.getNodeProperty(u);
    cpp11::writable::list res;
    for (bool i : {true, false})
    {
        const char * statsname = i ? "FlowCore" : "FlowJo";
        auto stats = node.getStats(i);
        cpp11::writable::doubles vals;
        cpp11::writable::strings types;
        for (auto it : stats)
        {
			types.push_back(it.first);
			vals.push_back(it.second);
        }
		
		if(vals.size()>0)
		{
			vals.names() = types;
			res.push_back(cpp11::named_arg(statsname) = vals);
		}
	}
    return res;
}

[[cpp11::register]]
cpp11::list cpp_getCompensation(cpp11::external_pointer<GatingSet> gs,string sampleName){
  GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);
  compensation comp=gh.get_compensation();
	return(cpp11::list({cpp11::named_arg("cid")=comp.cid
						,cpp11::named_arg("prefix")=comp.prefix
						,cpp11::named_arg("suffix")=comp.suffix
						,cpp11::named_arg("comment")=comp.comment
						,cpp11::named_arg("parameters")=comp.marker
            ,cpp11::named_arg("detectors")=comp.detector
						,cpp11::named_arg("spillOver")=comp.spillOver})
			);


}

[[cpp11::register]]
void set_transformations(cpp11::external_pointer<GatingSet> gs,string sampleName, cpp11::list translist){


	GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);
	trans_map trans = convert_transformer_list(translist);
	gh.addTransMap(trans);
}

[[cpp11::register]]
cpp11::writable::list cpp_getTransformations(cpp11::external_pointer<GatingSet> gs,string sampleName, bool inverse){


	GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);
	trans_map trans=gh.getLocalTrans().getTransMap();
	cpp11::writable::list res;
	
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
					res.push_back(cpp11::named_arg(chnl.c_str())=cpp11::list({cpp11::named_arg("type") = "log"
												,cpp11::named_arg("decade") = thisTrans->decade
												,cpp11::named_arg("offset") = thisTrans->offset
												,cpp11::named_arg("T") = thisTrans->T
												,cpp11::named_arg("scale") = thisTrans->scale
                    })
									);
					break;
				}
				case LIN:
				{

					res.push_back(cpp11::named_arg(chnl.c_str())=cpp11::writable::list({cpp11::named_arg("type") = "lin"})
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
                    cpp11::writable::list coef;
                    for (auto it : obj.coefs)
                    {
                        coef.push_back(cpp11::named_arg(it.first.c_str()) = it.second);
                    }
                    res.push_back(cpp11::named_arg(chnl.c_str())=cpp11::list({cpp11::named_arg("z") = coef
												,cpp11::named_arg("method") = obj.method
												,cpp11::named_arg("type") =  "caltbl"
                    })
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
                    cpp11::writable::list coef;
                    for (auto it : obj.coefs)
                    {
                        coef.push_back(cpp11::named_arg(it.first.c_str()) = it.second);
                    }
					/*
					 * in addition, output the 5 arguments
					 */
					res.push_back(cpp11::named_arg(chnl.c_str())=cpp11::list({cpp11::named_arg("z") = coef
												,cpp11::named_arg("method") = obj.method
												,cpp11::named_arg("type") = "biexp"
												, cpp11::named_arg("channelRange") =  thisTrans->channelRange
												, cpp11::named_arg("maxValue") =  thisTrans->maxValue
												, cpp11::named_arg("neg") =  thisTrans->neg
												, cpp11::named_arg("pos") =  thisTrans->pos
												, cpp11::named_arg("widthBasis") =  thisTrans->widthBasis
                })
									);

					break;
				}
				case FASINH:
				{
					shared_ptr<fasinhTrans> thisTrans = dynamic_pointer_cast<fasinhTrans>(curTrans);

					res.push_back(cpp11::named_arg(chnl.c_str())=cpp11::list({cpp11::named_arg("type") = "fasinh"
												, cpp11::named_arg("A") =  thisTrans->A
												, cpp11::named_arg("M") =  thisTrans->M
												, cpp11::named_arg("T") =  thisTrans->T
												, cpp11::named_arg("length") =  thisTrans->length
												, cpp11::named_arg("maxRange") =  thisTrans->maxRange
                })
								);

					break;
				}
				case LOGICLE:
				{
					shared_ptr<logicleTrans> thisTrans = dynamic_pointer_cast<logicleTrans>(curTrans);
					logicle_params p = thisTrans->get_params();
					res.push_back(cpp11::named_arg(chnl.c_str())=cpp11::list({cpp11::named_arg("type") = "logicle"
												, cpp11::named_arg("A") =  p.A
												, cpp11::named_arg("M") =  p.M
												, cpp11::named_arg("T") =  p.T
												, cpp11::named_arg("W") =  p.W
                    })
								);

					break;
				}
  			case LOGGML2:
  			{
  			  shared_ptr<logGML2Trans> thisTrans = dynamic_pointer_cast<logGML2Trans>(curTrans);
  			  res.push_back(cpp11::named_arg(chnl.c_str())=cpp11::list({cpp11::named_arg("type") = "logtGml2"
                                    ,cpp11::named_arg("T") = thisTrans->T
                                    ,cpp11::named_arg("M") = thisTrans->M
                })
  			  );
  			  break;
  			}
			case SCALE:
			{
				shared_ptr<scaleTrans> thisTrans = dynamic_pointer_cast<scaleTrans>(curTrans);
				res.push_back(cpp11::named_arg(chnl.c_str())=cpp11::list({
								cpp11::named_arg("type") =  "scale",
								cpp11::named_arg("trans_scale") =  thisTrans->t_scale,
								cpp11::named_arg("raw_scale") =  thisTrans->r_scale,
								cpp11::named_arg("scale_factor") =  thisTrans->scale_factor
                                    })
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

[[cpp11::register]]
cpp11::list cpp_getGate(cpp11::external_pointer<GatingSet> gs,string sampleName,string gatePath){

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
					cpp11::writable::doubles_matrix<> covMat(2,2);
					for(unsigned i =0; i < 2; i++){
						covMat(i,0) = cov.at(i).x;
						covMat(i,1) = cov.at(i).y;
					}

					 cpp11::list ret=cpp11::list({cpp11::named_arg("parameters") = thisG.getParamNames()
							 	 	 	 	 ,cpp11::named_arg("mu") =  {mu.x,mu.y}
							 	 	 	 	 ,cpp11::named_arg("cov") =  covMat
							 	 	 	 	 ,cpp11::named_arg("dist") =  dist
							 	 	 	 	 ,cpp11::named_arg("type") = ELLIPSEGATE
							 	 	 	 	 , cpp11::named_arg("filterId") =  nodeName
                     });
					return ret;
				}
		case POLYGONGATE:
			{
				vertices_vector vert=g->getVertices().toVector();
				 auto pn = g->getParamNames();
				 cpp11::writable::list ret({cpp11::named_arg("parameters") =  pn
						 	 	 	 	 ,cpp11::named_arg("x") = vert.x
                                         ,cpp11::named_arg("y") = vert.y
						 	 	 	 	 ,cpp11::named_arg("type") = POLYGONGATE
						 	 	 	 	 , cpp11::named_arg("filterId") =  nodeName
						 	 	 	 	 });
				 if(quadpops.size() > 0)
				 {

					 cpp11::writable::doubles inter({quadintersection.x, quadintersection.y});
					 inter.attr("names") = pn;
					 ret.push_back(cpp11::named_arg("quadintersection") = inter);
					 ret.push_back(cpp11::named_arg("quadrants") = quadrants);
					 ret.push_back(cpp11::named_arg("quadpops") = quadpops);

				 }
				return ret;
			}

		case RANGEGATE:
			{
				vertices_vector vert=g->getVertices().toVector();

				cpp11::list ret=cpp11::list({cpp11::named_arg("parameters") = g->getParamNames()
									 ,cpp11::named_arg("range") = vert.x
									 ,cpp11::named_arg("type") = RANGEGATE
									 , cpp11::named_arg("filterId") =  nodeName
									 });
				return ret;
			}
		case BOOLGATE:
			{
			  boolGate & bg=dynamic_cast<boolGate&>(*g);
			  vector<BOOL_GATE_OP> boolOpSpec=bg.getBoolSpec();
			  vector<string> v;
			  vector<string>v2;
			  cpp11::writable::list ref;
			  for(vector<BOOL_GATE_OP>::iterator it=boolOpSpec.begin();it!=boolOpSpec.end();it++)
			  {
				  v.push_back(it->isNot?"!":"");
				  v2.push_back(string(1, it->op));
                  vector<string> spath(it->path.begin(), it->path.end());
                  ref.push_back(cpp11::strings(spath));
              }
                
			  cpp11::list ret=cpp11::list({cpp11::named_arg("v") = v
									 ,cpp11::named_arg("v2") = v2
									 ,cpp11::named_arg("ref") = ref
									 ,cpp11::named_arg("type") = BOOLGATE
									 , cpp11::named_arg("filterId") =  nodeName
									 });
			  return ret;

			}
	case MULTIRANGEGATE: {
	  auto& mg = dynamic_cast<cytolib::MultiRangeGate&>(*g);
	  auto ranges_pairs = mg.getRanges();
	  cpp11::writable::doubles min;
	  cpp11::writable::doubles max;
	  for (auto range : ranges_pairs) {
	    min.push_back(range.first);
	    max.push_back(range.second);
	  }
	  cpp11::writable::list ranges_list = cpp11::writable::list(
	  {cpp11::named_arg("min") = min, cpp11::named_arg("max") = max});
	  cpp11::writable::strings names;
	  for (auto name : g->getParamNames()) {
	    names.push_back(name);
	  }
	  cpp11::writable::list ret =
	    cpp11::list({cpp11::named_arg("parameters") = g->getParamNames(),
                 cpp11::named_arg("ranges") = ranges_list,
                 cpp11::named_arg("type") = static_cast<unsigned>(
                   MULTIRANGEGATE),
                   cpp11::named_arg("filterId") = nodeName,
                   cpp11::named_arg("negated") = g->isNegate()});
	  return ret;
	}
	  
		case LOGICALGATE:
		{
			boolGate & bg=dynamic_cast<boolGate&>(*g);
		  vector<BOOL_GATE_OP> boolOpSpec=bg.getBoolSpec();
		  vector<string> v;
		  vector<string>v2;
            cpp11::writable::list ref;
            for(vector<BOOL_GATE_OP>::iterator it=boolOpSpec.begin();it!=boolOpSpec.end();it++)
            {
                v.push_back(it->isNot?"!":"");
                v2.push_back(string(1, it->op));
                vector<string> spath(it->path.begin(), it->path.end());
                ref.push_back(cpp11::strings(spath));
            }
			cpp11::writable::list ret({cpp11::named_arg("v") = v, cpp11::named_arg("v2") = v2, cpp11::named_arg("type") = LOGICALGATE, cpp11::named_arg("filterId") = nodeName});
			if(ref.size()==0)
				ret.push_back(cpp11::named_arg("ref") = R_NilValue);
			else
				ret.push_back(cpp11::named_arg("ref") = ref);
			return ret;

		}
		case CLUSTERGATE:
		{
		  clusterGate & cg=dynamic_cast<clusterGate&>(*g);
		  vector<BOOL_GATE_OP> boolOpSpec=cg.getBoolSpec();
		  vector<string> v;
		    vector<string>v2;
            cpp11::writable::list ref;
            for(vector<BOOL_GATE_OP>::iterator it=boolOpSpec.begin();it!=boolOpSpec.end();it++)
            {
                v.push_back(it->isNot?"!":"");
                v2.push_back(string(1, it->op));
                vector<string> spath(it->path.begin(), it->path.end());
                ref.push_back(cpp11::strings(spath));
            }
		

			cpp11::writable::list ret({cpp11::named_arg("v") = v
							, cpp11::named_arg("v2") = v2
							, cpp11::named_arg("type") = CLUSTERGATE
							, cpp11::named_arg("filterId") = nodeName
							, cpp11::named_arg("cluster_method_name") =  cg.get_cluster_method_name()
		});
			if(ref.size()==0)
				ret.push_back(cpp11::named_arg("ref") = R_NilValue);
			else
				ret.push_back(cpp11::named_arg("ref") = ref);
				  return ret;

		}
		default:
		{
//			COUT<<g->getType()<<endl;
			throw(domain_error("unknown gate thrown by R_getGate!" + std::to_string(gType)));
		}

	}


}


[[cpp11::register]]
vector<bool> cpp_getIndices(cpp11::external_pointer<GatingSet> gs,string sampleName,string gatePath){

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

[[cpp11::register]]
void cpp_setIndices(cpp11::external_pointer<GatingSet> gs,string sampleName,int u, cpp11::logicals ind){


	if(u<0)throw(domain_error("not valid vertexID!"));

	GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);

	nodeProperties & node = gh.getNodeProperty(u);
	node.setIndices(vector<bool>(ind.begin(), ind.end()));
	node.computeStats();

}


[[cpp11::register]]
bool cpp_getGateFlag(cpp11::external_pointer<GatingSet> gs,string sampleName,string gatePath){

	GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);
	NODEID u = gh.getNodeID(gatePath);
	return gh.getNodeProperty(u).isGated();


}


[[cpp11::register]]
bool cpp_getNegateFlag(cpp11::external_pointer<GatingSet> gs,string sampleName,string gatePath){

	GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);
	NODEID u = gh.getNodeID(gatePath);
	return gh.getNodeProperty(u).getGate()->isNegate();

}

[[cpp11::register]]
bool cpp_getHiddenFlag(cpp11::external_pointer<GatingSet> gs,string sampleName,string gatePath){

	GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);
	NODEID u = gh.getNodeID(gatePath);
	return gh.getNodeProperty(u).getHiddenFlag();

}

vector<BOOL_GATE_OP> boolFilter_R_to_C(cpp11::list filter){


			/*
			 * get specification from R
			 */
			cpp11::strings refs(filter["refs"]);
			cpp11::strings op(filter["op"]);
			cpp11::logicals isNot(filter["isNot"]);

			/*
			 * convert to c class
			 */
			vector<BOOL_GATE_OP> res;
			for(int i=0;i<refs.size();i++)
			{

				BOOL_GATE_OP gOpObj;
                string thispath = refs.at(i);
                boost::split(gOpObj.path, thispath,boost::is_any_of("/"));
				if(gOpObj.path.at(0).empty())
					gOpObj.path.erase(gOpObj.path.begin());//remove the first empty string

				gOpObj.isNot=isNot.at(i)==TRUE;
				gOpObj.op=boost::iequals(string(op.at(i)),"|")?'|':'&';

				res.push_back(gOpObj);
			}
			return (res);
}
/*
 * convert R filter to specific gate class
 * Note: up to caller to free the dynamically allocated gate object
 */
gatePtr  newGate(cpp11::list filter){

	cpp11::strings names=filter.names();

	unsigned short gateType=cpp11::integers(filter["type"])[0];

	bool isNeg=cpp11::logicals(filter["negated"])[0];
	gatePtr  g;

	switch(gateType)
	{
		case RANGEGATE:
		{
			cpp11::strings params(filter["params"]);
			unique_ptr<rangeGate> rg(new rangeGate());
			rg->setNegate(isNeg);

			cpp11::doubles p(filter["range"]);

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
			auto params = cpp11::as_cpp<vector<string>>(filter["params"]);
			unique_ptr<polygonGate> pg(new polygonGate());

			pg->setNegate(isNeg);

			paramPoly pp;
			pp.setName(params);

			vector<coordinate> v;
			cpp11::doubles_matrix<> boundaries(filter["boundaries"]);
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
			auto params = cpp11::as_cpp<vector<string>>(filter["params"]);
			unique_ptr<rectGate> rectg(new rectGate());

			rectg->setNegate(isNeg);

			paramPoly pp;
			pp.setName(params);

			vector<coordinate> v;
			cpp11::doubles_matrix<> boundaries(filter["boundaries"]);
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
			unique_ptr<clusterGate> cg(new clusterGate(cpp11::as_cpp<string>(filter["cluster_method_name"])));
			cg->setNegate(isNeg);
			g.reset(cg.release());
			break;
		}
		case ELLIPSEGATE:
		{



			//parse the mean
			cpp11::doubles mean(filter["mu"]);
			coordinate mu(mean.at(0), mean.at(1));
			double dist = cpp11::as_cpp<double>(filter["dist"]);

			//parse cov mat
			vector<coordinate> cov;
			cpp11::doubles_matrix<> covMat(filter["cov"]);
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
			auto params = cpp11::as_cpp<vector<string>>(filter["params"]);
			paramPoly pp;
			pp.setName(params);
			eg->setParam(pp);

			g.reset(eg.release());

			break;
		}
		case QUADGATE:
		{
			auto params = cpp11::as_cpp<vector<string>>(filter["params"]);
			auto mu = cpp11::doubles(filter["mu"]);

			paramPoly intersect;
			intersect.setName(params);
			intersect.setVertices({coordinate(mu[0], mu[1])});

			string uid = cpp11::as_cpp<string>(filter["uid"]);
			QUAD quadrant = static_cast<QUAD>(cpp11::as_cpp<int>(filter["quad"]));
			unique_ptr<quadGate> qg(new quadGate(intersect, uid, quadrant));

			g.reset(qg.release());

			break;

		}
	case MULTIRANGEGATE: {
	  std::unique_ptr<cytolib::MultiRangeGate> mrg(new cytolib::MultiRangeGate);
	  auto param = cpp11::as_cpp<std::string>(filter["params"]);
	  assert(param == "Time");
	  mrg->setNegate(isNeg);
	  std::vector<double> ranges_input =
	    cpp11::as_cpp<std::vector<double>>(filter["ranges"]);
	  assert(ranges_in.size() % 2 == 0);
	  std::vector<std::pair<float, float>> ranges;
	  for (int i = 0; i < ranges_input.size(); i += 2) {
	    ranges.push_back(
	      std::make_pair(static_cast<float>(ranges_input.at(i)),
                      static_cast<float>(ranges_input.at(i + 1))));
	  }
	  mrg->setRanges(ranges);
	  g = std::move(mrg);
	  break;
	}
	  
		default:
			throw(domain_error("unsupported gate type!valid types: POLYGONGATE(1),RANGEGATE(2),BOOLGATE(3),RECTGATE(5),LOGICALGATE(6)"));

	}
	g->setTransformed(TRUE);
	return g;

}

[[cpp11::register]]
NODEID cpp_addGate(cpp11::external_pointer<GatingSet> gs,string sampleName
                   ,cpp11::list filter
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
[[cpp11::register]]
void cpp_boolGating(cpp11::external_pointer<GatingSet> gs,string sampleName,cpp11::list filter,unsigned nodeID) {

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

[[cpp11::register]]
void set_quadgate(cpp11::external_pointer<GatingSet> gs,string sampleName,string gatePath, vector<double> inter) {

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

[[cpp11::register]]
void cpp_setGate(cpp11::external_pointer<GatingSet> gs,string sampleName
               ,string gatePath,cpp11::list filter) {

		GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);

		NODEID u = gh.getNodeID(gatePath);

		gatePtr  g=newGate(filter);

		nodeProperties & node=gh.getNodeProperty(u);
		node.setGate(g);


}

[[cpp11::register]]
void cpp_removeNode(cpp11::external_pointer<GatingSet> gs,string sampleName
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
[[cpp11::register]]
void moveNode(cpp11::external_pointer<GatingSet> gsPtr, string sampleName, string node, string parent){

  GatingHierarchy & gh = *gsPtr->getGatingHierarchy(sampleName);

  gh.moveNode(node, parent);


}

[[cpp11::register]]
void setNodeName(cpp11::external_pointer<GatingSet> gs,string sampleName
                   ,string gatePath, string newNodeName) {

		GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);

		NODEID u = gh.getNodeID(gatePath);

		nodeProperties &node=gh.getNodeProperty(u);
		node.setName(newNodeName.c_str());

}

[[cpp11::register]]
void setNodeFlag(cpp11::external_pointer<GatingSet> gs,string sampleName
                   ,string gatePath, bool hidden) {

		GatingHierarchy & gh=*gs->getGatingHierarchy(sampleName);

		NODEID u = gh.getNodeID(gatePath);

		nodeProperties &node=gh.getNodeProperty(u);
		node.setHiddenFlag(hidden);

}

