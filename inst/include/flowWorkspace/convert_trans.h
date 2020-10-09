#ifndef CONVERTTRANS_H_
#define CONVERTTRANS_H_


#include <cytolib/GatingSet.hpp>
using namespace cytolib;

#include <RcppArmadillo.h> //include this instead of Rcpp.h so that RcppArmadillo inclusion won't be preceded by Rcpp.h in RcppExport.cpp
#include <RcppCommon.h>
using namespace Rcpp;

inline TransPtr convert_transformer(List trans, string chnl){
  TransPtr thisTrans;
  string type = as<string>(trans["type"]);
  if(type == "flowJo_log")
  {
    
    thisTrans.reset(new logTrans(as<EVENT_DATA_TYPE>(trans["offset"]), as<EVENT_DATA_TYPE>(trans["decade"]), as<unsigned>(trans["scale"]), 262144));
  }
  else if(type == "logtGml2")
  {
    thisTrans.reset(new logGML2Trans(as<EVENT_DATA_TYPE>(trans["t"]), as<EVENT_DATA_TYPE>(trans["m"])));
  }
  else if(type == "logicle")
  {
    try
    {
      thisTrans.reset(new logicleTrans(as<double>(trans["t"]), as<double>(trans["w"]), as<double>(trans["m"]), as<double>(trans["a"]), false));
    }catch(const domain_error &e)
    {
      throw(domain_error("logicle transformation constructor error: " + chnl +"\n" + string(e.what())));
    }
  }
  else if(type == "flowJo_biexp")
    thisTrans.reset(new biexpTrans(as<int>(trans["channelRange"]), as<EVENT_DATA_TYPE>(trans["pos"]), as<EVENT_DATA_TYPE>(trans["neg"]), as<EVENT_DATA_TYPE>(trans["widthBasis"]), as<EVENT_DATA_TYPE>(trans["maxValue"])));
  else if(type == "asinhtGml2" || type == "flowJo_fasinh")
    thisTrans.reset(new fasinhTrans(as<EVENT_DATA_TYPE>(trans["t"]), as<EVENT_DATA_TYPE>(trans["length"]), as<EVENT_DATA_TYPE>(trans["t"]), as<EVENT_DATA_TYPE>(trans["a"]), as<EVENT_DATA_TYPE>(trans["m"])));
  else if(type == "logicleGml2")
  {
    try
    {
      thisTrans.reset(new logicleTrans(as<double>(trans["T"]), as<double>(trans["W"]), as<double>(trans["M"]), as<double>(trans["A"]), true));
    }catch(const domain_error &e)
    {
      throw(domain_error("logicle transformation constructor error: " + chnl +"\n" + string(e.what())));
    }
  }
  else if(type == "scale")
  {
    try
    {
      if((as<int>(trans["t_scale"]) == 0) || (as<int>(trans["r_scale"]) == 0))
        thisTrans.reset(new scaleTrans(as<EVENT_DATA_TYPE>(trans["scale_factor"])));
      else
        thisTrans.reset(new scaleTrans(as<int>(trans["t_scale"]), as<int>(trans["r_scale"])));
    }catch(const domain_error &e)
    {
      throw(domain_error("scale transformation constructor error: " + chnl +"\n" + string(e.what())));
    }
  }
  else
    stop("unknown transformation in set_transformations!");
  
  return thisTrans;
}

inline trans_map convert_transformer_list(List translist){
  
  trans_map trans;
  List res;
  if(Rf_isNull(translist.names()))
    stop("empty names for translist!");
  vector<string> chnls = as<vector<string>>(translist.names());
  for (string chnl : chnls)
  {
    trans[chnl] = convert_transformer(translist[chnl], chnl);
  }
  return trans;
}

#endif