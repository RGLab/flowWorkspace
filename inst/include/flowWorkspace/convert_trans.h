#ifndef CONVERTTRANS_H_
#define CONVERTTRANS_H_


#include <cytolib/GatingSet.hpp>
using namespace cytolib;

inline TransPtr convert_transformer(cpp11::list trans, string chnl){
  TransPtr thisTrans;
  auto type = cpp11::as_cpp<cpp11::r_string>(trans["type"]);
  if(type == "flowJo_log")
  {
    
    thisTrans.reset(new logTrans(cpp11::as_cpp<EVENT_DATA_TYPE>(trans["offset"]), cpp11::as_cpp<EVENT_DATA_TYPE>(trans["decade"]), cpp11::as_cpp<unsigned>(trans["scale"]), 262144));
  }
  else if(type == "logtGml2")
  {
    thisTrans.reset(new logGML2Trans(cpp11::as_cpp<EVENT_DATA_TYPE>(trans["t"]), cpp11::as_cpp<EVENT_DATA_TYPE>(trans["m"])));
  }
  else if(type == "logicle")
  {
    try
    {
      thisTrans.reset(new logicleTrans(cpp11::as_cpp<double>(trans["t"]), cpp11::as_cpp<double>(trans["w"])
      , cpp11::as_cpp<double>(trans["m"]), cpp11::as_cpp<double>(trans["a"]), false));
    }catch(const domain_error &e)
    {
      throw(domain_error("logicle transformation constructor error: " + chnl +"\n" + string(e.what())));
    }
  }
  else if(type == "flowJo_biexp")
    thisTrans.reset(new biexpTrans(cpp11::as_cpp<int>(trans["channelRange"])
    , cpp11::as_cpp<EVENT_DATA_TYPE>(trans["pos"]), cpp11::as_cpp<EVENT_DATA_TYPE>(trans["neg"]), cpp11::as_cpp<EVENT_DATA_TYPE>(trans["widthBasis"]), cpp11::as_cpp<EVENT_DATA_TYPE>(trans["maxValue"])));
  else if(type == "asinhtGml2" || type == "flowJo_fasinh")
    thisTrans.reset(new fasinhTrans(cpp11::as_cpp<EVENT_DATA_TYPE>(trans["t"]), cpp11::as_cpp<EVENT_DATA_TYPE>(trans["length"]), cpp11::as_cpp<EVENT_DATA_TYPE>(trans["t"]), cpp11::as_cpp<EVENT_DATA_TYPE>(trans["a"]), cpp11::as_cpp<EVENT_DATA_TYPE>(trans["m"])));
  else if(type == "logicleGml2")
  {
    try
    {
      thisTrans.reset(new logicleTrans(cpp11::as_cpp<double>(trans["T"]), cpp11::as_cpp<double>(trans["W"]), cpp11::as_cpp<double>(trans["M"]), cpp11::as_cpp<double>(trans["A"]), true));
    }catch(const domain_error &e)
    {
      throw(domain_error("logicle transformation constructor error: " + chnl +"\n" + string(e.what())));
    }
  }
  else if(type == "scale")
  {
    try
    {
      if((cpp11::as_cpp<int>(trans["t_scale"]) == 0) || (cpp11::as_cpp<int>(trans["r_scale"]) == 0))
        thisTrans.reset(new scaleTrans(cpp11::as_cpp<EVENT_DATA_TYPE>(trans["scale_factor"])));
      else
        thisTrans.reset(new scaleTrans(cpp11::as_cpp<int>(trans["t_scale"]), cpp11::as_cpp<int>(trans["r_scale"])));
    }catch(const domain_error &e)
    {
      throw(domain_error("scale transformation constructor error: " + chnl +"\n" + string(e.what())));
    }
  }
  else
    cpp11::stop("unknown transformation in set_transformations!");
  
  return thisTrans;
}

inline trans_map convert_transformer_list(cpp11::list translist){
  
  trans_map trans;
  
  if(Rf_isNull(translist.names()))
    cpp11::stop("empty names for translist!");
  auto chnls = cpp11::as_cpp<cpp11::strings>(translist.names());
  for (string chnl : chnls)
  {
    trans[chnl] = convert_transformer(cpp11::as_cpp<cpp11::list>(translist[chnl]), chnl);
  }
  return trans;
}

#endif