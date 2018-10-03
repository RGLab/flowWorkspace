#include <flowWorkspace/pairVectorRcppWrap.h>
#include <flowWorkspace/convert_to_str_idx.h>
using namespace Rcpp;
using namespace cytolib;

//[[Rcpp::plugins("temp")]]

// [[Rcpp::export]] 
Rcpp::XPtr<CytoSet> fcs_to_cytoset(vector<pair<string,string>> sample_uid_vs_file_path, const FCS_READ_PARAM & config, bool is_h5, string h5_dir)
{

  Rcpp::XPtr<CytoSet> cs(new CytoSet(sample_uid_vs_file_path, config, is_h5, h5_dir));
  
  return cs;
  
}


// [[Rcpp::export]] 
vector<string> get_colnames(Rcpp::XPtr<CytoSet> cs){
  return cs->get_channels();
}

// [[Rcpp::export]]
Rcpp::XPtr<CytoSet> realize_view_cytoset(Rcpp::XPtr<CytoSet> cs, string path)
{
  return XPtr<CytoSet>(new CytoSet(cs->copy_realized(path)));
}

// [[Rcpp::export]]
Rcpp::XPtr<CytoSet> shallow_copy_cytoset(Rcpp::XPtr<CytoSet> cs)
{
  return XPtr<CytoSet>(new CytoSet(*cs));
}

// [[Rcpp::export]]
void subset_cytoset_by_rows(Rcpp::XPtr<CytoSet> cs
                                             , string sn
                                             , vector<unsigned> idx
                                            )
{
  
  cs->get_cytoframe_view_ref(sn).rows_(idx);
}
  
// [[Rcpp::export]]
void subset_cytoset(Rcpp::XPtr<CytoSet> cs
                                     , SEXP i_obj
                                     , SEXP j_obj
                                      )
{
  
  /*
  * parse i index (sample name)
  */
  unsigned short i_type = TYPEOF(i_obj);
  // Rcout << "i_type:" << i_type << endl;
  if(i_type != NILSXP)
  {
    StringVector sample_uids;
    if(i_type == STRSXP)
      sample_uids = as<StringVector>(i_obj);
    else
    {
      
      sample_uids = convert_to_str_idx(wrap(cs->get_sample_uids()), i_obj);
    }
    cs->sub_samples_(as<vector<string>>(sample_uids));
  }
  
  /*
   * parse j index (col)
   */  
  unsigned short j_type = TYPEOF(j_obj);
  // Rcout << "j_type:" << j_type << endl;
  vector<string> ch_selected;
  if(j_type != NILSXP)
  {
    // Rcout << "STRSXP:" << STRSXP << endl;
    
    if(j_type == STRSXP)
      ch_selected = as<vector<string>>(as<StringVector>(j_obj));
    else
      ch_selected = as<vector<string>>(convert_to_str_idx(wrap(cs->get_channels()), j_obj));
    cs->cols_(ch_selected, ColType::channel);
  }
  
}

// [[Rcpp::export]]
Rcpp::XPtr<CytoFrameView> get_cytoFrame(Rcpp::XPtr<CytoSet> cs
                , Rcpp::RObject i_obj
                , Rcpp::RObject j_obj
                )
{
  
  /*
   * parse i index (sample name)
   */
  std::string sample_uid;
  unsigned short i_type = i_obj.sexp_type();

  if(i_type  == STRSXP)
  {
    sample_uid = Rcpp::as<std::string>(i_obj.get__());
  }
  else if(i_type == REALSXP || i_type == INTSXP)
  {
    unsigned s_ind = Rcpp::as<unsigned>(i_obj.get__());
    sample_uid =  cs->get_sample_uids()[s_ind - 1];
  }
  else
    Rcpp::stop("unsupported i type!");

  
  XPtr<CytoFrameView> frame(new CytoFrameView(cs->get_cytoframe_view(sample_uid)));
  /*
  * parse j index (channel names)
  */
   /*
  * subset by j if applicable
  */
  int j_type = j_obj.sexp_type();

  if(j_type != NILSXP)
  {
    //get local channel names
    StringVector colnames = wrap(frame->get_channels());
    StringVector ch_selected;
    
    //creating j index used for subsetting colnames and pdata
    Rcpp::IntegerVector j_indx;

    if(j_type == STRSXP)//when character vector
    {
      ch_selected = StringVector(j_obj.get__());
  
    }
    else if(j_type == LGLSXP)
    {
      Rcpp::LogicalVector j_val(j_obj.get__());
      //convert to integer vector
      unsigned nSel = Rcpp::sum(j_val);
      unsigned thisCount = 0;
      j_indx = Rcpp::IntegerVector(nSel);
      for(unsigned i = 0; i < j_val.size(); i++){
        if(j_val(i)){
          j_indx(thisCount++) = i;
        }
  
      }
  
      ch_selected = colnames[j_indx];
    }
    else if(j_type == INTSXP || j_type == REALSXP)
    {
      Rcpp::IntegerVector j_val(j_obj.get__());
      j_indx = j_val - 1; //convert to 0-based index
      ch_selected = colnames[j_indx];
    }
    else
      Rcpp::stop("unsupported j expression!");
    
    frame->cols_(as<vector<string>>(ch_selected), ColType::channel);
  }
  
  return frame;
  
   
}

// [[Rcpp::export]] 
void set_pheno_data(Rcpp::XPtr<CytoSet> cs, DataFrame value)
{
  
  vector<string> sample_uids = as<vector<string>>(value.attr("row.names"));
  vector<string> colnames = as<vector<string>>(value.names());
  
  for(auto i = 0; i < value.rows(); i++)
  {
    CytoFrameView & fr = cs->get_cytoframe_view_ref(sample_uids[i]);
    PDATA pd;
    for(const string & key : colnames)
    {
      vector<string> col = value[key];
      pd[key] = col[i];
    }
    fr.set_pheno_data(pd);
  }

}

// [[Rcpp::export]] 
List get_pheno_data(Rcpp::XPtr<CytoSet> cs)
{
  unordered_map<string, vector<string>> pd;
  unsigned nSample = cs->size();
  vector<string> sample_uids(nSample);
  unsigned i = 0;
  //row-major to col-major
  for(const auto & it_cs : *cs)
  {
    sample_uids[i] = it_cs.first;
    const CytoFrameView & fr = it_cs.second;
    //assuming pdata is already homogenious across ghs
    for(const auto & j: fr.get_pheno_data())
    {
      if(i==0)
        pd[j.first] = vector<string>(nSample);
        
      pd[j.first][i] = j.second;
    }
    i++;
  }
  //construct and assign DataFrame directly seems to
  //not preserving DataFrame class info at return
  List res;
  for(const auto & it : pd)
  {
    res[it.first] = it.second;
  }
  res.attr("row.names") = sample_uids;
  res.attr("class") = "data.frame";
  
  return res;
  // return DataFrame(res);
  
  
}



