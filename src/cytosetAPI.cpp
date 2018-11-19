#include <flowWorkspace/pairVectorRcppWrap.h>
#include <flowWorkspace/convert_to_str_idx.h>
using namespace Rcpp;
using namespace cytolib;

//[[Rcpp::plugins("temp")]]

//convert from list of R matrix to compensations
unordered_map<string, compensation> list_to_comps(List comps);

// [[Rcpp::export]] 
void cs_compensate(Rcpp::XPtr<GatingSet> cs, List comps){

	unordered_map<string, compensation> comp_objs = list_to_comps(comps);
	string dir = generate_unique_dir(fs::temp_directory_path(), "gs");
	for(auto sn : cs->get_sample_uids())
	{
		GatingHierarchyPtr gh = cs->getGatingHierarchy(sn);
		auto it = comp_objs.find(sn);
		if(it==comp_objs.end())
			throw(domain_error("compensation not found for: " + sn));
		compensation comp = it->second;
		gh->set_compensation(comp, false);
		MemCytoFrame fr(*(gh->get_cytoframe_view().get_cytoframe_ptr()));
		gh->compensate(fr);
		string h5file = dir + "/" + sn + ".h5";
		fr.write_h5(h5file);
		gh->set_cytoFrame_view(CytoFrameView(CytoFramePtr(new H5CytoFrame(h5file))));


	}
}
// [[Rcpp::export]]
Rcpp::XPtr<GatingSet> fcs_to_cytoset(vector<pair<string,string>> sample_uid_vs_file_path, const FCS_READ_PARAM & config, bool is_h5, string h5_dir)
{

  Rcpp::XPtr<GatingSet> cs(new GatingSet(sample_uid_vs_file_path, config, is_h5, h5_dir));
  
  return cs;
  
}


// [[Rcpp::export]] 
vector<string> get_colnames(Rcpp::XPtr<GatingSet> cs){
  return cs->get_channels();
}

// [[Rcpp::export]]
Rcpp::XPtr<GatingSet> realize_view_cytoset(Rcpp::XPtr<GatingSet> cs, string path)
{
  return XPtr<GatingSet>(new GatingSet(cs->copy(true, true, path)));
}

// [[Rcpp::export]]
Rcpp::XPtr<GatingSet> shallow_copy_cytoset(Rcpp::XPtr<GatingSet> cs)
{
  return XPtr<GatingSet>(new GatingSet(*cs));
}

// [[Rcpp::export]]
void subset_cytoset_by_rows(Rcpp::XPtr<GatingSet> cs
                                             , string sn
                                             , vector<unsigned> idx
                                            )
{
  
  cs->get_cytoframe_view_ref(sn).rows_(idx);
}
  
// [[Rcpp::export]]
void subset_cytoset(Rcpp::XPtr<GatingSet> cs
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
Rcpp::XPtr<CytoFrameView> get_cytoFrame(Rcpp::XPtr<GatingSet> cs
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
void set_pheno_data(Rcpp::XPtr<GatingSet> cs, DataFrame value)
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
List get_pheno_data(Rcpp::XPtr<GatingSet> cs)
{
  unordered_map<string, vector<string>> pd;
  vector<string> sample_uids = cs->get_sample_uids();
  unsigned nSample = sample_uids.size();
  //row-major to col-major
  for(unsigned i = 0; i < nSample; i++)
  {
	string sn = sample_uids[i];
	const GatingHierarchy & fr = *(cs->getGatingHierarchy(sn));
	//assuming pdata is already homogenious across ghs
	for(const auto & j: fr.get_pheno_data())
	{
	  if(i==0)
		pd[j.first] = vector<string>(nSample);

	  pd[j.first][i] = j.second;
	}
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



