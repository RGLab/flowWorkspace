#include <cytolib/H5CytoFrame.hpp>
#include <flowWorkspace/pairVectorCpp11Convert.h>
#include <flowWorkspace/list_to_comp.h>

using namespace cytolib;



[[cpp11::register]]
void cs_set_compensation(cpp11::external_pointer<GatingSet> cs, cpp11::list comps, bool compensate_data){

	unordered_map<string, compensation> comp_objs = list_to_comps(comps);
	for(auto sn : cs->get_sample_uids())
	{
		GatingHierarchyPtr gh = cs->getGatingHierarchy(sn);
		auto it = comp_objs.find(sn);
		if(it==comp_objs.end())
			throw(domain_error("compensation not found for: " + sn));
		compensation comp = it->second;
		gh->set_compensation(comp, false);
		if(compensate_data)
		{
			//assume always dealing with h5 based gs
			auto &fr = static_cast<H5CytoFrame &>(*(gh->get_cytoframe_view().get_cytoframe_ptr()));
			gh->compensate(fr);
		}
	}
}


[[cpp11::register]]
void set_cytoframe(cpp11::external_pointer<GatingSet> cs, string sn, cpp11::external_pointer<CytoFrameView> fr)
{
	cs->update_cytoframe_view(sn, *fr);

}

[[cpp11::register]]
void add_cytoframe(cpp11::external_pointer<GatingSet> cs, string sn, cpp11::external_pointer<CytoFrameView> fr)
{
	cs->add_cytoframe_view(sn, *fr);

}


[[cpp11::register]]
cpp11::external_pointer<GatingSet> new_cytoset()
{

  cpp11::external_pointer<GatingSet> cs(new GatingSet());

  return cs;

}

[[cpp11::register]]
cpp11::external_pointer<GatingSet> fcs_to_cytoset(cpp11::strings files
		, cpp11::list rconfig, string backend, string backend_dir)
{
    auto config = sexp_to_fcs_read_param(rconfig);
    FileFormat fmt;
    if(backend == "mem")
		fmt = FileFormat::MEM;
	else
		fmt = FileFormat::H5;
    auto n = files.size();
    vector<pair<string, string>> sample_uid_vs_file_path(n);
    cpp11::strings sids(files.names());
    if(sids.size()!=n)
        cpp11::stop("file paths must be a named characters!");
    for (size_t i = 0; i < n; i++)
    {
        sample_uid_vs_file_path[i].first = sids[i];
        sample_uid_vs_file_path[i].second = files[i];
    }

    cpp11::external_pointer<GatingSet> cs(new GatingSet(sample_uid_vs_file_path, config, fmt, backend_dir));

    return cs;
  
}


[[cpp11::register]]
vector<string> get_colnames(cpp11::external_pointer<GatingSet> cs){
  return cs->get_channels();
}

[[cpp11::register]]
cpp11::external_pointer<GatingSet> realize_view_cytoset(cpp11::external_pointer<GatingSet> cs, string path)
{
  return cpp11::external_pointer<GatingSet>(new GatingSet(cs->copy(true, true, path)));
}

[[cpp11::register]]
cpp11::external_pointer<GatingSet> copy_view_cytoset(cpp11::external_pointer<GatingSet> cs)
{
  return cpp11::external_pointer<GatingSet>(new GatingSet(cs->copy(false, true, "")));

}

[[cpp11::register]]
void subset_cytoset_by_rows(cpp11::external_pointer<GatingSet> cs
                                             , string sn
                                             , vector<int> idx
                                            )
{
  
  cs->get_cytoframe_view_ref(sn).rows_(vector<unsigned>(idx.begin(), idx.end()));
}
  
[[cpp11::register]]
void subset_cytoset(cpp11::external_pointer<GatingSet> cs
                                     , vector<string> sample_uids
                                     , vector<string> ch_selected
                                      )
{
  
  /*
  * parse i index (sample name)
  */
//   unsigned short i_type = TYPEOF(i_obj);
//   // Rcout << "i_type:" << i_type << endl;
//   if(i_type != NILSXP)
//   {
//     StringVector sample_uids;
//     if(i_type == STRSXP)
//       sample_uids = cpp11::strings(i_obj);
//     else
//     {
      
//       sample_uids = convert_to_str_idx(wrap(cs->get_sample_uids()), i_obj);
//     }
    if(sample_uids.size()>0)
        cs->sub_samples_(sample_uids);
//   }
  
  /*
   * parse j index (col)
   */  
//   unsigned short j_type = TYPEOF(j_obj);
//   // Rcout << "j_type:" << j_type << endl;
//   vector<string> ch_selected;
//   if(j_type != NILSXP)
//   {
//     // Rcout << "STRSXP:" << STRSXP << endl;
    
//     if(j_type == STRSXP)
//       ch_selected = as<vector<string>>(as<StringVector>(j_obj));
//     else
//       ch_selected = as<vector<string>>(convert_to_str_idx(wrap(cs->get_channels()), j_obj));
if(ch_selected.size()>0)
    cs->cols_(ch_selected, ColType::channel);
//   }
  
}

[[cpp11::register]]
cpp11::external_pointer<CytoFrameView> get_cytoframe(cpp11::external_pointer<GatingSet> cs
                , string sample_uid
                                     , vector<string> ch_selected
                )
{
  
  /*
   * parse i index (sample name)
   */
//   std::string sample_uid;
//   unsigned short i_type = i_obj.sexp_type();

//   if(i_type  == STRSXP)
//   {
//     sample_uid = Rcpp::as<std::string>(i_obj.get__());
//   }
//   else if(i_type == REALSXP || i_type == INTSXP)
//   {
//     unsigned s_ind = Rcpp::as<unsigned>(i_obj.get__());
//     sample_uid =  cs->get_sample_uids()[s_ind - 1];
//   }
//   else
//     Rcpp::stop("unsupported i type!");

  
  cpp11::external_pointer<CytoFrameView> frame(new CytoFrameView(cs->get_cytoframe_view(sample_uid)));
  /*
  * parse j index (channel names)
  */
   /*
  * subset by j if applicable
  */
//   int j_type = j_obj.sexp_type();

//   if(j_type != NILSXP)
//   {
//     //get local channel names
//     StringVector colnames = wrap(frame->get_channels());
//     StringVector ch_selected;
    
//     //creating j index used for subsetting colnames and pdata
//     Rcpp::IntegerVector j_indx;

//     if(j_type == STRSXP)//when character vector
//     {
//       ch_selected = StringVector(j_obj.get__());
  
//     }
//     else if(j_type == LGLSXP)
//     {
//       Rcpp::LogicalVector j_val(j_obj.get__());
//       //convert to integer vector
//       unsigned nSel = Rcpp::sum(j_val);
//       unsigned thisCount = 0;
//       j_indx = Rcpp::IntegerVector(nSel);
//       for(unsigned i = 0; i < j_val.size(); i++){
//         if(j_val(i)){
//           j_indx(thisCount++) = i;
//         }
  
//       }
  
//       ch_selected = colnames[j_indx];
//     }
//     else if(j_type == INTSXP || j_type == REALSXP)
//     {
//       Rcpp::IntegerVector j_val(j_obj.get__());
//       j_indx = j_val - 1; //convert to 0-based index
//       ch_selected = colnames[j_indx];
//     }
//     else
//       Rcpp::stop("unsupported j expression!");
    if(ch_selected.size()>0)
        frame->cols_(ch_selected, ColType::channel);
//   }
  
  return frame;
  
   
}

[[cpp11::register]]
void set_pheno_data(cpp11::external_pointer<GatingSet> cs, cpp11::data_frame value)
{
  
  cpp11::strings sample_uids(value.attr("row.names"));
  cpp11::strings colnames(value.names());
  
  for(auto i = 0; i < value.nrow(); i++)
  {
    CytoFrameView & fr = cs->get_cytoframe_view_ref(sample_uids[i]);
    PDATA pd;
    for(const string & key : colnames)
    {
      cpp11::strings col(value[key]);
      pd[key] = col[i];
    }
    fr.set_pheno_data(pd);
  }

}

[[cpp11::register]]
cpp11::writable::list get_pheno_data(cpp11::external_pointer<GatingSet> cs)
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
  //construct and assign cpp11::data_frame directly seems to
  //not preserving cpp11::data_frame class info at return
  cpp11::writable::list res;
  for(const auto & it : pd)
  {
      res.push_back(cpp11::named_arg(it.first.c_str()) = cpp11::as_sexp<cpp11::writable::strings>(it.second));
  }
  if(sample_uids.size()>0)
  {
      res.attr("row.names") = cpp11::as_sexp<cpp11::writable::strings>(sample_uids);
    res.attr("class") = "data.frame";
  }
  return res;

  
}



