#include <cytolib/H5CytoFrame.hpp>
#include <cytolib/CytoFrame.hpp>
#include <flowWorkspace/pairVectorRcppWrap.h>
using namespace Rcpp;
using namespace cytolib;

// [[Rcpp::export]]
string backend_type(Rcpp::XPtr<CytoFrameView> fr)
{
	return fmt_to_str(fr->get_backend_type());
}

// [[Rcpp::export(name=".cf_scale_time_channel")]]
void cf_scale_time_channel(Rcpp::XPtr<CytoFrameView> fr)
{
	fr->scale_time_channel();
}
// [[Rcpp::export]]
void cf_set_readonly(Rcpp::XPtr<CytoFrameView> fr, bool flag)
{
	fr->set_readonly(flag);
}
// [[Rcpp::export(name=".cf_flush_meta")]]
void cf_flush_meta(Rcpp::XPtr<CytoFrameView> fr)
{
	fr->flush_meta();
}
// [[Rcpp::export(name=".cf_load_meta")]]
void cf_load_meta(Rcpp::XPtr<CytoFrameView> fr)
{
	fr->load_meta();
}

// [[Rcpp::export]]
string get_uri(Rcpp::XPtr<CytoFrameView> fr)
{
	return fr->get_uri();
}

// [[Rcpp::export]]
Rcpp::XPtr<CytoFrameView> realize_view_cytoframe(Rcpp::XPtr<CytoFrameView> fr, string filename)
{
 return XPtr<CytoFrameView>(new CytoFrameView(fr->copy_realized(filename)));
}

// [[Rcpp::export]]
Rcpp::XPtr<CytoFrameView> copy_view_cytoframe(Rcpp::XPtr<CytoFrameView> fr)
{
  return XPtr<CytoFrameView>(new CytoFrameView(*fr));
}

// [[Rcpp::export]]
void subset_cytoframe_by_rows(Rcpp::XPtr<CytoFrameView> fr, vector<unsigned> idx)
{
  
  fr->rows_(idx);
}

// [[Rcpp::export]]
void subset_cytoframe_by_cols(Rcpp::XPtr<CytoFrameView> fr, vector<unsigned> idx)
{
  
  fr->cols_(idx);
}
 /*
  * subset by cols and rows in place for each frames
  */  
 
 // unsigned short r_type = TYPEOF(r_obj);
 // // Rcout << "r_type:" << r_type << endl;  
 // for(auto & it : *cs_new)
 // {
 //   if(j_type != NILSXP)
 //     it.second->cols_(ch_selected, ColType::channel);
 //   if(r_type != NILSXP)
 //     it.second->rows_(convert_to_uint_idx(it.second->n_rows(), r_obj));
 // }
 
 
// [[Rcpp::export]] 
void frm_compensate(Rcpp::XPtr<CytoFrameView> fr, NumericMatrix spillover){
  vector<string> marker = as<vector<string>>(colnames(spillover));
  mat spill = as<mat>(spillover);
  // spill.print(Rcout, "spill");
  compensation comp(spill, marker);
  // comp.get_spillover_mat().print(Rcout, "comp");
  fr->compensate(comp);
}

// [[Rcpp::export]]
void write_to_disk(Rcpp::XPtr<CytoFrameView> fr, string filename, bool ish5, CytoCtx ctx){
  FileFormat format = ish5?FileFormat::H5:FileFormat::TILE;

  fr->write_to_disk(filename, format, ctx);
  
}
// [[Rcpp::export]]
XPtr<CytoFrameView> load_cf(string url, bool readonly, bool on_disk,CytoCtx ctx){
    CytoFramePtr ptr = load_cytoframe(url, readonly, ctx);

	if(!on_disk)
	{
		ptr.reset(new MemCytoFrame(*ptr));
	}

	return Rcpp::XPtr<CytoFrameView>(new CytoFrameView(ptr));

}


// [[Rcpp::export]]
XPtr<CytoFrameView> cf_to_memcf(Rcpp::XPtr<CytoFrameView> fr){
    CytoFrameView new_fr = fr->copy_realized();
    CytoFramePtr ptr = new_fr.get_cytoframe_ptr();
    if(ptr->get_backend_type() != FileFormat::MEM){
      string temp_uri = ptr->get_uri();
      ptr.reset(new MemCytoFrame(*ptr));
      // Delete file made in copy to avoid polluting tempdir
      if(!temp_uri.empty())
        fs::remove_all(temp_uri);
    }
    return Rcpp::XPtr<CytoFrameView>(new CytoFrameView(ptr));
}

// [[Rcpp::export]] 
void setMarker(Rcpp::XPtr<CytoFrameView> fr, string channel, string marker){
  fr->set_marker(channel, marker);
}                                      

// [[Rcpp::export]] 
void set_all_channels(Rcpp::XPtr<CytoFrameView> fr, vector<string> new_names){
  fr->set_channels(new_names);
}

// [[Rcpp::export]]
void setChannel(Rcpp::XPtr<CytoFrameView> fr, string old, string new_name){
  fr->set_channel(old, new_name);
}

// [[Rcpp::export]]
Rcpp::XPtr<CytoFrameView> append_cols(Rcpp::XPtr<CytoFrameView> fr, vector<string> new_colnames, NumericMatrix new_cols_mat){
  
  CytoFrameView new_fr = fr->copy_realized();
  CytoFramePtr ptr = new_fr.get_cytoframe_ptr();
  string new_uri = ptr->get_uri();
  FileFormat fmt = ptr->get_backend_type();
  
  // For now, push all backends through MemCytoFrame::append_columns
  // by conversion
  if(fmt != FileFormat::MEM)
    ptr.reset(new MemCytoFrame(*ptr));
  
  // Add the columns to the MemCytoFrame
  arma::mat new_cols = as<arma::mat>(new_cols_mat);
  ptr->append_columns(new_colnames, new_cols);
  
  // If necessary, convert back to prior backend, overwriting the copy uri.
  if(fmt != FileFormat::MEM){
    ptr->write_to_disk(new_uri, fmt);
    ptr = load_cytoframe(new_uri, false);
  }
  
  return Rcpp::XPtr<CytoFrameView>(new CytoFrameView(ptr));
}
                                      
// [[Rcpp::export]] 
Rcpp::XPtr<CytoFrameView> parseFCS(string filename, FCS_READ_PARAM config, bool text_only = false
		, string format = "mem", string uri = "")
{
	CytoFramePtr ptr;
	unique_ptr<MemCytoFrame> cf(new MemCytoFrame(filename.c_str(), config));
	if(format!="mem"&&text_only)
	{
		warning("text_only is ignored when format is set to 'h5' or 'tile'!");
		text_only = false;
	}
	if(text_only)
		cf->read_fcs_header();
	else
		cf->read_fcs();
	if(format=="mem")
	{
		ptr.reset(cf.release());
	}
	else
	{
		FileFormat fmt;
		if(format == "h5")
			fmt = FileFormat::H5;
		else
			fmt = FileFormat::TILE;
		cf->write_to_disk(uri, fmt);
		ptr = load_cytoframe(uri, false);
	}

	return Rcpp::XPtr<CytoFrameView>(new CytoFrameView(ptr));
}

// [[Rcpp::export]] 
NumericVector cf_getData(Rcpp::XPtr<CytoFrameView> fr){
  // int nrow = fr->n_rows();
  int ncol = fr->n_cols();
  // int ntotal = ncol * nrow;
  
  EVENT_DATA_VEC dat = fr->get_data();
  NumericMatrix mat = wrap(dat);
  // mat.attr("dim") = Dimension(nrow, ncol);
  StringVector chnl = wrap(fr->get_channels());
  StringVector cid(ncol);
  for(int i = 0; i < ncol; i++)
    cid[i] = "$P" + to_string(i+1) + "N";
  
    
  chnl.attr("names") = cid;
  mat.attr("dimnames") = List::create(R_NilValue, chnl);
  return mat;
}
// [[Rcpp::export]]
void cf_setData(Rcpp::XPtr<CytoFrameView> fr, EVENT_DATA_VEC data){
 fr->set_data(data);
}

// [[Rcpp::export]] 
string cf_getKeyword(Rcpp::XPtr<CytoFrameView> fr, string key){
  
  string res = fr->get_keyword(key);
  return res;
}

// [[Rcpp::export]] 
KW_PAIR cf_getKeywords(Rcpp::XPtr<CytoFrameView> fr){
  // return fr->getKeywords().getPairs();
  return fr->get_keywords().getPairs();
}

// [[Rcpp::export]] 
void setKeywords(Rcpp::XPtr<CytoFrameView> fr, List keys){
    vector<string> names = keys.names();
    KEY_WORDS kws;
    for(int i = 0; i < keys.size(); i++) 
      kws[names[i]] = as<string>(keys[i]);
    fr->set_keywords(kws);
}
// [[Rcpp::export]] 
int getncol(Rcpp::XPtr<CytoFrameView> fr){
  
  return fr->n_cols();
}

// [[Rcpp::export]] 
int getnrow(Rcpp::XPtr<CytoFrameView> fr){
  
  return fr->n_rows();
}

// [[Rcpp::export]] 
void setpdata(Rcpp::XPtr<CytoFrameView> fr, Rcpp::DataFrame df){
	int nChnls = df.nrows();
	//assume channels are consistent between fr and df
	vector<string> chnls = df["name"];
	vector<string> markers = df["desc"];
	vector<float> minRange = df["minRange"];
	vector<float> maxRange = df["maxRange"];
	for(int i = 0; i < nChnls; i++)
	{
		string chnl = chnls[i];
		fr->set_marker(chnl, markers[i]);
		fr->set_range(chnl, ColType::channel, pair<float, float>(minRange[i], maxRange[i]));
	}
	//no need to update $Pn keyword based on rownames of df assuming it is done through keyword setter separately
}

// [[Rcpp::export]]
Rcpp::DataFrame getpdata(Rcpp::XPtr<CytoFrameView> fr){
  
  int ncol = fr->n_cols();
  StringVector rowid(ncol);
  StringVector names(ncol);
  StringVector desc(ncol);
  NumericVector range(ncol);
  NumericVector minRange(ncol);
  NumericVector maxRange(ncol);
  vector<string> chnl = fr->get_channels();
  vector<string> marker = fr->get_markers();
  vector<unsigned> orig_rowid = fr->get_original_col_ids();
  for(int i = 0; i < ncol; i++)
  {
    rowid[i] = "$P" + to_string(orig_rowid[i]+1);
    names[i] = chnl[i];
    if(marker[i].empty())
      desc[i] = StringVector::get_na();
    else
      desc[i] = marker[i];
    pair<float, float> r = fr->get_range(chnl[i], ColType::channel, RangeType::instrument);
    maxRange[i] = range[i] = r.second;
    minRange[i] = r.first;
  }
  rowid.attr("class") = "AsIs";
  desc.attr("class") = "AsIs";
  names.attr("class") = "AsIs";
  DataFrame df = DataFrame::create(Named("name") = names
                             ,Named("desc") = desc
                             ,Named("range") = range
                             ,Named("minRange") = minRange
                             ,Named("maxRange") = maxRange
                             );
  df.attr("row.names") = rowid;
  return df;
}
