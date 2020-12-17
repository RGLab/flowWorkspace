#include <cytolib/H5CytoFrame.hpp>
#include <cytolib/CytoFrame.hpp>
#include <cytolib/utils.hpp>
#include <flowWorkspace/pairVectorRcppWrap.h>
#include <flowWorkspace/convert_trans.h>
using namespace Rcpp;
using namespace cytolib;

// [[Rcpp::export]]
void del_rownames(Rcpp::XPtr<CytoFrameView> fr)
{
  return fr->del_rownames();
}

// [[Rcpp::export]]
void set_rownames(Rcpp::XPtr<CytoFrameView> fr, vector<string> val)
{
  return fr->set_rownames(val);
}

// [[Rcpp::export]]
vector<string> get_rownames(Rcpp::XPtr<CytoFrameView> fr)
{
  return fr->get_rownames();
}


// [[Rcpp::export]]
string backend_type(Rcpp::XPtr<CytoFrameView> fr)
{
	return fmt_to_str(fr->get_backend_type());
}

// [[Rcpp::export]]
bool cf_is_indexed(Rcpp::XPtr<CytoFrameView> fr)
{
	return fr->is_row_indexed()||fr->is_col_indexed();
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
void write_to_disk(Rcpp::XPtr<CytoFrameView> fr, string filename, bool ish5, XPtr<CytoCtx> ctx){
  FileFormat format = ish5?FileFormat::H5:FileFormat::TILE;

  fr->write_to_disk(filename, format, *ctx);
  
}
// [[Rcpp::export]]
XPtr<CytoFrameView> load_cf(string url, bool readonly, bool on_disk,XPtr<CytoCtx> ctx){
    CytoFramePtr ptr = load_cytoframe(url, readonly, *ctx);

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
vector<string> get_channels(Rcpp::XPtr<CytoFrameView> fr){
	return fr->get_channels();
}

// [[Rcpp::export]]
Rcpp::XPtr<CytoFrameView> append_cols(Rcpp::XPtr<CytoFrameView> fr, vector<string> new_colnames, NumericMatrix new_cols_mat){
  
  
  // Add the columns to the MemCytoFrame
  arma::mat new_cols = as<arma::mat>(new_cols_mat);
  fr->append_columns(new_colnames, new_cols);
  
  return fr;
}

// [[Rcpp::export]]
List load_fcs_cpp(vector<string> filenames, FCS_READ_PARAM config, bool text_only, string format, vector<string> uri, int num_threads)
{

  FileFormat fmt;
  if(format == "mem")
  {
    fmt = FileFormat::MEM;
  }
  else if(format == "tile")
    fmt = FileFormat::TILE;
  else
    fmt = FileFormat::H5;
  
	auto cf_ptrs = load_fcs(filenames, config, text_only, fmt, uri, num_threads);
	int n = cf_ptrs.size();
	List res(n);
	for(int i = 0; i< n; i++)
	{
	  res[i] = Rcpp::XPtr<CytoFrameView>(new CytoFrameView(cf_ptrs[i]));
	}
	return res;
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
  colnames(mat) = chnl;
  auto rn = fr->get_rownames();
  if(rn.size()>0)
	  rownames(mat) = wrap(rn);
  return mat;
}
// [[Rcpp::export]]
void cf_setData(Rcpp::XPtr<CytoFrameView> fr, EVENT_DATA_VEC data){
 fr->set_data(data);
}

// [[Rcpp::export]]
void cf_transform_data(Rcpp::XPtr<CytoFrameView> fr, List translist){
  trans_map trans = convert_transformer_list(translist);
  trans_local t_local = trans_local();
  t_local.setTransMap(trans);
  CytoFramePtr cf = fr->get_cytoframe_ptr();
  
  MemCytoFrame cf_mem(*cf);
  cf_mem.transform_data(t_local);
  
  cf->set_data(cf_mem.get_data());
  cf->set_params(cf_mem.get_params());
  cf->set_keywords(cf_mem.get_keywords());
  
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
void cf_setKeywords(Rcpp::XPtr<CytoFrameView> fr, List keys){
    vector<string> names = keys.names();
    KEY_WORDS kws;
    for(int i = 0; i < keys.size(); i++) 
      kws[names[i]] = as<string>(keys[i]);
    fr->set_keywords(kws);
}

// [[Rcpp::export]]
void cf_setKeywordsSubset(Rcpp::XPtr<CytoFrameView> fr, StringVector keys, StringVector values){
    for(int i = 0; i < keys.size(); i++)
      fr->set_keyword(as<string>(keys[i]), as<string>(values[i]));
}

// [[Rcpp::export]]
void cf_renameKeywords(Rcpp::XPtr<CytoFrameView> fr, StringVector old_keys, StringVector new_keys){
  for(int i = 0; i < old_keys.size(); i++)
    fr->rename_keyword(as<string>(old_keys[i]), as<string>(new_keys[i]));
}

// [[Rcpp::export]]
void cf_removeKeywords(Rcpp::XPtr<CytoFrameView> fr, StringVector keys){
  for(int i = 0; i < keys.size(); i++)
    fr->remove_keyword(as<string>(keys[i]));
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
