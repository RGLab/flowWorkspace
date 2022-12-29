#include <cpp11.hpp>
#include <vector>
#include <cytolib/H5CytoFrame.hpp>
#include <cytolib/CytoFrame.hpp>
#include <cytolib/CytoFrameView.hpp>
#include <cytolib/GatingHierarchy.hpp>

#include <flowWorkspace/pairVectorCpp11Convert.h>
#include <flowWorkspace/convert_trans.h>

using namespace cytolib;
[[cpp11::register]] void del_rownames(cpp11::external_pointer<CytoFrameView> fr)
{
  return fr->del_rownames();
}

[[cpp11::register]]
void set_rownames(cpp11::external_pointer<CytoFrameView> fr, vector<string> val)
{
  return fr->set_rownames(val);
}

[[cpp11::register]]
vector<string> get_rownames(cpp11::external_pointer<CytoFrameView> fr)
{
  return fr->get_rownames();
}


[[cpp11::register]]
string backend_type(cpp11::external_pointer<CytoFrameView> fr)
{
	return fmt_to_str(fr->get_backend_type());
}

[[cpp11::register]]
bool cf_is_indexed(cpp11::external_pointer<CytoFrameView> fr)
{
	return fr->is_row_indexed()||fr->is_col_indexed();
}

[[cpp11::register]]
void cf_scale_time_channel_cpp(cpp11::external_pointer<CytoFrameView> fr)
{
	fr->scale_time_channel();
}

[[cpp11::register]]
void cf_set_readonly(cpp11::external_pointer<CytoFrameView> fr, bool flag)
{
	fr->set_readonly(flag);
}

[[cpp11::register]]
void cf_flush_meta_cpp(cpp11::external_pointer<CytoFrameView> fr)
{
	fr->flush_meta();
}

[[cpp11::register]]
void cf_load_meta_cpp(cpp11::external_pointer<CytoFrameView> fr)
{
	fr->load_meta();
}

[[cpp11::register]]
string get_uri(cpp11::external_pointer<CytoFrameView> fr)
{
	return fr->get_uri();
}

[[cpp11::register]]
cpp11::external_pointer<CytoFrameView> realize_view_cytoframe(cpp11::external_pointer<CytoFrameView> fr, string filename)
{
 return cpp11::external_pointer<CytoFrameView>(new CytoFrameView(fr->copy_realized(filename)));
}

[[cpp11::register]]
cpp11::external_pointer<CytoFrameView> copy_view_cytoframe(cpp11::external_pointer<CytoFrameView> fr)
{
  return cpp11::external_pointer<CytoFrameView>(new CytoFrameView(*fr));
}

[[cpp11::register]]
void subset_cytoframe_by_rows(cpp11::external_pointer<CytoFrameView> fr, vector<int> idx)
{
  
  fr->rows_(vector<unsigned>(idx.begin(), idx.end()));
}

[[cpp11::register]]
void subset_cytoframe_by_cols(cpp11::external_pointer<CytoFrameView> fr, vector<int> idx)
{
  
  fr->cols_(vector<unsigned>(idx.begin(), idx.end()));
}
  
 
[[cpp11::register]]
void frm_compensate(cpp11::external_pointer<CytoFrameView> fr, cpp11::doubles_matrix<> spillover){
  cpp11::list dimn(spillover.attr("dimnames"));

  vector<string> marker = cpp11::as_cpp<vector<string>>(cpp11::strings(dimn[1]));
  vector<string> detector;
  if(!Rf_isNull(dimn[0]))
    detector = cpp11::as_cpp<vector<string>>(cpp11::strings(dimn[0]));
  else
    detector = marker;

  auto spill = rmatrix_to_arma(spillover);

  compensation comp(spill, marker, detector);

  fr->compensate(comp);
}

[[cpp11::register]]
void write_to_disk(cpp11::external_pointer<CytoFrameView> fr, string filename, bool ish5){
  FileFormat format = FileFormat::H5;

  fr->write_to_disk(filename, format);
  
}
[[cpp11::register]]
cpp11::external_pointer<CytoFrameView> load_cf(string url, bool readonly, bool on_disk){
    CytoFramePtr ptr = load_cytoframe(url, readonly);

	if(!on_disk)
	{
		ptr.reset(new MemCytoFrame(*ptr));
	}

	return cpp11::external_pointer<CytoFrameView>(new CytoFrameView(ptr));

}


[[cpp11::register]]
cpp11::external_pointer<CytoFrameView> cf_to_memcf(cpp11::external_pointer<CytoFrameView> fr){
    CytoFrameView new_fr = fr->copy_realized();
    CytoFramePtr ptr = new_fr.get_cytoframe_ptr();
    if(ptr->get_backend_type() != FileFormat::MEM){
      string temp_uri = ptr->get_uri();
      ptr.reset(new MemCytoFrame(*ptr));
      // Delete file made in copy to avoid polluting tempdir
      if(!temp_uri.empty())
        fs::remove_all(temp_uri);
    }
    return cpp11::external_pointer<CytoFrameView>(new CytoFrameView(ptr));
}

[[cpp11::register]]
void setMarker(cpp11::external_pointer<CytoFrameView> fr, string channel, string marker){
  fr->set_marker(channel, marker);
}                                      

[[cpp11::register]]
void set_all_channels(cpp11::external_pointer<CytoFrameView> fr, vector<string> new_names){
  fr->set_channels(new_names);
}

[[cpp11::register]]
void setChannel(cpp11::external_pointer<CytoFrameView> fr, string old, string new_name){
  fr->set_channel(old, new_name);
}

[[cpp11::register]]
vector<string> get_channels(cpp11::external_pointer<CytoFrameView> fr){
	return fr->get_channels();
}

[[cpp11::register]]
void append_cols(cpp11::external_pointer<CytoFrameView> fr, vector<string> new_colnames, cpp11::doubles_matrix<> new_cols_mat){
  
  
  // Add the columns to the MemCytoFrame

  fr->append_columns(new_colnames, rmatrix_to_arma(new_cols_mat));
  
}
                                      
[[cpp11::register]]
cpp11::external_pointer<CytoFrameView> parseFCS(string filename, SEXP configr, bool text_only = false
		, string format = "mem", string uri = "")
{
  auto config = sexp_to_fcs_read_param(configr);
  CytoFramePtr ptr;
  unique_ptr<MemCytoFrame> cf(new MemCytoFrame(filename.c_str(), config));
	if(format!="mem"&&text_only)
	{
		cpp11::warning(std::string("text_only is ignored when format is set to 'h5' or 'tile'!"));
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
		fmt = FileFormat::H5;
		cf->write_to_disk(uri, fmt);
		ptr = load_cytoframe(uri, false);
	}

	return cpp11::external_pointer<CytoFrameView>(new CytoFrameView(ptr));
}

[[cpp11::register]]
cpp11::writable::doubles_matrix<> cf_getData(cpp11::external_pointer<CytoFrameView> fr){
  auto ncol = fr->n_cols();
  
  EVENT_DATA_VEC dat = fr->get_data();
  cpp11::writable::doubles_matrix<> mat = arma_to_rmatrix(dat);

  cpp11::writable::strings chnl(fr->get_channels());

  cpp11::writable::strings cid(ncol);
  for(int i = 0; i < ncol; i++)
    cid[i] = "$P" + to_string(i+1) + "N";
  
  chnl.attr("names") = cid;
    
  cpp11::writable::list_of<cpp11::writable::strings> mydims(
      {R_NilValue, chnl});

  auto rn = fr->get_rownames();
  if (rn.size() > 0)
  {
    mydims[0] = cpp11::writable::strings(rn.begin(), rn.end());
  } 

   Rf_setAttrib(cpp11::as_sexp(mat), cpp11::as_sexp({"dimnames"}), cpp11::as_sexp(mydims));
  // mat.attr("dimnames") = mydims; //somehow this doesn't work for matrix class
  return mat;
}
[[cpp11::register]]
void cf_setData(cpp11::external_pointer<CytoFrameView> fr, cpp11::doubles_matrix<> rmat){
 
 fr->set_data(rmatrix_to_arma(rmat));
}

[[cpp11::register]]
void cf_transform_data(cpp11::external_pointer<CytoFrameView> fr, cpp11::list translist){
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

[[cpp11::register]]
string cf_getKeyword(cpp11::external_pointer<CytoFrameView> fr, string key){
  
  string res = fr->get_keyword(key);
  return res;
}

[[cpp11::register]]
SEXP cf_getKeywords(cpp11::external_pointer<CytoFrameView> fr){
  return kw_to_sexp(fr->get_keywords().getPairs());
}

[[cpp11::register]]
void cf_setKeywords(cpp11::external_pointer<CytoFrameView> fr, cpp11::list_of<cpp11::r_string> keys){
    
    KEY_WORDS kws;
    for(int i = 0; i < keys.size(); i++) 
      kws[keys.names()[i]] = cpp11::as_cpp<string>(keys[i]);
    fr->set_keywords(kws);
}

[[cpp11::register]]
void cf_setKeywordsSubset(cpp11::external_pointer<CytoFrameView> fr, cpp11::strings keys, cpp11::strings values){
    for(int i = 0; i < keys.size(); i++)
      fr->set_keyword(keys[i], values[i]);
}

[[cpp11::register]]
void cf_renameKeywords(cpp11::external_pointer<CytoFrameView> fr, cpp11::strings old_keys, cpp11::strings new_keys){
  for(int i = 0; i < old_keys.size(); i++)
    fr->rename_keyword(old_keys[i], new_keys[i]);
}

[[cpp11::register]]
void cf_removeKeywords(cpp11::external_pointer<CytoFrameView> fr, cpp11::strings keys){
  for(int i = 0; i < keys.size(); i++)
    fr->remove_keyword(keys[i]);
}

[[cpp11::register]]
int getncol(cpp11::external_pointer<CytoFrameView> fr){
  
  return fr->n_cols();
}

[[cpp11::register]]
int getnrow(cpp11::external_pointer<CytoFrameView> fr){
  
  return fr->n_rows();
}

[[cpp11::register]]
void setpdata(cpp11::external_pointer<CytoFrameView> fr, cpp11::data_frame df){
	int nChnls = df.nrow();
	//assume channels are consistent between fr and df
	cpp11::strings chnls(df["name"]);
	cpp11::strings markers(df["desc"]);
	cpp11::doubles minRange(df["minRange"]);
	cpp11::doubles maxRange(df["maxRange"]);
	for(int i = 0; i < nChnls; i++)
	{
		string chnl = chnls[i];
		fr->set_marker(chnl, markers[i]);
		fr->set_range(chnl, ColType::channel, pair<float, float>(minRange[i], maxRange[i]));
	}
	//no need to update $Pn keyword based on rownames of df assuming it is done through keyword setter separately
}

[[cpp11::register]]
cpp11::writable::data_frame getpdata(cpp11::external_pointer<CytoFrameView> fr){
  
  int ncol = fr->n_cols();
  cpp11::writable::strings rowid(ncol);
  cpp11::writable::strings names(ncol);
  cpp11::writable::strings desc(ncol);
  cpp11::writable::doubles range(ncol);
  cpp11::writable::doubles minRange(ncol);
  cpp11::writable::doubles maxRange(ncol);
  vector<string> chnl = fr->get_channels();
  vector<string> marker = fr->get_markers();
  vector<unsigned> orig_rowid = fr->get_original_col_ids();
  for(int i = 0; i < ncol; i++)
  {
    rowid[i] = "$P" + to_string(orig_rowid[i]+1);
    names[i] = chnl[i];
    if(marker[i].empty())
      desc[i] = NA_STRING;
    else
      desc[i] = marker[i];
    pair<float, float> r = fr->get_range(chnl[i], ColType::channel, RangeType::instrument);
    range[i] = r.second;
    maxRange[i] = r.second;
    minRange[i] = r.first;
  }
  rowid.attr("class") = "AsIs";
  desc.attr("class") = "AsIs";
  names.attr("class") = "AsIs";
  using namespace cpp11::literals;

  cpp11::writable::data_frame df ({"name"_nm = names
                             ,"desc"_nm = desc
                             ,"range"_nm = range
                             ,"minRange"_nm = minRange
                             ,"maxRange"_nm = maxRange
                              }
                             );
  df.attr("row.names") = rowid;
  return df;
}
