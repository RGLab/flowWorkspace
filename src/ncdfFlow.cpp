#include "include/ncdfFlow.hpp"
#include <stdexcept>

ncdfFlow::ncdfFlow(string _fileName){
	fileName=_fileName;

}

ncdfFlow::ncdfFlow(){

}

void ncdfFlow::fileName_set(string _fileName){
	fileName=_fileName;

}

string ncdfFlow::fileName_get(){
	return fileName;
}

void ncdfFlow::params_set(vector<string> _params){
	params=_params;
}
vector<string> ncdfFlow::params_get(){
	return params;
}
/*
 * it only read the whole slice (1*nChannels*nEvents)
 * optionally we want to provide the interface for slicing at arbitrary dimensions
 * Note that it is up to caller to free the memory of the returned value

 * this c++ version is meant to replace the original C-version APIs of ncdfFlow package
 * for the easy maintenance
 */
float * ncdfFlow::readSlice(unsigned int sampleID)
{
	if(fileName.empty())
		throw(ios_base::failure("ncdfFlow is not associated with any cdf file yet!\n"));

	NcFile dataFile(fileName.c_str(), NcFile::ReadOnly);
	if (!dataFile.is_valid())
	{
	 throw(ios_base::failure("Couldn't open cdf file!\n"));

	}

	NcVar *data = dataFile.get_var("exprsMat");
	if(!data)
	   throw(domain_error("can't get variable:exprsMat!"));

	NcAtt *eCounts=data->get_att("eventCount");
	if(!eCounts)
	   throw(domain_error("can't get attribute:eventCount!"));

	int nRow=eCounts->as_int(sampleID);
	delete eCounts;

	unsigned nChannels= dataFile.get_dim("channel")->size();

	unsigned int nSize=nRow*nChannels;
	float * mat=new float[nSize];
	data->set_cur(sampleID,0,0);

	data->get(mat,nSize);

	return mat;

}
/*
 * another version of readSlice to return the 1-D matrix as well as dimensions info for 2-D matrix
 * all these info encapsulated within the flowData object
 */
flowData ncdfFlow::readflowData(unsigned int sampleID)
{
	if(fileName.empty())
		throw(ios_base::failure("ncdfFlow is not associated with any cdf file yet!\n"));

	NcFile dataFile(fileName.c_str(), NcFile::ReadOnly);
	if (!dataFile.is_valid())
	{
	 throw(ios_base::failure("Couldn't open cdf file!\n"));

	}

	NcVar *data = dataFile.get_var("exprsMat");
	if(!data)
	   throw(domain_error("can't get variable:exprsMat!"));

	NcAtt *eCounts=data->get_att("eventCount");
	if(!eCounts)
	   throw(domain_error("can't get attribute:eventCount!"));

	int nRow=eCounts->as_int(sampleID);
	delete eCounts;

	unsigned nChannels= dataFile.get_dim("channel")->size();

	unsigned int nSize=nRow*nChannels;
	float * mat=new float[nSize];
	data->set_cur(sampleID,0,0);

	data->get(mat,nSize);
	flowData res(mat,nRow,nChannels);
	res.params_set(params);

	return res;

}

