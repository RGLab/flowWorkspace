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
//float * ncdfFlow::readSlice(unsigned int sampleID)
//{
//	if(fileName.empty())
//		throw(ios_base::failure("ncdfFlow is not associated with any cdf file yet!\n"));
//
//	NcFile dataFile(fileName.c_str(), NcFile::ReadOnly);
//	if (!dataFile.is_valid())
//	{
//	 throw(ios_base::failure("Couldn't open cdf file!\n"));
//
//	}
//
//	NcVar *data = dataFile.get_var("exprsMat");
//	if(!data)
//	   throw(domain_error("can't get variable:exprsMat!"));
//
//	NcAtt *eCounts=data->get_att("eventCount");
//	if(!eCounts)
//	   throw(domain_error("can't get attribute:eventCount!"));
//
//	int nRow=eCounts->as_int(sampleID);
//	delete eCounts;
//
//	unsigned nChannels= dataFile.get_dim("channel")->size();
//
//	unsigned int nSize=nRow*nChannels;
//	float * mat=new float[nSize];
//	data->set_cur(sampleID,0,0);
//
//	data->get(mat,nSize);
//
//	return mat;
//
//}
/*
 * another version of readSlice to return the 1-D matrix as well as dimensions info for 2-D matrix
 * all these info encapsulated within the flowData object
 */
//flowData ncdfFlow::readflowData(unsigned int sampleID)
//{
//	if(fileName.empty())
//		throw(ios_base::failure("ncdfFlow is not associated with any cdf file yet!\n"));
//	cout<<"opening file:"<<fileName.c_str()<<endl;
//	NcFile dataFile(fileName.c_str(), NcFile::ReadOnly);
//	if (!dataFile.is_valid())
//	{
//	 throw(ios_base::failure("Couldn't open cdf file!\n"));
//
//	}
//
//	NcVar *data = dataFile.get_var("exprsMat");
//	if(!data)
//	   throw(domain_error("can't get variable:exprsMat!"));
//
//	NcAtt *eCounts=data->get_att("eventCount");
//	if(!eCounts)
//	   throw(domain_error("can't get attribute:eventCount!"));
//
//	int nRow=eCounts->as_int(sampleID);
//	delete eCounts;
//
//	unsigned nChannels= dataFile.get_dim("channel")->size();
//
//	unsigned int nSize=nChannels*nRow;
//	float * mat=new float[nSize];
//	data->set_cur(sampleID,0,0);
//
//	data->get(mat,1,nChannels,nRow);
//	flowData res(mat,nRow,nChannels);
//	res.params_set(params);
//
//	return res;
//
//}
/*
 * modified c version of readSlice,
 * there is conflicts when loading both netcdf_c++ and netcdf shared library
 * so it may be safer to stick to c library if this is the only cdf function we use here
 */
#define ERR(e) {throw(ios_base::failure(nc_strerror(e)));}

flowData ncdfFlow::readflowData(unsigned int sampleID)
{
	if(fileName.empty())
		throw(ios_base::failure("ncdfFlow is not associated with any cdf file yet!\n"));
	cout<<"opening file:"<<fileName.c_str()<<endl;

	int retval, ncid, varid, colStart, colEnd, nRow;

	if ((retval = nc_open(fileName.c_str(), NC_NOWRITE,&ncid)))
			ERR(retval);

	if((retval = nc_inq_varid (ncid, "exprsMat", &varid)))
			ERR(retval);

	int sampCount;
	if((retval = nc_get_att_int(ncid, varid, "sampleCount", &sampCount)))
		ERR(retval);

	int *eCount = new int[sampCount];
	if((retval = nc_get_att_int(ncid, varid, "eventCount", eCount)))
		ERR(retval);
	nRow = eCount[sampleID] ;

	delete eCount;

	int dimID;
	long int nChannels;
    if((retval=nc_inq_dimid(ncid, "channel", &dimID)))
    	ERR(retval);
    ncdiminq(ncid, dimID, 0, &nChannels);

    unsigned int nSize=nChannels*nRow;

    size_t start[] = {sampleID, 0, 0};
	size_t count[] = {1,nChannels, nRow};

	double * mat=new double[nSize];

	if((retval = nc_get_vara_double(ncid, varid, start, count, mat)))
		ERR(retval);

	flowData res(mat,(unsigned int)nRow,(unsigned int)nChannels);
	res.params_set(params);

	return res;

}
