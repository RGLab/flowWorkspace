#include "ncdfFlow.hpp"
#include <stdexcept>
#include <algorithm>
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

void ncdfFlow::sample_set(vector<string> _sampleNames){
	sampleNames=_sampleNames;
}


vector<string> ncdfFlow::params_get(){
	return params;
}

/*
 * modified c version of readSlice,
 * there is conflicts when loading both netcdf_c++ and netcdf shared library
 * so it may be safer to stick to c library if this is the only cdf function we use here
 */
#define ERR(e) {throw(ios_base::failure(nc_strerror(e)));}

flowData ncdfFlow::readflowData(string sampleName)
{

	vector<string>::iterator it=find(sampleNames.begin(),sampleNames.end(),sampleName);
	if(it==sampleNames.end())
		throw(domain_error("sampleName not found in cdf file!"));
	unsigned sampleInd=it-sampleNames.begin();
	return readflowData(sampleInd);
}



flowData ncdfFlow::readflowData(unsigned int sampleID)
{
	if(fileName.empty())
		throw(ios_base::failure("ncdfFlow is not associated with any cdf file yet!\n"));

//	cout<<"opening file:"<<fileName.c_str()<<endl;
	/*
	 *opening file
	 */
	int retval, ncid, varid, nRow;

//	int startTime, endTime, totalTime;

//	startTime = time(NULL);

	if ((retval = nc_open(fileName.c_str(), NC_NOWRITE,&ncid)))
			ERR(retval);

//	endTime = time(NULL);
//
//	totalTime = endTime - startTime;


	/*
	 * start reading data
	 */

//	startTime = time(NULL);

	if((retval = nc_inq_varid (ncid, "exprsMat", &varid)))
			ERR(retval);

	int sampCount;
	if((retval = nc_get_att_int(ncid, varid, "sampleCount", &sampCount)))
		ERR(retval);

	int *eCount = new int[sampCount];
	if((retval = nc_get_att_int(ncid, varid, "eventCount", eCount)))
		ERR(retval);
	nRow = eCount[sampleID] ;

	delete[] eCount;

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

//	endTime = time(NULL);


//	std::cout << "reading data cost: " << endTime - startTime << " seconds.";

	flowData res(mat,params,(unsigned int)nRow,sampleID);

	delete[] mat;

//	startTime = time(NULL);

	if((retval = nc_close(ncid)))
		ERR(retval);

//	endTime = time(NULL);
//
//	totalTime += endTime - startTime;
//	std::cout << "open/close cost: " << totalTime << " seconds.";

	return res;

}
flowData hdfFlow::readflowData(string sampleName)
{

	vector<string>::iterator it=find(sampleNames.begin(),sampleNames.end(),sampleName);
	if(it==sampleNames.end())
		throw(domain_error("sampleName not found in cdf file!"));
	unsigned sampleInd=it-sampleNames.begin();
	return readflowData(sampleInd);
}
flowData hdfFlow::readflowData(unsigned int sampleIndx)
{
	if(fileName.empty())
		throw(ios_base::failure("ncdfFlow is not associated with any cdf file yet!\n"));

	/*
	 * Open the file and the dataset.
	 */
	hid_t       file, dataset,dataspace, memspace;         /* handles */
	hsize_t 	dimsm[2]; //dimenstions
	herr_t      status;
	file = H5Fopen(fileName.c_str(), H5F_ACC_RDONLY, H5P_DEFAULT);
	dataset = H5Dopen2(file, DATASETNAME, H5P_DEFAULT);
	dataspace = H5Dget_space(dataset);    /* dataspace handle */

	/*
	 * get the total number of events for the current sample
	 */
   hsize_t dims[3];
   hid_t attrID;
   status  = H5Sget_simple_extent_dims(dataspace, dims, NULL); //get dimensions of datset
   unsigned nSample = dims[0];//get total number of samples
   unsigned * eCount = (unsigned *) malloc(nSample * sizeof(unsigned));
   attrID = H5Aopen(dataset, "eventCount", H5P_DEFAULT);
   status = H5Aread(attrID, H5T_NATIVE_UINT32, eCount);
   unsigned nEvents = eCount[sampleIndx];
   free(eCount);
   H5Aclose(attrID);

	/*
	 * Define the memory dataspace.
	 */
   	unsigned short chCount = params.size();
	dimsm[0] = chCount;
	dimsm[1] = nEvents;
	memspace = H5Screate_simple(2,dimsm,NULL);


	/*
	 * Define hyperslab in the dataset.
	 */
	hsize_t      count[3];              /* size of the hyperslab in the file */
	hsize_t      offset[3];             /* hyperslab offset in the file */
	hsize_t      count_out[2];          /* size of the hyperslab in memory */
	hsize_t      offset_out[2];         /* hyperslab offset in memory */

	unsigned int nSize=chCount*nEvents;
	double * data_out=new double[nSize];
	unsigned i;
	for(i = 0; i < chCount; i++){
		int colStart = i;//convert from R to C indexing
		offset[0] = sampleIndx;//start from sampleIndx-th sample
		offset[1] = colStart; //start from colStart-th channel
		offset[2] = 0; //start from the first event

		count[0]  = 1;//get one sample
		count[1]  = 1;//get one channel
		count[2]  = nEvents; //get all events


		status = H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, NULL,
											count, NULL);


		/*
		 * Define memory hyperslab.
		 */
		offset_out[0] = i;//start from ith column
		offset_out[1] = 0;//start from 0th event

		count_out[0]  = 1;//one channel
		count_out[1]  = nEvents; //all events
		status = H5Sselect_hyperslab(memspace, H5S_SELECT_SET, offset_out, NULL,
					 count_out, NULL);

		/*
		 * Read data from hyperslab in the file into the hyperslab in
		 * memory .
		 */
		status = H5Dread(dataset, H5T_NATIVE_DOUBLE, memspace, dataspace,
				 H5P_DEFAULT, data_out);

	}


	/*
	 * Close/release resources.
	 */

	H5Dclose(dataset);
	H5Sclose(dataspace);
	H5Sclose(memspace);
	H5Fclose(file);




	flowData res(data_out,params,(unsigned int)nEvents,sampleIndx);

	delete[] data_out;

	return res;

}

void ncdfFlow::writeflowData(flowData & fdata)
{

	if(fileName.empty())
		throw(ios_base::failure("ncdfFlow is not associated with any cdf file yet!\n"));

	int retval, ncid, varid;

	if ((retval = nc_open(fileName.c_str(), NC_WRITE,&ncid)))
			ERR(retval);

	unsigned nChannels=params.size();
	unsigned nSize=fdata.dataSize();
	unsigned nRow=nSize/nChannels;

	size_t start[] = {fdata.getSampleID(), 0, 0};
	size_t count[] = {1,nChannels, nRow};
	double *mat = new double[nSize];

	fdata.getData(mat,nSize);
//	for(unsigned i=0;i<nSize;i++)
//		mat[i]=fdata.data[i];

//	for(unsigned i=0;i<10;i++)
//		cout<<mat[i+6*nRow]<<",";
	if((retval = nc_inq_varid (ncid, "exprsMat", &varid)))
		ERR(retval);

	if((retval = nc_put_vara_double(ncid, varid, start, count, mat)))
		ERR(retval);

	delete[] mat;
	/*
	 * assume the new slice is the same size of original one, so no need to update event counts attribute
	 */

	if((retval = nc_close(ncid)))
		ERR(retval);

}
/*
 * to simply the installation and avoid dealing with both c++ and c library of netCDF
 * the IO using ncdfFlow package exposed API directly
 */
//flowData ncdfFlow::readflowData(unsigned int sampleID)
//{
//	if(fileName.empty())
//		throw(ios_base::failure("ncdfFlow is not associated with any cdf file yet!\n"));
//	cout<<"opening file:"<<fileName.c_str()<<endl;
//
//	int nChannel=params.size();
//	int _channel[2]={1,nChannel};
//	int _sampleID=sampleID+1;
//	double * mat=readSlice(fileName.c_str(), _channel,_sampleID);
//
//	/*
//	 * read slice
//	 */
//
//	/*
//	 * get counts
//	 */
//	int nEcount=readEventCounts(fileName.c_str());
//
//	/*
//	 * return a flowData object
//	 */
//	flowData res(mat,(unsigned int)nRow,(unsigned int)nChannels);
//	res.params_set(params);
//
//}
