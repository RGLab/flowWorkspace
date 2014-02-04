/*
 * ncdf_test.cpp
 *
 *  Created on: May 15, 2012
 *      Author: wjiang2
 */


#include "test_header.hpp"
void ncdf_test(){
	string ncFile="../output/HIPC_trial/nc1.nc";

	//read colnames from text
//	vector<string> params;
//	vector<string> sampleNames;
//	sampleNames.push_back("004_A1_A01.fcs");
//	sampleNames.push_back("004_B1_B01.fcs");
//
//	std::ifstream myfile;
//	myfile.open("../output/HIPC_trial/colnames.txt",ifstream::in);
////		myfile.open("../output/Yale/colnames.txt",ifstream::in);
//	vector<string> myLines;
//	string line;
//	while (std::getline(myfile, line))
//	{
//		params.push_back(line);
//	}
//
//	myfile.close();
	ncdfFlow nc(ncFile);
//	nc.params_set(params);
//	nc.sample_set(sampleNames);
	for(unsigned i=0;i<100;i++)
		nc.readflowData(0);
	cout<<"done!"<<endl;


}
