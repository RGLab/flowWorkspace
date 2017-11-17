/*
 * workspace.cpp
 *
 *  Created on: Mar 22, 2012
 *      Author: wjiang2
 */




#include "flowWorkspace/workspace.hpp"
#include <iostream>
#include <sstream>
workspace::~workspace(){

//	COUT<<"entring the destructor of workspace"<<endl;

	 /*free the document */
	if(doc!=NULL)
	{
		xmlFreeDoc(doc);
		doc = NULL;

		/*
		 *Free the global variables that may
		 *have been allocated by the parser.
		 */
		xmlCleanupParser();
		if(g_loglevel>=GATING_SET_LEVEL)
			COUT<<"xml freed!"<<endl;
	}
}

void workspace::toArray(string sCalTable, vector<double> &x, vector<double> &y){


//	sCalTable="3980,1.9428805e5,3981,1.9478264e5,3982,1.9527849e5,3983,1.9577559e5,3984,1.9627398e5";
	vector<string> stringVec;
	boost::split(stringVec,sCalTable,boost::is_any_of(","));
	int nLen = stringVec.size()/2;
	x.resize(nLen);
	y.resize(nLen);
	for(unsigned i=0;i<nLen;i++)
	{
		y[i]=atof(stringVec.at(2*i).c_str());
		x[i]=atof(stringVec.at(2*i + 1).c_str());
//		COUT<<res[i]<<",";
	}

}
