/*
 * workspace.cpp
 *
 *  Created on: Mar 22, 2012
 *      Author: wjiang2
 */




#include "include/workspace.hpp"
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
		if(dMode>=GATING_SET_LEVEL)
			COUT<<"xml freed!"<<endl;
	}
}


valarray<double> workspace::toArray(string sCalTable){


//	sCalTable="3980,1.9428805e5,3981,1.9478264e5,3982,1.9527849e5,3983,1.9577559e5,3984,1.9627398e5";
	vector<string> stringVec;
	boost::split(stringVec,sCalTable,boost::is_any_of(","));
//

	valarray<double> res(stringVec.size());
	for(unsigned i=0;i<stringVec.size();i++)
	{
		res[i]=atof(stringVec.at(i).c_str());
//		COUT<<res[i]<<",";
	}
	return res;
}
