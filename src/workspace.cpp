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

//	cout<<"entring the destructor of workspace"<<endl;

	 /*free the document */
	xmlFreeDoc(doc);

	/*
	 *Free the global variables that may
	 *have been allocated by the parser.
	 */
	xmlCleanupParser();
	if(dMode>=GATING_SET_LEVEL)
		cout<<"xml freed!"<<endl;
}


