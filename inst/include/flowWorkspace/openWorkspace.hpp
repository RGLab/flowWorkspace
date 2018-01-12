/*
 * ws2gh.hpp
 *
 *  Created on: Aug 8, 2017
 *      Author: wjiang2
 */

#ifndef INCLUDE_WS2GH_HPP_
#define INCLUDE_WS2GH_HPP_

#include "macFlowJoWorkspace.hpp"
#include "winFlowJoWorkspace.hpp"
/**
 * read xml file and create the appropriate flowJoWorkspace object
 * The reason to return a dynamically allocated pointer is solely for the sake of runtime polymorphism
 */
inline workspace * openWorkspace(string sFileName,unsigned short sampNloc,int xmlParserOption,unsigned short wsType)
{
		workspace * wsPtr=NULL;
		LIBXML_TEST_VERSION

		/*parse the file and get the DOM */
		xmlDocPtr doc = xmlReadFile(sFileName.c_str(), NULL, xmlParserOption);
		if (doc == NULL ) {
//				fprintf(stderr,"Document not parsed successfully. \n");
				throw(ios_base::failure("Document not parsed successfully.Check if the path is valid."));
			}
		//validity check
		xmlNodePtr cur = xmlDocGetRootElement(doc);
		if (cur == NULL) {
//				fprintf(stderr,"empty document\n");
				throw(invalid_argument("empty document!"));
			}
		 if (!xmlStrEqual(cur->name, (const xmlChar *) "Workspace")) {
//				fprintf(stderr,"document of the wrong type, root node != Workspace");
				throw(invalid_argument("document of the wrong type, root node != 'Workspace'"));
			}


		 switch(wsType){
		 	 case WS_WIN:
		 		 wsPtr=new winFlowJoWorkspace(doc);
		 		 break;
		 	 case WS_MAC:
		 		 wsPtr=new macFlowJoWorkspace(doc);
		 		 break;
		 	 case WS_VX:
				 wsPtr=new xFlowJoWorkspace(doc);
				 break;
		 	case WS_MAC_3:
				 wsPtr=new macFlowJoWorkspace_3(doc);
				 break;
		 	 default:
		 		throw(invalid_argument("unsupported workspace Type!"));
		 }


		 wsPtr->nodePath.sampNloc=sampNloc;

//		 if(g_loglevel>=GATING_SET_LEVEL)
//			 COUT<<"internal gating set created from "<<sFileName<<endl;

		 wsPtr->parseVersionList();
		 return wsPtr;
}
#endif /* INCLUDE_WS2GH_HPP_ */
