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

namespace flowWorkspace
{
/**
 * read xml file and create the appropriate flowJoWorkspace object
 * The reason to return a dynamically allocated pointer is solely for the sake of runtime polymorphism
 */
inline unique_ptr<flowJoWorkspace> openWorkspace(string xml_filename,SAMPLE_NAME_LOCATION sample_name_location,int xmlParserOption)
{

		LIBXML_TEST_VERSION

		/*parse the file and get the DOM */
		xmlDocPtr doc = xmlReadFile(xml_filename.c_str(), NULL, xmlParserOption);
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
		 unique_ptr<flowJoWorkspace> wsPtr;

		 WS_TYPE wsType = get_workspace_type(doc);
		 switch(wsType){
		 	 case WS_TYPE::WS_WIN:
		 		 wsPtr=unique_ptr<flowJoWorkspace>(new winFlowJoWorkspace(doc));
		 		 break;
		 	 case WS_TYPE::WS_MAC:
		 		 wsPtr=unique_ptr<flowJoWorkspace>(new macFlowJoWorkspace(doc));
		 		 break;
		 	 case WS_TYPE::WS_VX:
				 wsPtr=unique_ptr<flowJoWorkspace>(new xFlowJoWorkspace(doc));
				 break;
		 	case WS_TYPE::WS_MAC_3:
				 wsPtr=unique_ptr<flowJoWorkspace>(new macFlowJoWorkspace_3(doc));
				 break;
		 	 default:
		 		throw(invalid_argument("unsupported workspace Type!"));
		 }



		 wsPtr->nodePath.sample_name_location=sample_name_location;

		 if(g_loglevel>=GATING_SET_LEVEL)
			 COUT<<"internal gating set created from "<<xml_filename<<endl;

		 wsPtr->parseVersionList();
		 return wsPtr;
}
};
#endif /* INCLUDE_WS2GH_HPP_ */
