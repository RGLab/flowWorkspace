/*
 * flowJoWorkspace.cpp
 *
 *  Created on: Mar 15, 2012
 *      Author: wjiang2
 */
#include "flowJoWorkspace.hpp"
#include <string>
#include <libxml/tree.h>
#include <libxml/parser.h>
#include <iostream>
using namespace std;

flowJoWorkspace::flowJoWorkspace(const char * sFileName){
	cout<<sFileName<<endl;
	this->doc=NULL;

}

flowJoWorkspace::~flowJoWorkspace(){

	 /*free the document */
			xmlFreeDoc(this->doc);

			/*
			 *Free the global variables that may
			 *have been allocated by the parser.
			 */
			xmlCleanupParser();
			cout<<"xml freed!"<<endl;
}




void flowJoWorkspace::openWorkspace(const char * sFileName)
{
//	 xmlDoc *doc = NULL;
//		xmlNode *root_element = NULL;

		/*
		 * this initialize the library and check potential ABI mismatches
		 * between the version it was compiled for and the actual shared
		 * library used.
		 */
		LIBXML_TEST_VERSION
	//	const char * sFileName="/home/wjiang2/rglab/workspace/HIPC-Lyoplate/data/HIPC_trial.xml";
		/*parse the file and get the DOM */
		this->doc = xmlReadFile(sFileName, NULL, 0);

		if (this->doc == NULL) {
			printf("error: could not parse file %s\n", sFileName);
		}
		/*Get the root element node */
//		 root_element = xmlDocGetRootElement(doc);

//		print_element_names(root_element);

}

void flowJoWorkspace::print_element_names(xmlNode *a_node)
{



    xmlNode *cur_node = NULL;

    for (cur_node = a_node; cur_node; cur_node = cur_node->next) {
        if (cur_node->type == XML_ELEMENT_NODE) {
            printf("node type: Element, name: %s\n", cur_node->name);
        }

        print_element_names(cur_node->children);
    }
}
