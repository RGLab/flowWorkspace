/*
 * workspace.cpp
 *
 *  Created on: Mar 22, 2012
 *      Author: wjiang2
 */




#include "include/workspace.hpp"
#include <iostream>

workspace::~workspace(){

	 /*free the document */
			xmlFreeDoc(doc);

			/*
			 *Free the global variables that may
			 *have been allocated by the parser.
			 */
			xmlCleanupParser();
			if(dMode>=1)
				cout<<"xml freed!"<<endl;
}


//void workspace::print_element_names(xmlNode *a_node)
//{
//
//
//
//    xmlNode *cur_node = NULL;
//
//    for (cur_node = a_node; cur_node; cur_node = cur_node->next) {
//        if (cur_node->type == XML_ELEMENT_NODE) {
//            printf("node type: Element, name: %s\n", cur_node->name);
//        }
//
//        print_element_names(cur_node->children);
//    }
//}
