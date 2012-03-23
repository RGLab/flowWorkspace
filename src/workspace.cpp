/*
 * workspace.cpp
 *
 *  Created on: Mar 22, 2012
 *      Author: wjiang2
 */




#include "workspace.hpp"
#include <iostream>

workspace::~workspace(){

	 /*free the document */
			xmlFreeDoc(doc);

			/*
			 *Free the global variables that may
			 *have been allocated by the parser.
			 */
			xmlCleanupParser();
			cout<<"xml freed!"<<endl;
}

/*Oftentimes we need to do the xquery based on the context of the current node instead of doc
* it is strange that I haven't found this commonly used API in libxml2
* so have to write my own here
*/
xmlXPathObjectPtr workspace::xpathInNode(string xpath,xmlNodePtr curNode)
{
	xmlXPathContextPtr ctxt=xmlXPathNewContext(this->doc);
	ctxt->node=curNode;
	xmlXPathObjectPtr res=xmlXPathEval((xmlChar *)xpath.c_str(),ctxt);
	xmlXPathFreeContext(ctxt);
	return res;
}
void workspace::print_element_names(xmlNode *a_node)
{



    xmlNode *cur_node = NULL;

    for (cur_node = a_node; cur_node; cur_node = cur_node->next) {
        if (cur_node->type == XML_ELEMENT_NODE) {
            printf("node type: Element, name: %s\n", cur_node->name);
        }

        print_element_names(cur_node->children);
    }
}
