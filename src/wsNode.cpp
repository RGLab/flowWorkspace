/*
 * wsNode.cpp
 *
 *  Created on: Mar 27, 2012
 *      Author: wjiang2
 */



#include "include/wsNode.hpp"


/**
 * validity check for the xquery result to prevent the illegal access to the NULL pointer
 * @param ptr
 * @param path
 */
void check_xmlXPathObjectPtr(const xmlXPathObjectPtr ptr, const string & xpath){
	string err = xpath + " not found!";
	if(ptr==NULL)
		throw(domain_error(err));
	else
		if(ptr->nodesetval ==  NULL)
			throw(domain_error(err));
}


/*Oftentimes we need to do the xquery based on the context of the current node instead of doc
* it is strange that I haven't found this commonly used API in libxml2
* so have to write my own here
*
* Note: need to be cautious that  the result from xmlXPathEval call is not freed within  this API
* it is up to user to call xmlXPathFreeObject to free it
*/

xmlXPathObjectPtr wsNode::xpathInNode(string xpath)
{
	xmlXPathContextPtr ctxt=xmlXPathNewContext(thisNode->doc);
	ctxt->node=thisNode;
	xmlXPathObjectPtr res=xmlXPathEval((xmlChar *)xpath.c_str(),ctxt);
	xmlXPathFreeContext(ctxt);
	check_xmlXPathObjectPtr(res, xpath);
	return res;
}
/*
 * query from root
 * Note: need to be cautious that  the result from xmlXPathEval call is not freed within  this API
 * it is up to user to call xmlXPathFreeObject to free it
 */
xmlXPathObjectPtr wsNode::xpath(string xpath)
{
	xmlXPathContextPtr ctxt=xmlXPathNewContext(thisNode->doc);
	xmlXPathObjectPtr res=xmlXPathEval((xmlChar *)xpath.c_str(),ctxt);
	xmlXPathFreeContext(ctxt);
	check_xmlXPathObjectPtr(res, xpath);
	return res;
}

string wsNode::getNsProperty(string propName,string ns){

	xmlChar * name= xmlGetNsProp(this->thisNode,(xmlChar *)propName.c_str(),(xmlChar *)ns.c_str());
	string sName;
	if(name!=0)
		sName=(const char*)name;
	xmlFree(name);
	return sName;

}

string wsNode::getProperty(string propName){

	xmlChar * name= xmlGetProp(this->thisNode,(xmlChar *)propName.c_str());
	string sName;
	if(name!=0)
		sName=(const char*)name;
	xmlFree(name);
	return sName;

}

string wsNode::getContent(){

	xmlChar * content = xmlNodeListGetString(thisNode->doc, thisNode->xmlChildrenNode, 1);
	string res;
	if(content!=0)
		res=(const char*)content;
	xmlFree(content);

	return res;

}

string wsNode::getName(){
	string res =(const char*)this->thisNode->name;
	return res;
}
