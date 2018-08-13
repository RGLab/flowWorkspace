/*
 * wsNode.hpp
 *
 *  Created on: Mar 22, 2012
 *      Author: wjiang2
 */

#ifndef WSNODE_HPP_
#define WSNODE_HPP_
#include <libxml/tree.h>
#include <libxml/xpath.h>
#include <libxml/parser.h>
#include "cytolib/nodeProperties.hpp"
using namespace std;

#ifdef ROUT
#include <RcppArmadillo.h>
#define COUT Rcpp::Rcout //flowWorkspace is still using Rcpp, so we don't bother replace COUT with PRINT yet
#endif


#ifndef ROUT
#define COUT cout
#endif


namespace flowWorkspace
{
class wsNode{

	xmlNodePtr thisNode;
public:
	/**
	 * validity check for the xquery result to prevent the illegal access to the NULL pointer
	 * @param ptr
	 * @param path
	 */
	void check_xmlXPathObjectPtr(const xmlXPathObjectPtr ptr, const string & xpath) const{
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

	xmlXPathObjectPtr xpathInNode(const string & xpath) const
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
	xmlXPathObjectPtr xpath(const string & xpath, bool is_validity_check = true)
	{
		xmlXPathContextPtr ctxt=xmlXPathNewContext(thisNode->doc);
		xmlXPathObjectPtr res=xmlXPathEval((xmlChar *)xpath.c_str(),ctxt);
		xmlXPathFreeContext(ctxt);
		if(is_validity_check)
			check_xmlXPathObjectPtr(res, xpath);
		return res;
	}

	string getNsProperty(string propName,string ns){

		xmlChar * name= xmlGetNsProp(this->thisNode,(xmlChar *)propName.c_str(),(xmlChar *)ns.c_str());
		string sName;
		if(name!=0)
			sName=(const char*)name;
		xmlFree(name);
		return sName;

	}

	string getProperty(string propName) const{

		xmlChar * name= xmlGetProp(this->thisNode,(xmlChar *)propName.c_str());
		string sName;
		if(name!=0)
			sName=(const char*)name;
		xmlFree(name);
		return sName;

	}

	string getContent(){

		xmlChar * content = xmlNodeListGetString(thisNode->doc, thisNode->xmlChildrenNode, 1);
		string res;
		if(content!=0)
			res=(const char*)content;
		xmlFree(content);

		return res;

	}

	string getName(){
		string res =(const char*)this->thisNode->name;
		return res;
	}

	xmlNodePtr getNodePtr(){return thisNode;};
	void setNodePtr(xmlNodePtr n){thisNode=n;};
	wsNode(xmlNodePtr node){thisNode=node;};
	wsNode(){};
};

class wsGroupNode:public wsNode{
public:
	wsGroupNode(){};
	wsGroupNode(xmlNodePtr node){setNodePtr(node);};


};


class wsSampleNode:public wsNode{
public:
	wsSampleNode(){};
	wsSampleNode(xmlNodePtr node){setNodePtr(node);};


};

class wsRootNode:public wsNode{
public:
	wsRootNode(xmlNodePtr node){setNodePtr(node);};
};

class wsPopNode:public wsNode{
public:
	wsPopNode(xmlNodePtr node){setNodePtr(node);};
//	wsPopNode(){};
};

class wsRectGateNode:public wsNode{
public:
	wsRectGateNode(xmlNodePtr node){setNodePtr(node);};
//	wsPopNode(){};
};

class wsPolyGateNode:public wsNode{
public:
	wsPolyGateNode(xmlNodePtr node){setNodePtr(node);};

};

class wsEllipseGateNode:public wsNode{
public:
	wsEllipseGateNode(xmlNodePtr node){setNodePtr(node);};
};

class wsRangeGateNode:public wsNode{
public:
	wsRangeGateNode(xmlNodePtr node){setNodePtr(node);};
};

class wsBooleanGateNode:public wsNode{
public:
	wsBooleanGateNode(xmlNodePtr node){setNodePtr(node);};

};
class wsCurlyQuadGateNode:public wsNode{
public:
	wsCurlyQuadGateNode(xmlNodePtr node){setNodePtr(node);};

};

typedef vector<wsPopNode> wsPopNodeSet;

};
#endif /* WSNODE_HPP_ */

