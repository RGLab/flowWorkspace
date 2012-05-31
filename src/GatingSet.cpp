/*
 * GatingSet.cpp
 *
 *  Created on: Mar 19, 2012
 *      Author: wjiang2
 */


#include "include/GatingSet.hpp"
#include <string>
#include <libxml/tree.h>
#include <libxml/parser.h>
#include <iostream>
#include <exception>
using namespace std;


template <class T>
wsSampleNode getSample(T ws,string sampleID){

		string xpath=ws->xPathSample(sampleID);

		wsNode docRoot(xmlDocGetRootElement(ws->doc));

		xmlXPathObjectPtr res=docRoot.xpathInNode(xpath);
		if(res->nodesetval->nodeNr>1)
		{
//			cout<<sampleID<<" is not unique within this group!"<<endl;
			xmlXPathFreeObject(res);
			throw(domain_error("non-unique sampleID within the group!"));
		}

		wsSampleNode sample(res->nodesetval->nodeTab[0]);
		xmlXPathFreeObject(res);
		return sample;
}



GatingSet::~GatingSet()
{
//	cout<<"entring the destructor of GatingSet"<<endl;
	if(ws!=NULL)
		delete ws;
//	typedef map<string,GatingHierarchy *> map_t;
	BOOST_FOREACH(gh_map::value_type & it,ghs){
			GatingHierarchy * ghPtr=it.second;
			string sampleName=it.first;
			delete ghPtr;
			if(dMode>=GATING_HIERARCHY_LEVEL)
				cout<<"GatingHierarchy freed:"<<sampleName<<endl;
	}

	for(trans_global_vec::iterator it=gTrans.begin();it!=gTrans.end();it++)
	{
		trans_map curTrans=it->trans;
		if(dMode>=GATING_SET_LEVEL)
			cout<<"free transformatioin group:"<<it->groupName<<endl;
		for(trans_map::iterator it1=curTrans.begin();it1!=curTrans.end();it1++)
		{
			transformation * curTran=it1->second;
			if(curTran!=NULL)
			{
				if(dMode>=GATING_SET_LEVEL)
						cout<<"free transformatioin:"<<curTran->channel<<endl;

				delete curTran;
			}

		}

	}

}
/*
 * TODO:current version of this contructor is based on gating template ,simply copying
 * compensation and transformation,more options can be allowed in future like providing different
 * comp and trans
 */
GatingSet::GatingSet(GatingHierarchy & gh_template,vector<string> sampleNames,unsigned short _dMode=1){

	dMode=_dMode;
	/*
	 * deep copy gtrans
	 */
//	for(trans_global_vec::iterator it=gh_template.gTrans->begin();it!=gh_template.gTrans->end();it++)
//	{
//
//		if(dMode>=GATING_SET_LEVEL)
//			cout<<"copying transformatioin group:"<<it->groupName<<endl;
//
//		trans_global newTransGroup=it->clone();
//
//		gTrans.push_back(newTransGroup);
//
//	}



	vector<string>::iterator it;
	for(it=sampleNames.begin();it!=sampleNames.end();it++)
	{
		string curSampleName=*it;
		if(dMode>=GATING_HIERARCHY_LEVEL)
			cout<<endl<<"... start cloning GatingHierarchy for: "<<curSampleName<<"... "<<endl;


		GatingHierarchy *curGh=gh_template.clone(false);

		ghs[curSampleName]=curGh;//add to the map

		if(dMode>=GATING_HIERARCHY_LEVEL)
			cout<<"Gating hierarchy cloned: "<<curSampleName<<endl;
	}
}
//read xml file and create the appropriate flowJoWorkspace object
GatingSet::GatingSet(string sFileName,bool isParseGate,unsigned short _dMode=1)
{

		LIBXML_TEST_VERSION

		/*parse the file and get the DOM */
		xmlDocPtr doc = xmlReadFile(sFileName.c_str(), NULL, 0);
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

		//get version info
		 xmlChar * wsVersion=xmlGetProp(cur,(const xmlChar*)"version");

		 if (xmlStrEqual(wsVersion,(const xmlChar *)"1.61")||xmlStrEqual(wsVersion,(const xmlChar *)"1.6"))
			 ws=new winFlowJoWorkspace(doc);
		 else if (xmlStrEqual(wsVersion,(const xmlChar *)"2.0"))
			 ws=new macFlowJoWorkspace(doc);
		 else
		 {
			 xmlFree(wsVersion);
			 throw(invalid_argument("We currently only support flowJo version 1.61 and 2.0"));
		 }
		 xmlFree(wsVersion);
		 ws->dMode=_dMode;
		 dMode=_dMode;
		 if(dMode>=GATING_SET_LEVEL)
			 cout<<"internal gating set created from "<<sFileName<<endl;
		 /*
		  * parsing global calibration tables
		  */
		 if(isParseGate)
		 {
			 if(dMode>=GATING_SET_LEVEL)
				 cout<<"... start parsing global transformations... "<<endl;
			 gTrans=ws->getGlobalTrans();
		 }

}

void GatingSet::parseWorkspace(unsigned short groupID,bool isParseGate)
{
	//first get all the sample IDs for given groupID
	vector<string> sampleID=ws->getSampleID(groupID);
	parseWorkspace(sampleID,isParseGate);

}
void GatingSet::parseWorkspace(vector<string> sampleIDs,bool isParseGate)
{

	//contruct gating hiearchy for each sampleID
	vector<string>::iterator it;
	for(it=sampleIDs.begin();it!=sampleIDs.end();it++)
	{
		if(dMode>=GATING_HIERARCHY_LEVEL)
			cout<<endl<<"... start parsing sample: "<<*it<<"... "<<endl;
		wsSampleNode curSampleNode=getSample(ws,*it);

		GatingHierarchy *curGh=new GatingHierarchy(curSampleNode,ws,isParseGate,&nc,dMode,&gTrans);

		string sampleName=ws->getSampleName(curSampleNode);

//		curGh->setSample(sampleName);
		ghs[sampleName]=curGh;//add to the map


//		sampleList.push_back(sampleName);
		if(dMode>=GATING_HIERARCHY_LEVEL)
			cout<<"Gating hierarchy created: "<<sampleName<<endl;
	}

}

GatingHierarchy * GatingSet::getGatingHierarchy(string sampleName)
{

	gh_map::iterator it=ghs.find(sampleName);
	if(it==ghs.end())
		throw(domain_error(sampleName+"not found in gating set!"));
	else
		return it->second;
}

GatingHierarchy * GatingSet::getGatingHierarchy(unsigned index)
{
		if(index>=ghs.size())
		throw(out_of_range("index out of range:"));

		return getGatingHierarchy(getSamples().at(index));
}
vector<string> GatingSet::getSamples(void)
{
		vector<string> res;

		BOOST_FOREACH(gh_map::value_type & it,ghs){
			res.push_back(it.first);
		}
		return res;
		//	return(this->sampleList);
};

void GatingSet::attachData(string fileName,vector<string> sampleNames,vector<string> params){

	nc.fileName_set(fileName);
	nc.params_set(params);
	nc.sample_set(sampleNames);
}

void GatingSet::gating(){

}
//void GatingSet::cloneGatingHierarchy(){
//
//}
