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


void save_gs(const GatingSet &gs,string filename, unsigned short format){
	    // make an archive
	    std::ofstream ofs(filename.c_str(),std::ios::out|std::ios::trunc|std::ios::binary);

	    switch(format)
	    {
	    case ARCHIVE_TYPE_BINARY:
	    	{
	    		boost::archive::binary_oarchive oa(ofs);
	    		oa << BOOST_SERIALIZATION_NVP(gs);
	    	}

	    	break;
	    case ARCHIVE_TYPE_TEXT:
			{
				boost::archive::text_oarchive oa1(ofs);
				oa1 << BOOST_SERIALIZATION_NVP(gs);
			}

	    	break;
	    case ARCHIVE_TYPE_XML:
	    	{
	    		boost::archive::xml_oarchive oa2(ofs);
				oa2 << BOOST_SERIALIZATION_NVP(gs);
	    	}

		    break;
		default:
			throw(invalid_argument("invalid archive format!only 0,1 or 2 are valid type."));
		    break;

	    }





	}
void restore_gs(GatingSet &s, string filename, unsigned short format)
{
    // open the archive
    std::ifstream ifs(filename.c_str());

    switch(format)
	{
	case ARCHIVE_TYPE_BINARY:
		{
			boost::archive::binary_iarchive ia(ifs);
			ia >> BOOST_SERIALIZATION_NVP(s);
		}

		break;
	case ARCHIVE_TYPE_TEXT:
		{
			boost::archive::text_iarchive ia1(ifs);
			ia1 >> BOOST_SERIALIZATION_NVP(s);
		}

		break;
	case ARCHIVE_TYPE_XML:
		{
			boost::archive::xml_iarchive ia2(ifs);
			ia2 >> BOOST_SERIALIZATION_NVP(s);
		}

		break;
	default:
		throw(invalid_argument("invalid archive format!only 0,1 or 2 are valid type."));
		break;

	}



}


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

/*
 * this should be called only in GatingSet destructor
 * because it is shared by every GatingHiearchy class
 * thus should not be deleted separately (otherwise it causes some segfault particulary on mac)
 */
void GatingSet::freeWorkspace(){
	if(ws!=NULL)
	{
		delete ws;
		ws = NULL;
	}

}
GatingSet::~GatingSet()
{
	if(dMode>=GATING_SET_LEVEL)
		cout<<endl<<"start to free GatingSet..."<<endl;

	freeWorkspace();

	BOOST_FOREACH(gh_map::value_type & it,ghs){
		GatingHierarchy * ghPtr=it.second;
		string sampleName=it.first;
		if(dMode>=GATING_HIERARCHY_LEVEL)
			cout<<endl<<"start to free GatingHierarchy:"<<sampleName<<endl;

		delete ghPtr;

	}

	for(trans_global_vec::iterator it=gTrans.begin();it!=gTrans.end();it++)
	{
		trans_map curTrans=it->getTransMap();
		if(dMode>=GATING_SET_LEVEL)
			cout<<endl<<"start to free transformatioin group:"<<it->getGroupName()<<endl;
		for(trans_map::iterator it1=curTrans.begin();it1!=curTrans.end();it1++)
		{
			transformation * curTran=it1->second;
			if(curTran!=NULL)
			{
				if(dMode>=GATING_SET_LEVEL)
						cout<<"free transformatioin:"<<curTran->getChannel()<<endl;

				delete curTran;
				curTran = NULL;
			}

		}

	}

}

/*
 * up to caller to free the memory
 */
/*
 * non-serialization version,should be faster
 * but no transformation object gets copied
 */
GatingSet* GatingSet::clone_treeOnly(vector<string> samples){



	GatingSet * newGS=new GatingSet();

	newGS->add(*this,samples);

	return newGS;
}
/*
 * memory buffer version
 */
//GatingSet* GatingSet::clone_sstream(vector<string> samples){
//
//
//	stringstream ss (stringstream::in | stringstream::out | stringstream::binary);
//
//	{
//		boost::archive::binary_oarchive oa(ss);
//		oa << *this;
//	}
//
//
//	GatingSet * newGS=new GatingSet();
//
//	{
//		boost::archive::binary_iarchive ia(ss);
//		ia >> *newGS;
//	}
//
//
//
//	//remove unused samples
//	BOOST_FOREACH(gh_map::value_type & it,newGS->ghs){
//			GatingHierarchy * ghPtr=it.second;
//			string sampleName=it.first;
//			/*
//			 * if the sampleName not in the clone sample list ,remove it from the tree map
//			 */
//			vector<string>::iterator fit=find(samples.begin(),samples.end(),sampleName);
//			if(fit==samples.end())
//			{
//				delete ghPtr;
//				newGS->ghs.erase(sampleName);
//			}
//
//
//		}
//
//	return newGS;
//}
///*
// * disk version because stringstream has size limit
// */
//GatingSet* GatingSet::clone_fstream(vector<string> samples){
//
//
//	save_gs(*this,"tmp");
//
//	GatingSet * newGS=new GatingSet();
//
//	restore_gs(*newGS,"tmp");
//
//	//remove unused samples
//	BOOST_FOREACH(gh_map::value_type & it,newGS->ghs){
//			GatingHierarchy * ghPtr=it.second;
//			string sampleName=it.first;
//			/*
//			 * if the sampleName not in the clone sample list ,remove it from the tree map
//			 */
//			vector<string>::iterator fit=find(samples.begin(),samples.end(),sampleName);
//			if(fit==samples.end())
//			{
//				delete ghPtr;
//				newGS->ghs.erase(sampleName);
//			}
//
//
//		}
//
//	return newGS;
//}
//void GatingSet::add(gate * g,string parentName,string nodeName,unsigned short _dMode){
//
//	BOOST_FOREACH(gh_map::value_type & it,ghs){
//
//				GatingHierarchy * gh=it.second;
//				gh->addGate(g,parentName,nodeName);
//			}
//}
/*
 *TODO: trans is not copied for now since it involves copying the global trans vector
 *and rematch them to each individual hierarchy
 */
void GatingSet::add(GatingSet & gs,vector<string> sampleNames,unsigned short _dMode){

	/*
	 * ws is not needed here since all the info comes from gh_template instead of ws
	 * but we need to initialize it to NULL to avoid the illegal deletion of the Nil pointer in the gatingset destructor
	 */
//	ws=NULL;

	dMode=_dMode;
	/*
	 * copy trans from gh_template into gtrans
	 * involve deep copying of transformation pointers
	 */
//	if(dMode>=GATING_SET_LEVEL)
//		cout<<"copying transformation from gh_template..."<<endl;
//	trans_global newTransGroup;

//	trans_map newTmap=gh_template->getLocalTrans().cloneTransMap();
//	newTransGroup.setTransMap(newTmap);
//	gTrans.push_back(newTransGroup);

	/*
	 * use newTmap for all other new ghs
	 */
	vector<string>::iterator it;
	for(it=sampleNames.begin();it!=sampleNames.end();it++)
	{
		string curSampleName=*it;
		if(dMode>=GATING_HIERARCHY_LEVEL)
			cout<<endl<<"... copying GatingHierarchy: "<<curSampleName<<"... "<<endl;


		GatingHierarchy *toCopy=gs.getGatingHierarchy(curSampleName);

		GatingHierarchy * curGh=toCopy->clone();

//		curGh->setNcPtr(NULL);
		curGh->dMode=_dMode;

		ghs[curSampleName]=curGh;//add to the map

	}
}
/*
 * TODO:current version of this contructor is based on gating template ,simply copying
 * compensation and transformation,more options can be allowed in future like providing different
 * comp and trans
 */
GatingSet::GatingSet(GatingHierarchy * gh_template,vector<string> sampleNames,unsigned short _dMode){

	/*
	 * ws is not needed here since all the info comes from gh_template instead of ws
	 * but we need to initialize it to NULL to avoid the illegal deletion of the Nil pointer in the gatingset destructor
	 */
	ws=NULL;

	dMode=_dMode;
	/*
	 * copy trans from gh_template into gtrans
	 * involve deep copying of transformation pointers
	 */
	if(dMode>=GATING_SET_LEVEL)
		cout<<"copying transformation from gh_template..."<<endl;
	trans_global newTransGroup;
//	gh_template->printLocalTrans();
	trans_map newTmap=gh_template->getLocalTrans().cloneTransMap();
	newTransGroup.setTransMap(newTmap);
	gTrans.push_back(newTransGroup);

	/*
	 * use newTmap for all other new ghs
	 */
	vector<string>::iterator it;
	for(it=sampleNames.begin();it!=sampleNames.end();it++)
	{
		string curSampleName=*it;
		if(dMode>=GATING_HIERARCHY_LEVEL)
			cout<<endl<<"... start cloning GatingHierarchy for: "<<curSampleName<<"... "<<endl;


		GatingHierarchy *curGh=gh_template->clone(newTmap,&gTrans);
//		curGh->setNcPtr(&nc);//make sure to assign the global nc pointer to  GatingHierarchy
		curGh->dMode=_dMode;

		ghs[curSampleName]=curGh;//add to the map

		if(dMode>=GATING_HIERARCHY_LEVEL)
			cout<<"Gating hierarchy cloned: "<<curSampleName<<endl;
	}
}

GatingSet::GatingSet(vector<string> sampleNames,unsigned short _dMode){


	ws=NULL;
	dMode=_dMode;

	vector<string>::iterator it;
	for(it=sampleNames.begin();it!=sampleNames.end();it++)
	{
		string curSampleName=*it;
		if(dMode>=GATING_HIERARCHY_LEVEL)
			cout<<endl<<"... start adding GatingHierarchy for: "<<curSampleName<<"... "<<endl;


		GatingHierarchy *curGh=new GatingHierarchy();
//		curGh->setNcPtr(NULL);
		curGh->dMode=_dMode;

		curGh->addRoot();//add default root


		ghs[curSampleName]=curGh;//add to the map

	}
}


//read xml file and create the appropriate flowJoWorkspace object
GatingSet::GatingSet(string sFileName,bool isParseGate,unsigned short sampNloc,int xmlParserOption,unsigned short _dMode)
{
		ws=NULL;
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

		//get version info
		 xmlChar * wsVersion=xmlGetProp(cur,(const xmlChar*)"version");

		 if (xmlStrEqual(wsVersion,(const xmlChar *)"1.61")||xmlStrEqual(wsVersion,(const xmlChar *)"1.6"))
			 ws=new winFlowJoWorkspace(doc);
		 else if (xmlStrEqual(wsVersion,(const xmlChar *)"2.0"))
			 ws=new macFlowJoWorkspace(doc);
		 else if (xmlStrEqual(wsVersion,(const xmlChar *)"1.8"))
			 ws=new xFlowJoWorkspace(doc);
		 else
		 {
			 xmlFree(wsVersion);
			 throw(invalid_argument("We currently only support flowJo version 1.61 and 2.0"));
		 }
		 xmlFree(wsVersion);
		 ws->dMode=_dMode;
		 ws->nodePath.sampNloc=sampNloc;
		 dMode=_dMode;
		 if(dMode>=GATING_SET_LEVEL)
			 cout<<"internal gating set created from "<<sFileName<<endl;

		 ws->parseVersionList();
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

		GatingHierarchy *curGh=new GatingHierarchy(curSampleNode,ws,isParseGate,&gTrans,&globalBiExpTrans,&globalLinTrans,dMode);

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
/* change the sample name by inserting a new entry and deleting the old one*/
void GatingSet::setSample(string oldName, string newName)
{

		GatingHierarchy * gh = getGatingHierarchy(oldName);

		ghs[newName] = gh;
		ghs.erase(oldName);

};


void GatingSet::gating(){

}
//void GatingSet::cloneGatingHierarchy(){
//
//}
