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
#include "include/delimitedMessage.hpp"
using namespace std;


/**
 * serialization by boost serialization library
 * @param filename
 * @param format archive format, can be text,xml or binary
 */
void GatingSet::serialize_bs(string filename, unsigned short format){


	    	// make an archive
			std::ios::openmode mode = std::ios::out|std::ios::trunc;
			if(format == ARCHIVE_TYPE_BINARY)
				mode = mode | std::ios::binary;



			std::ofstream ofs(filename.c_str(), mode);


			switch(format)
			{
			case ARCHIVE_TYPE_BINARY:
				{
					boost::archive::binary_oarchive oa(ofs);
					oa << BOOST_SERIALIZATION_NVP(*this);
				}

				break;
			case ARCHIVE_TYPE_TEXT:
				{
					boost::archive::text_oarchive oa1(ofs);
					oa1 << BOOST_SERIALIZATION_NVP(*this);
				}

				break;
			case ARCHIVE_TYPE_XML:
				{
					boost::archive::xml_oarchive oa2(ofs);
					oa2 << BOOST_SERIALIZATION_NVP(*this);
				}

				break;
			default:
				throw(invalid_argument("invalid archive format!only 0,1 or 2 are valid type."));
				break;

			}


	}
/**
 * separate filename from dir to avoid to deal with path parsing in c++
 * @param path the dir of filename
 * @param filename
 */
void GatingSet::serialize_pb(string filename){
		// Verify that the version of the library that we linked against is
		// compatible with the version of the headers we compiled against.
		GOOGLE_PROTOBUF_VERIFY_VERSION;
		//init the output stream
		ofstream output(filename.c_str(), ios::out | ios::trunc | ios::binary);
		google::protobuf::io::OstreamOutputStream raw_output(&output);

		//empty message for gs
		pb::GatingSet gs_pb;

		/*
		 * add messages for trans
		 */

		//save the address of global biexp (as 1st entry)
		pb::TRANS_TBL * trans_tbl_pb = gs_pb.add_trans_tbl();
		intptr_t address = (intptr_t)&globalBiExpTrans;
		trans_tbl_pb->set_trans_address(address);


		//save the address of global lintrans(as 2nd entry)
		trans_tbl_pb = gs_pb.add_trans_tbl();
		address = (intptr_t)&globalLinTrans;
		trans_tbl_pb->set_trans_address(address);


		// cp trans group
		BOOST_FOREACH(trans_global_vec::value_type & it, gTrans){
			pb::trans_local * tg = gs_pb.add_gtrans();
			it.convertToPb(*tg, gs_pb);
		}

		//add sample name
		BOOST_FOREACH(gh_map::value_type & it,ghs){
				string sn = it.first;
				gs_pb.add_samplename(sn);
		}

		//write gs message to stream
		bool success = writeDelimitedTo(gs_pb, raw_output);

		if (!success){
			google::protobuf::ShutdownProtobufLibrary();
			throw(domain_error("Failed to write GatingSet."));
		}else
		{
			/*
			 * write pb message for each sample
			 */

			BOOST_FOREACH(gh_map::value_type & it,ghs){
					string sn = it.first;
					GatingHierarchy * gh =  it.second;

					pb::GatingHierarchy pb_gh;
					gh->convertToPb(pb_gh);
					//write the message
					bool success = writeDelimitedTo(pb_gh, raw_output);
					if (!success)
						throw(domain_error("Failed to write GatingHierarchy."));
			}

		}

		// Optional:  Delete all global objects allocated by libprotobuf.
		google::protobuf::ShutdownProtobufLibrary();
}
/**
 * constructor from the archives (de-serialization)
 * @param filename
 * @param format
 * @param isPB
 */
GatingSet::GatingSet(string filename, unsigned short format, bool isPB):wsPtr(NULL)
{


	if(isPB){
		GOOGLE_PROTOBUF_VERIFY_VERSION;
		ifstream input(filename.c_str(), ios::in | ios::binary);
		if (!input) {
			throw(invalid_argument("File not found.." ));
		} else{
			 pb::GatingSet pbGS;

			 google::protobuf::io::IstreamInputStream raw_input(&input);
			 //read gs message
			 bool success = readDelimitedFrom(raw_input, pbGS);

			if (!success) {
				throw(domain_error("Failed to parse GatingSet."));
			}

			//parse global trans tbl from message
			map<intptr_t, transformation *> trans_tbl;

			for(int i = 0; i < pbGS.trans_tbl_size(); i++){
				const pb::TRANS_TBL & trans_tbl_pb = pbGS.trans_tbl(i);
				const pb::transformation & trans_pb = trans_tbl_pb.trans();
				intptr_t old_address = (intptr_t)trans_tbl_pb.trans_address();

				/*
				 * first two global trans do not need to be restored from archive
				 * since they use the default parameters
				 * simply add the new address
				 */

				switch(i)
				{
				case 0:
					trans_tbl[old_address] = &globalBiExpTrans;
					break;
				case 1:
					trans_tbl[old_address] = &globalLinTrans;
					break;
				default:
					{
						switch(trans_pb.trans_type())
						{
						case pb::PB_CALTBL:
							trans_tbl[old_address] = new transformation(trans_pb);
							break;
						case pb::PB_BIEXP:
							trans_tbl[old_address] = new biexpTrans(trans_pb);
							break;
						case pb::PB_FASIGNH:
							trans_tbl[old_address] = new fasinhTrans(trans_pb);
							break;
						case pb::PB_FLIN:
							trans_tbl[old_address] = new flinTrans(trans_pb);
							break;
						case pb::PB_LIN:
							trans_tbl[old_address] = new linTrans(trans_pb);
							break;
						case pb::PB_LOG:
							trans_tbl[old_address] = new logTrans(trans_pb);
							break;
	//					case pb::PB_SCALE:
	//						trans_tbl[old_address] = new scaleTrans(trans_pb);
	//						break;
						default:
							throw(domain_error("unknown type of transformation archive!"));
						}
					}
				}

			}
			/*
			 * recover the trans_global
			 */

			for(int i = 0; i < pbGS.gtrans_size(); i++){
				const pb::trans_local & trans_local_pb = pbGS.gtrans(i);
				gTrans.push_back(trans_global(trans_local_pb, trans_tbl));
			}

			//read gating hierarchy messages
			for(int i = 0; i < pbGS.samplename_size(); i++){
				string sn = pbGS.samplename(i);
				//gh message is stored as the same order as sample name vector in gs
				pb::GatingHierarchy gh_pb;
				bool success = readDelimitedFrom(raw_input, gh_pb);

				if (!success) {
					throw(domain_error("Failed to parse GatingHierarchy."));
				}


				ghs[sn] = new GatingHierarchy(gh_pb, trans_tbl);
			}
		}
	}
	else
	{
		// open the archive
		std::ios::openmode mode = std::ios::in;
		if(format == ARCHIVE_TYPE_BINARY)
			mode = mode | std::ios::binary;
		std::ifstream ifs(filename.c_str(), mode);

		switch(format)
		{
		case ARCHIVE_TYPE_BINARY:
			{
				boost::archive::binary_iarchive ia(ifs);
				ia >> BOOST_SERIALIZATION_NVP(*this);
			}

			break;
		case ARCHIVE_TYPE_TEXT:
			{
				boost::archive::text_iarchive ia1(ifs);
				ia1 >> BOOST_SERIALIZATION_NVP(*this);
			}

			break;
		case ARCHIVE_TYPE_XML:
			{
				boost::archive::xml_iarchive ia2(ifs);
				ia2 >> BOOST_SERIALIZATION_NVP(*this);
			}

			break;
		default:
			throw(invalid_argument("invalid archive format!only 0,1 or 2 are valid type."));
			break;

		}
	}


}


template <class T>
wsSampleNode getSample(T & ws,string sampleID){

		string xpath=ws.xPathSample(sampleID);

		wsNode docRoot(xmlDocGetRootElement(ws.doc));

		xmlXPathObjectPtr res=docRoot.xpathInNode(xpath);
		if(res->nodesetval->nodeNr>1)
		{
//			COUT<<sampleID<<" is not unique within this group!"<<endl;
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
	if(wsPtr!=NULL)
	{
		delete wsPtr;
		wsPtr = NULL;
	}

}
GatingSet::~GatingSet()
{
	if(g_loglevel>=GATING_SET_LEVEL)
		COUT<<endl<<"start to free GatingSet..."<<endl;

	freeWorkspace();

	BOOST_FOREACH(gh_map::value_type & it,ghs){
		GatingHierarchy * ghPtr=it.second;
		string sampleName=it.first;
		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
			COUT<<endl<<"start to free GatingHierarchy:"<<sampleName<<endl;

		delete ghPtr;

	}

	for(trans_global_vec::iterator it=gTrans.begin();it!=gTrans.end();it++)
	{
		trans_map curTrans=it->getTransMap();
		if(g_loglevel>=GATING_SET_LEVEL)
			COUT<<endl<<"start to free transformatioin group:"<<it->getGroupName()<<endl;
		for(trans_map::iterator it1=curTrans.begin();it1!=curTrans.end();it1++)
		{
			transformation * curTran=it1->second;
			if(curTran!=NULL)
			{
				if(g_loglevel>=GATING_SET_LEVEL)
						COUT<<"free transformatioin:"<<curTran->getChannel()<<endl;

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
//void GatingSet::add(gate * g,string parentName,string nodeName){
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
void GatingSet::add(GatingSet & gs,vector<string> sampleNames){

	/*
	 * wsPtr is not needed here since all the info comes from gh_template instead of wsPtr
	 * but we need to initialize it to NULL to avoid the illegal deletion of the Nil pointer in the gatingset destructor
	 */
//	wsPtr=NULL;


	/*
	 * copy trans from gh_template into gtrans
	 * involve deep copying of transformation pointers
	 */
//	if(g_loglevel>=GATING_SET_LEVEL)
//		COUT<<"copying transformation from gh_template..."<<endl;
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
		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
			COUT<<endl<<"... copying GatingHierarchy: "<<curSampleName<<"... "<<endl;


		GatingHierarchy *toCopy=gs.getGatingHierarchy(curSampleName);

		GatingHierarchy * curGh=toCopy->clone();

//		curGh->setNcPtr(NULL);


		ghs[curSampleName]=curGh;//add to the map

	}
}
/*
 * TODO:current version of this contructor is based on gating template ,simply copying
 * compensation and transformation,more options can be allowed in future like providing different
 * comp and trans
 */
GatingSet::GatingSet(GatingHierarchy * gh_template,vector<string> sampleNames){

	/*
	 * wsPtr is not needed here since all the info comes from gh_template instead of ws
	 * but we need to initialize it to NULL to avoid the illegal deletion of the Nil pointer in the gatingset destructor
	 */
	wsPtr=NULL;


	/*
	 * copy trans from gh_template into gtrans
	 * involve deep copying of transformation pointers
	 */
	if(g_loglevel>=GATING_SET_LEVEL)
		COUT<<"copying transformation from gh_template..."<<endl;
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
		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
			COUT<<endl<<"... start cloning GatingHierarchy for: "<<curSampleName<<"... "<<endl;


		GatingHierarchy *curGh=gh_template->clone(newTmap,&gTrans);
//		curGh->setNcPtr(&nc);//make sure to assign the global nc pointer to  GatingHierarchy


		ghs[curSampleName]=curGh;//add to the map

		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
			COUT<<"Gating hierarchy cloned: "<<curSampleName<<endl;
	}
}

GatingSet::GatingSet(vector<string> sampleNames){


	wsPtr=NULL;


	vector<string>::iterator it;
	for(it=sampleNames.begin();it!=sampleNames.end();it++)
	{
		string curSampleName=*it;
		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
			COUT<<endl<<"... start adding GatingHierarchy for: "<<curSampleName<<"... "<<endl;


		GatingHierarchy *curGh=new GatingHierarchy();
//		curGh->setNcPtr(NULL);


		curGh->addRoot();//add default root


		ghs[curSampleName]=curGh;//add to the map

	}
}


//read xml file and create the appropriate flowJoWorkspace object
GatingSet::GatingSet(string sFileName,bool isParseGate,unsigned short sampNloc,int xmlParserOption,unsigned short wsType)
{
		wsPtr=NULL;
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

		 if(g_loglevel>=GATING_SET_LEVEL)
			 COUT<<"internal gating set created from "<<sFileName<<endl;

		 wsPtr->parseVersionList();
		 /*
		  * parsing global calibration tables
		  */

		 if(isParseGate)
		 {
			 if(g_loglevel>=GATING_SET_LEVEL)
				 COUT<<"... start parsing global transformations... "<<endl;
			 gTrans=wsPtr->getGlobalTrans();
		 }
}

/**
 * doesn't seem to be used anymore
 * @param groupID
 * @param isParseGate
 */
void GatingSet::parseWorkspace(unsigned short groupID,bool isParseGate, StringVec sampleNames)
{
	//first get all the sample IDs for given groupID
	vector<string> sampleID=wsPtr->getSampleID(groupID);
	parseWorkspace(sampleID,isParseGate, sampleNames);

}
void GatingSet::parseWorkspace(vector<string> sampleIDs,bool isParseGate, StringVec sampleNames)
{
	unsigned nSample = sampleNames.size();
	if(nSample!=sampleIDs.size())
		throw(domain_error("Sizes of sampleIDs and sampleNames are not equal!"));
	//contruct gating hiearchy for each sampleID
	for(unsigned i = 0; i < nSample; i++)
	{
		string sampleID = sampleIDs.at(i);
		string sampleName = sampleNames.at(i);
		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
			COUT<<endl<<"... start parsing sample: "<< sampleID <<"... "<<endl;
		wsSampleNode curSampleNode=getSample(*wsPtr, sampleID);

		GatingHierarchy *curGh=new GatingHierarchy(curSampleNode,*wsPtr,isParseGate,&gTrans,&globalBiExpTrans,&globalLinTrans);

//		string sampleName=wsPtr->getSampleName(curSampleNode);

		ghs[sampleName]=curGh;//add to the map


//		sampleList.push_back(sampleName);
		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
			COUT<<"Gating hierarchy created: "<<sampleName<<endl;
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
}

/**
 *  change the sample name by inserting a new entry and deleting the old one
 *  do nothing when the newName is the same as the old (because the current logic will lose
 *  it by erase it)
 *  */
void GatingSet::setSample(string oldName, string newName)
{
		if(oldName.compare(newName) != 0){
			GatingHierarchy * gh = getGatingHierarchy(oldName);
			ghs[newName] = gh;
			ghs.erase(oldName);
		}


}


void GatingSet::gating(){

}
//void GatingSet::cloneGatingHierarchy(){
//
//}

/**
 * It is used by R API to add global transformation object during the auto gating.
 * @param tName transformation name (usually channel name)
 * @param tm trans_map
 */
void GatingSet::addTransMap(string gName,trans_map tm){
	/*
	 * assumption is there is either none transformation group exists before adding
	 */
	if(gTrans.size() == 0){

		trans_global thisTransGroup = trans_global();
		thisTransGroup.setGroupName(gName);
		thisTransGroup.setTransMap(tm);
		gTrans.push_back(thisTransGroup);
	}
	else
		throw(domain_error("transformation group already exists!Can't add the second one."));

}
