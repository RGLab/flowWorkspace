/*
 *code originally from  GatingSet.cpp
 *
 *  Created on: Mar 19, 2012
 *      Author: wjiang2
 */

#include "flowWorkspace/ws2gs.hpp"
/**
 * read xml file and create the appropriate flowJoWorkspace object
 * The reason to return a dynamically allocated pointer is solely for the sake of runtime polymorphism
 */
workspace * openWorkspace(string sFileName,unsigned short sampNloc,int xmlParserOption,unsigned short wsType)
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

		 if(g_loglevel>=GATING_SET_LEVEL)
			 COUT<<"internal gating set created from "<<sFileName<<endl;

		 wsPtr->parseVersionList();
		 return wsPtr;
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

GatingSet * ws2gs(workspace * ws, vector<string> sampleIDs,bool isParseGate, StringVec sampleNames)
{
	GatingSet * gs=new GatingSet();
	 /*
	  * parsing global calibration tables
	  */
	trans_global_vec gTrans;
	 if(isParseGate)
	 {
		 if(g_loglevel>=GATING_SET_LEVEL)
			 COUT<<"... start parsing global transformations... "<<endl;
		 gTrans=ws->getGlobalTrans();

	 }

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
		wsSampleNode curSampleNode=getSample(*ws, sampleID);

		GatingHierarchy *gh = ws2gh(curSampleNode,*ws,isParseGate,&gTrans,gs->get_globalBiExpTrans(),gs->get_globalLinTrans());

		gs->addGatingHierarchy(gh, sampleName);

		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
			COUT<<"Gating hierarchy created: "<<sampleName<<endl;
	}
	gs->set_gTrans(gTrans);
	return gs;
}
