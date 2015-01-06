/*
 * gatingSet_test.cpp
 *
 *  Created on: May 15, 2012
 *      Author: wjiang2
 */

#include "test_header.hpp"
#include <boost/dynamic_bitset.hpp>
#include <boost/serialization/bitset.hpp>
#include <bitset>
#include <boost/serialization/array.hpp>
#include <boost/math/distributions/normal.hpp>

hdfFlow attachData(string fileName,vector<string> sampleNames,vector<string> params){
	hdfFlow nc;
	nc.fileName_set(fileName);
	nc.params_set(params);
	nc.sample_set(sampleNames);
	return nc;
}
/*
	 * plot gating hierarchy tree
	 */


void plotGraph(GatingHierarchy* gh){

			gh->drawGraph("../output/test.dot");
			system("dot2gxl ../output/test.dot -o ../output/test.gxl");
}

void gh_accessor_test(GatingHierarchy* gh){
	/*
		 * getNodes by the T order
		 */

		cout<<endl<<"tsorted node list"<<endl;
		VertexID_vec vertices;
		vertices=gh->getVertices(TSORT);
		for(VertexID_vec::iterator it=vertices.begin();it!=vertices.end();it++)
		{
			nodeProperties &node=gh->getNodeProperty(*it);
			cout<<*it<<"."<<node.getName()<<endl;
		}

		/*
		 * getNodes BFS
		 */
		cout<<endl<<"bfs node list"<<endl;

		vertices=gh->getVertices(BFS);
		for(VertexID_vec::iterator it=vertices.begin();it!=vertices.end();it++)
		{
			nodeProperties &node=gh->getNodeProperty(*it);
			cout<<*it<<"."<<node.getName()<<endl;
		}


		cout<<endl<<"compensation info"<<endl;
		compensation comp=gh->getCompensation();
		cout<<"cid:"<<comp.cid<<endl;
		cout<<"comment:"<<comp.comment<<endl;
		/*
		 * getNodes by vertices ID order
		 * and get stats from each node
		 */

		cout<<endl<<"node list in regular order and stats,gate"<<endl;
		vertices=gh->getVertices(REGULAR);
		for(VertexID_vec::iterator it=vertices.begin();it!=vertices.end();it++)
		{
			VertexID u=*it;
			nodeProperties &node=gh->getNodeProperty(u);
			cout<<u<<"."<<node.getName()<<":";
			cout<<node.getStats(false)["count"]<<endl;
			if(u!=ROOTNODE)
			{
				gate * g=node.getGate();
				cout<<typeid(*g).name()<<endl;


			}
		}

		/*
		 * getPopNames with full path
		 */
		cout<<endl<<"node list with/without full path:"<<endl;
		vector<string> popNames=gh->getPopPaths(REGULAR,true,true);
		for(vector<string>::iterator it=popNames.begin();it!=popNames.end();it++)
			cout<<*it<<endl;
		popNames=gh->getPopPaths(REGULAR,false,true);
		for(vector<string>::iterator it=popNames.begin();it!=popNames.end();it++)
			cout<<*it<<endl;


		/*
		 * get children and parent node index
		 */

		cout<<endl<<"check parent node"<<endl;
		for(size_t i=0;i<vertices.size();i++)
		{
			if(i!=0)
			{
				VertexID parent=gh->getParent(i);
				cout<<i<<"<--"<<parent<<" ";
				cout<<endl;
			}
		}



		cout<<endl<<"check children node"<<endl;
		for(size_t i=0;i<vertices.size();i++)
		{
			VertexID_vec children=gh->getChildren(i);
			cout<<i<<"-->";
			for(VertexID_vec::iterator it=children.begin();it!=children.end();it++)
						cout<<*it<<",";
			cout<<endl;
		}
}
hdfFlow gs_attachCDF(GatingSet & gs,testCase myTest)
{
	string ncFile=myTest.ncfile;

	/*
	 * get sample names from myTest
	 */
	vector<string> params;
	vector<string> sampleNames;
	for(map<string,string>::iterator it=myTest.samples.begin();it!=myTest.samples.end();it++)
		sampleNames.push_back(it->second);

	/*
	 * read colnames from text
	 */
	std::ifstream myfile;
	myfile.open(myTest.colfile.c_str(),ifstream::in);

	vector<string> myLines;
	string line;
	while (std::getline(myfile, line))
	{
		params.push_back(line);
	}

	myfile.close();

	/*
	 * attach Data source  to gs
	 */
	return attachData(ncFile,sampleNames,params);
}
void gs_gating(GatingSet &gs,string curSample, hdfFlow nc){
	cout<<endl<<"do the gating after the parsing"<<endl;

	//read transformed data once for all nodes
	GatingHierarchy* gh=gs.getGatingHierarchy(curSample);

//	gh->loadData(curSample);//get flow data from cdf

	/*
	 * read flow data from cdf into memory first (mimic the R code)
	 */
	flowData res=nc.readflowData(curSample);

	/*
	 * then load data from memory (as it(res) is passed from R routine
	 */
	gh->loadData(res);//
	gh->extendGate(0);
	gh->transformGate();
	gh->transforming();

	gh->gating(0,false);
	gh->unloadData();

}
void gh_counts(GatingHierarchy* gh,vector<bool> &isEqual, const float tolerance){
	cout<<endl<<"flowJo(flowcore) counts after gating"<<endl;
	VertexID_vec vertices=gh->getVertices(0);
	for(VertexID_vec::iterator it=vertices.begin();it!=vertices.end();it++)
	{
		VertexID u=*it;
		if(u!=ROOTNODE){
			nodeProperties &node=gh->getNodeProperty(u);
			int flowJoCount = node.getStats(false)["count"];
			if(flowJoCount != -1) //skip the unrecorded flowJo counts
			{

				int myCount = node.getStats(true)["count"];
				cout<<u<<"."<<node.getName()<<":";
				cout<< flowJoCount;
				cout<<"("<<myCount<<") "<< "cv = ";

				bool thisEqual;
				float thisCV ;
				float thisTol = tolerance;
				if(flowJoCount == myCount){
					thisEqual = true;
					thisCV = 0;
				}
				else
				{
					float min = flowJoCount>myCount?myCount:flowJoCount;
					float max = flowJoCount<myCount?myCount:flowJoCount;
					float mean = (min+max)/2;
					float sd = sqrt((pow((min-mean), 2) + pow((max-mean), 2))/2);
					boost::math::normal_distribution<> dist(mean,sd);
					float Q1 = quantile(dist,0.25);
					float Q3 = quantile(dist,0.75);
					float IQR = Q3 - Q1;
					thisCV = IQR/mean;
					thisTol = 1/max+thisTol;//add the weight of N of cells to make it more robust
					thisEqual = (thisCV < thisTol);
	//				cout << Q1 <<":" <<Q3 << " " << thisCV<<endl;
				}
				cout << thisCV << " tol = " << thisTol << endl;
				isEqual.push_back(thisEqual);
			}
		}
	}
}

void gh_removeGate(GatingHierarchy* gh){
	gh->removeNode(5);

}
void clone_test(testCase myTest){
	string archive=myTest.archive;
	GatingSet *gs=new GatingSet(archive,ARCHIVE_TYPE_BINARY, false);
	gs->clone_treeOnly(gs->getSamples());

}

void parser_test(testCase & myTest){

	bool isTemplate = myTest.isTemplate;
	bool isLoadArchive = myTest.isLoadArhive;
	unsigned format = myTest.archiveFormat;
	bool isParseGate = myTest.isParseGate;
	bool isSaveArchive = myTest.isSaveArchive;
	bool archiveType = myTest.archiveType;
	string archiveName = myTest.archive;

	if(archiveType)
		archiveName = archiveName.append(".pb");
	else
		archiveName = archiveName.append(".dat");
	unsigned short wsType = myTest.wsType;
		boost::scoped_ptr<GatingSet> gs;
		if(isLoadArchive)
		{

			gs.reset(new GatingSet(archiveName, format, archiveType));

		}
		else
			gs.reset(new GatingSet(myTest.filename,isParseGate,myTest.sampNloc,1,wsType));

		if(!isLoadArchive)
		{
			//parse a set of sampleIDs
			vector<string> sampleIDs;
			for(map<string,string>::iterator it=myTest.samples.begin();it!=myTest.samples.end();it++)
				sampleIDs.push_back(it->first);
			vector<string> sampleNames;
				for(map<string,string>::iterator it=myTest.samples.begin();it!=myTest.samples.end();it++)
					sampleNames.push_back(it->second);
			if(isTemplate)
				sampleIDs.erase(sampleIDs.begin());//remove the first sample,which is used for testing gating template feature

			if(!isLoadArchive)
				gs->parseWorkspace(sampleIDs,isParseGate,sampleNames);

			cout<<endl<<"get sample names from gating set"<<endl;


		}


		vector<string> samples=gs->getSamples();
		for(vector<string>::iterator it=samples.begin();it!=samples.end();it++)
			cout<<*it<<endl;

		GatingHierarchy* gh;

		string curSample=samples.at(0);
		/*
		 * nc is only used for c code debugging and not used
		 * in production code(the flow data is always from R) thus not archived
		 *
		 */
		hdfFlow nc = gs_attachCDF(*gs,myTest);

		gh=gs->getGatingHierarchy(curSample);

//		gh_accessor_test(gh);

		if(!isLoadArchive)
			gs_gating(*gs,curSample,nc);

		/*
		 * recompute the gate to check if the pop indices are restored properly
		 */
		if(isLoadArchive)
		{
			flowData res=nc.readflowData(curSample);
			gh->loadData(res);//
			gh->transforming();
			gh->gating(0,true);
			gh->unloadData();;
		}

		gh_counts(gh, myTest.isEqual, myTest.tolerance);
//		nodeProperties * np = gh->getNodeProperty(102);
//		vector<bool> thisInd = np->getIndices();
//		vector<bool> thisInd1(1024);
//		char thisInd[128];
//		boost::dynamic_bitset<>thisInd(346700);
//		bitset <346700> thisInd;

//		np->setIndices(10);
//		std::ofstream ofs(myTest.archive.c_str(),std::ios::out|std::ios::trunc|std::ios::binary);
//		boost::archive::text_oarchive oa(ofs);
//		boost::archive::xml_oarchive oa(ofs);
//		boost::archive::binary_oarchive oa(ofs);
//		oa << BOOST_SERIALIZATION_NVP(boost::serialization::make_array(&thisInd[0],thisInd.size()));

		if(isSaveArchive){
			if(archiveType == PB)
				gs->serialize_pb(archiveName);
			else
				gs->serialize_bs(archiveName,myTest.archiveFormat);
		}




}

//void gs_parse(testCase myTest,bool isTemplate,bool isLoadArchive){
//
//		bool isParseGate = true;
//		GatingSet *gs;
//		//create gating set object
//		if(isLoadArchive)
//		{
//			gs=new GatingSet();
//			restore_gs(*gs,myTest.archive, ARCHIVE_TYPE_BINARY);
//		}
//		else
//			gs=new GatingSet(myTest.filename,isParseGate,myTest.sampNloc);
//
//		/*
//		 * test cloning function
//		 */
////		GatingSet *clonedGs=gs->clone();
////		delete gs;
////		gs=clonedGs;
//
//		//parse a set of sampleIDs
//		vector<string> sampleIDs;
//		for(map<string,string>::iterator it=myTest.samples.begin();it!=myTest.samples.end();it++)
//			sampleIDs.push_back(it->first);
//		if(isTemplate)
//			sampleIDs.erase(sampleIDs.begin());//remove the first sample,which is used for testing gating template feature
//
//		if(!isLoadArchive)
//			gs->parseWorkspace(sampleIDs,isParseGate);
//
//		cout<<endl<<"get sample names from gating set"<<endl;
//
//
//
//		vector<string> samples=gs->getSamples();
//		for(vector<string>::iterator it=samples.begin();it!=samples.end();it++)
//			cout<<*it<<endl;
//
//		GatingHierarchy* gh;
//
//		string curSample=samples.at(0);
//		hdfFlow nc;
//		if(!isLoadArchive)
//			nc =gs_attachCDF(*gs,myTest);
//		gh=gs->getGatingHierarchy(curSample);
////		gh->printLocalTrans();
//		gh_accessor_test(gh);
////		plotGraph(gh);
//
//		if(!isLoadArchive)
//			gs_gating(*gs,curSample,nc);
//
//		gh_counts(gh, isEqual,tolerance);
//
////		gh_removeGate(gh);
//
////		gh_accessor_test(gh);
//
//		if(!isLoadArchive)
//			save_gs(*gs,myTest.archive, ARCHIVE_TYPE_BINARY);
//
//		if(isTemplate)
//		{
//
//
//		/*
//		 * gating_template_test
//		 */
//			cout<<"-- cloning getGatingHierarchy ---"<<endl;
//			/*
//			 * get sample names from myTest and remove the first one which was used to extract gating template
//			 */
//			vector<string> newSamples;
//			for(map<string,string>::iterator it=myTest.samples.begin();it!=myTest.samples.end();it++)
//				newSamples.push_back(it->second);
//			newSamples.erase(newSamples.begin()+1);
//
//			/*
//			 * clone the previous parsed gating hierarchy:gh
//			 */
//	//		gh->printLocalTrans();
//			GatingSet * newGS=new GatingSet(gh,newSamples);
//			hdfFlow nc = gs_attachCDF(*newGS,myTest);
//
//			/*
//			 * do the gating on cloned gating hierarchy
//			 */
//			string newSample=newSamples.at(0);
//			GatingHierarchy* gh_new;
//			gh_new=newGS->getGatingHierarchy(newSample);
////			gh_new->printLocalTrans();
//	//		gh_accessor_test(gh_new);
//
//			gs_gating(*newGS,newSample,nc);
//
//
////			gh_counts(gh_new);
//
//			delete newGS;
//		}
//
//		delete gs;
//
//}
//
//




