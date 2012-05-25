/*
 * gatingSet_test.cpp
 *
 *  Created on: May 15, 2012
 *      Author: wjiang2
 */

#include "test_header.hpp"
void getCalTbl_test(GatingHierarchy*gh){
	cout<<endl<<"get trans from gating hierarchy"<<endl;
	map<string,transformation* > trans=gh->trans.transformations;

	for (map<string,transformation* >::iterator it=trans.begin();it!=trans.end();it++)
	{
		transformation * curTrans=it->second;


		if(!curTrans->calTbl.isInterpolated)
				throw(domain_error("non-interpolated calibration table:"+curTrans->name+curTrans->channel+" from channel"+it->first));
		Spline_Coefs obj=curTrans->calTbl.getCalTbl();

		cout<<it->first<<curTrans->name<<" "<<curTrans->channel<<endl;;

	}
}





void gh_accessor_test(GatingHierarchy* gh){
	/*
		 * getNodes by the T order
		 */

//		cout<<endl<<"tsorted node list"<<endl;
		VertexID_vec vertices;
		vertices=gh->getVertices(true);
		for(VertexID_vec::iterator it=vertices.begin();it!=vertices.end();it++)
		{
			nodeProperties *node=gh->getNodeProperty(*it);
			cout<<*it<<"."<<node->getName()<<endl;
		}

		/*
		 * getNodes by vertices ID order
		 * and get stats from each node
		 */

		cout<<endl<<"node list in regular order and stats,gate"<<endl;
		vertices=gh->getVertices(false);
		for(VertexID_vec::iterator it=vertices.begin();it!=vertices.end();it++)
		{
			VertexID u=*it;
			nodeProperties *node=gh->getNodeProperty(u);
			cout<<u<<"."<<node->getName()<<":";
			cout<<node->getStats(false)["count"]<<endl;
			if(u!=ROOTNODE)
			{
				cout<<node->getGate()->getName()<<endl;


			}
		}

		/*
		 * getPopNames with full path
		 */
		cout<<endl<<"node list with/without full path:"<<endl;
		vector<string> popNames=gh->getPopNames(false,true);
		for(vector<string>::iterator it=popNames.begin();it!=popNames.end();it++)
			cout<<*it<<endl;
		popNames=gh->getPopNames(false,false);
		for(vector<string>::iterator it=popNames.begin();it!=popNames.end();it++)
			cout<<*it<<endl;


		/*
		 * get children and parent node index
		 */

		cout<<endl<<"check parent node"<<endl;
		for(int i=-1;i<=11;i++)
		{
			VertexID_vec parent=gh->getParent(i);
			cout<<i<<"<--";
			for(VertexID_vec::iterator it=parent.begin();it!=parent.end();it++)
				cout<<*it<<" ";
			cout<<endl;
		}



		cout<<endl<<"check children node"<<endl;
		for(int i=-1;i<=11;i++)
		{
			VertexID_vec children=gh->getChildren(i);
			cout<<i<<"-->";
			for(VertexID_vec::iterator it=children.begin();it!=children.end();it++)
						cout<<*it<<",";
			cout<<endl;
		}
}
void gh_gating(GatingSet &gs,testSuit myTest){
	cout<<endl<<"do the gating after the parsing"<<endl;
	string ncFile=myTest.ncfile;
	//read colnames from text
	vector<string> params;
	vector<string> sampleNames=gs.getSamples();

	std::ifstream myfile;
	myfile.open(myTest.colfile.c_str(),ifstream::in);

	vector<string> myLines;
	string line;
	while (std::getline(myfile, line))
	{
		params.push_back(line);
	}

	myfile.close();

	string curSample=sampleNames.at(0);
	gs.attachData(ncFile,sampleNames,params);
	//read transformed data once for all nodes
	GatingHierarchy* gh=gs.getGatingHierarchy(curSample);
	gh->loadData(curSample);

	gh->extendGate();

	gh->transforming(false);

	gh->gating();

	gh->unloadData();

}
void gh_counts(GatingHierarchy* gh){
	cout<<endl<<"flowJo(flowcore) counts after gating"<<endl;
	VertexID_vec vertices=gh->getVertices(false);
	for(VertexID_vec::iterator it=vertices.begin();it!=vertices.end();it++)
	{
		VertexID u=*it;
		nodeProperties *node=gh->getNodeProperty(u);
		cout<<u<<"."<<node->getName()<<":";
		cout<<node->getStats(false)["count"];
		cout<<"("<<node->getStats(true)["count"]<<") "<<endl;

//			if(u!=ROOTNODE)
//				cout<<node->getGate()->getName()<<endl;
	}

	/*
	 * plot gating hierarchy tree
	 */

//		gh->drawGraph("../output/test.dot");
//		system("dot2gxl ../output/test.dot -o ../output/test.gxl");
	//	return("test.gxl");

}
void gating_template_test(testSuit myTest){

	GatingSet gs(myTest.filename,true,4);
	/*
	 * parse gh from one sample
	 * use gs as dummy gating set to hold this gh
	 */
	string tmpSID=myTest.samples.begin()->first;
	vector<string> sampleIDs;
	sampleIDs.push_back(tmpSID);
	gs.parseWorkspace(sampleIDs,true);

	//use this gh as template to duplicate n copies of gh
//	unsigned nFile=2;
//	gs.clone(tmpSID,nFile);
//
//	string newData=myTest.ncfile;
//	//read colnames from text
//	vector<string> params;
//	vector<string> newSampleNames;
//
//	std::ifstream myfile;
//	myfile.open(myTest.colfile.c_str(),ifstream::in);
//
//	vector<string> myLines;
//	string line;
//	while (std::getline(myfile, line))
//	{
//		params.push_back(line);
//	}
//
//	myfile.close();
//
//	gs.attachData(newData,newSampleNames,params);
//	//read transformed data once for all nodes
//	for(gh_map::iterator it=gs.ghs)
//	gh->loadData();
//
//	gh->extendGate();
//
//	gh->transforming(false);
//
//	gh->gating();
//
//	gh->unloadData();


}
void gs_test(testSuit myTest){

		//create gating set object
		GatingSet gs(myTest.filename,true,0);

		//parse a set of sampleIDs
		vector<string> sampleIDs;
		for(map<string,string>::iterator it=myTest.samples.begin();it!=myTest.samples.end();it++)
			sampleIDs.push_back(it->first);

		gs.parseWorkspace(sampleIDs,true);

		cout<<endl<<"get sample names from gating set"<<endl;

		vector<string> samples=gs.getSamples();
		for(vector<string>::iterator it=samples.begin();it!=samples.end();it++)
			cout<<*it<<endl;

		GatingHierarchy* gh;

		gh=gs.getGatingHierarchy(0);

//		getCalTbl_test(gh);

//		gh_accessor_test(gh);

		gh_gating(gs,myTest);

		gh_counts(gh);



}
