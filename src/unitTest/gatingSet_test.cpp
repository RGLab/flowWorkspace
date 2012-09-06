/*
 * gatingSet_test.cpp
 *
 *  Created on: May 15, 2012
 *      Author: wjiang2
 */

#include "test_header.hpp"


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
			nodeProperties *node=gh->getNodeProperty(*it);
			cout<<*it<<"."<<node->getName()<<endl;
		}

		/*
		 * getNodes BFS
		 */
		cout<<endl<<"bfs node list"<<endl;

		vertices=gh->getVertices(BFS);
		for(VertexID_vec::iterator it=vertices.begin();it!=vertices.end();it++)
		{
			nodeProperties *node=gh->getNodeProperty(*it);
			cout<<*it<<"."<<node->getName()<<endl;
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
			nodeProperties *node=gh->getNodeProperty(u);
			cout<<u<<"."<<node->getName()<<":";
			cout<<node->getStats(false)["count"]<<endl;
			if(u!=ROOTNODE)
			{
				gate * g=node->getGate();
				cout<<typeid(*g).name()<<endl;


			}
		}

		/*
		 * getPopNames with full path
		 */
		cout<<endl<<"node list with/without full path:"<<endl;
		vector<string> popNames=gh->getPopNames(REGULAR,true);
		for(vector<string>::iterator it=popNames.begin();it!=popNames.end();it++)
			cout<<*it<<endl;
		popNames=gh->getPopNames(REGULAR,false);
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
void gs_attachCDF(GatingSet & gs,testSuit myTest)
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
	gs.attachData(ncFile,sampleNames,params);
}
void gs_gating(GatingSet &gs,string curSample){
	cout<<endl<<"do the gating after the parsing"<<endl;

	//read transformed data once for all nodes
	GatingHierarchy* gh=gs.getGatingHierarchy(curSample);

//	gh->loadData(curSample);//get flow data from cdf

	/*
	 * read flow data from cdf into memory first (mimic the R code)
	 */
	flowData res=gs.getNcObj().readflowData(curSample);

	/*
	 * then load data from memory (as it(res) is passed from R routine
	 */
	gh->loadData(res);//

	gh->extendGate();

	gh->transforming(false);

	gh->gating();

	gh->unloadData();

}
void gh_counts(GatingHierarchy* gh){
	cout<<endl<<"flowJo(flowcore) counts after gating"<<endl;
	VertexID_vec vertices=gh->getVertices(0);
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




}

void gs_parse(testSuit myTest,unsigned short dMode,bool isTemplate,bool isLoadArchive){

		GatingSet *gs;
		//create gating set object
		if(isLoadArchive)
		{
			gs=new GatingSet();
			restore_gs(*gs,myTest.archive);
		}
		else
			gs=new GatingSet(myTest.filename,true,myTest.sampNloc,dMode);

		//parse a set of sampleIDs
		vector<string> sampleIDs;
		for(map<string,string>::iterator it=myTest.samples.begin();it!=myTest.samples.end();it++)
			sampleIDs.push_back(it->first);
		if(isTemplate)
			sampleIDs.erase(sampleIDs.begin());//remove the first sample,which is used for testing gating template feature

		if(!isLoadArchive)
			gs->parseWorkspace(sampleIDs,true);

		cout<<endl<<"get sample names from gating set"<<endl;

		vector<string> samples=gs->getSamples();
		for(vector<string>::iterator it=samples.begin();it!=samples.end();it++)
			cout<<*it<<endl;

		GatingHierarchy* gh;

		string curSample=samples.at(0);
		if(!isLoadArchive)
			gs_attachCDF(*gs,myTest);
		gh=gs->getGatingHierarchy(curSample);
//		gh->printLocalTrans();
		gh_accessor_test(gh);
//		plotGraph(gh);

		if(!isLoadArchive)
			gs_gating(*gs,curSample);

		gh_counts(gh);

		if(!isLoadArchive)
			save_gs(*gs,myTest.archive);

		if(isTemplate)
		{


		/*
		 * gating_template_test
		 */
			cout<<"-- cloning getGatingHierarchy ---"<<endl;
			/*
			 * get sample names from myTest and remove the first one which was used to extract gating template
			 */
			vector<string> newSamples;
			for(map<string,string>::iterator it=myTest.samples.begin();it!=myTest.samples.end();it++)
				newSamples.push_back(it->second);
			newSamples.erase(newSamples.begin()+1);

			/*
			 * clone the previous parsed gating hierarchy:gh
			 */
	//		gh->printLocalTrans();
			GatingSet * newGS=new GatingSet(gh,newSamples,dMode);
			gs_attachCDF(*newGS,myTest);

			/*
			 * do the gating on cloned gating hierarchy
			 */
			string newSample=newSamples.at(0);
			GatingHierarchy* gh_new;
			gh_new=newGS->getGatingHierarchy(newSample);
			gh_new->printLocalTrans();
	//		gh_accessor_test(gh_new);

			gs_gating(*newGS,newSample);


			gh_counts(gh_new);

			delete newGS;
		}

		delete gs;

}






