/*
 * gatingSet_test.cpp
 *
 *  Created on: May 15, 2012
 *      Author: wjiang2
 */

#include "test_header.hpp"
//#include <boost/dynamic_bitset.hpp>
//#include <boost/serialization/bitset.hpp>
//#include <bitset>
//#include <boost/serialization/array.hpp>
#include <boost/math/distributions/normal.hpp>

/*
	 * plot gating hierarchy tree
	 */


void plotGraph(GatingHierarchy& gh){

			gh.drawGraph("../output/test.dot");
			system("dot2gxl ../output/test.dot -o ../output/test.gxl");
}

void gh_accessor_test(GatingHierarchy& gh){
	/*
		 * getNodes by the T order
		 */

		cout<<endl<<"tsorted node list"<<endl;
		VertexID_vec vertices;
		vertices=gh.getVertices(TSORT);
		for(VertexID_vec::iterator it=vertices.begin();it!=vertices.end();it++)
		{
			nodeProperties &node=gh.getNodeProperty(*it);
			cout<<*it<<"."<<node.getName()<<endl;
		}

		/*
		 * getNodes BFS
		 */
		cout<<endl<<"bfs node list"<<endl;

		vertices=gh.getVertices(BFS);
		for(VertexID_vec::iterator it=vertices.begin();it!=vertices.end();it++)
		{
			nodeProperties &node=gh.getNodeProperty(*it);
			cout<<*it<<"."<<node.getName()<<endl;
		}


		cout<<endl<<"compensation info"<<endl;
		compensation comp=gh.get_compensation();
		cout<<"cid:"<<comp.cid<<endl;
		cout<<"comment:"<<comp.comment<<endl;
		/*
		 * getNodes by vertices ID order
		 * and get stats from each node
		 */

		cout<<endl<<"node list in regular order and stats,gate"<<endl;
		vertices=gh.getVertices(REGULAR);
		for(VertexID_vec::iterator it=vertices.begin();it!=vertices.end();it++)
		{
			VertexID u=*it;
			nodeProperties &node=gh.getNodeProperty(u);
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
		vector<string> popNames=gh.getPopPaths(REGULAR,true,true);
		for(vector<string>::iterator it=popNames.begin();it!=popNames.end();it++)
			cout<<*it<<endl;
		popNames=gh.getPopPaths(REGULAR,false,true);
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
				VertexID parent=gh.getParent(i);
				cout<<i<<"<--"<<parent<<" ";
				cout<<endl;
			}
		}



		cout<<endl<<"check children node"<<endl;
		for(size_t i=0;i<vertices.size();i++)
		{
			VertexID_vec children=gh.getChildren(i);
			cout<<i<<"-->";
			for(VertexID_vec::iterator it=children.begin();it!=children.end();it++)
						cout<<*it<<",";
			cout<<endl;
		}
}

void gh_gating(GatingHierarchy & gh,bool is_fix_slash_in_channel_name, bool isH5, string h5_path,  compensation comp= compensation()){
	cout<<endl<<"do the gating after the parsing"<<endl;

//
//	gh.load_fdata_cache();//
//	if(comp.marker.size()>0)
//	{
//		comp.cid = "1";
//		gh.set_compensation(comp);
//	}
//	gh.compensate();
////	gh.adjustGate(gains);
//	gh.transform_gate();
//	gh.transform_data();
//	gh.extendGate(0);
//	gh.gating(0,false, true);
//	//sync comp & trans data
//
//	gh.release_fdata_cache(true);

}
void gh_counts(GatingHierarchy & gh,vector<bool> &isEqual, const float tolerance, const vector<VertexID> skipPops){
	cout<<endl<<"flowJo(flowcore) counts after gating"<<endl;
	VertexID_vec vertices=gh.getVertices(0);
	for(VertexID_vec::iterator it=vertices.begin();it!=vertices.end();it++)
	{
		VertexID u=*it;
		if(find(skipPops.begin(), skipPops.end(), u) == skipPops.end())//skip some pops that flowJo records the wrong counts
		{
			if(u!=ROOTNODE){
				nodeProperties &node=gh.getNodeProperty(u);
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
}

void gh_removeGate(GatingHierarchy& gh){
	gh.removeNode(5);

}
void clone_test(testCase myTest){
	string archive=myTest.archive;
	GatingSet gs=GatingSet(archive);
//	gs.sub_samples(gs.get_sample_uids()).deep_copy();

}

void parser_test(testCase & myTest){
	cout << "===============================================" << endl;
//	print_supported_workspace_version();
//	bool isTemplate = myTest.isTemplate;
	bool isLoadArchive = myTest.isLoadArhive;
//	unsigned format = myTest.archiveFormat;
	bool isSaveArchive = myTest.isSaveArchive;
//	bool archiveType = myTest.archiveType;
	string archiveName = myTest.archive;


	unique_ptr<GatingSet> gs;
	if(isLoadArchive)
	{

		gs.reset(new GatingSet(archiveName));

	}
	else
	{
		unique_ptr<flowJoWorkspace> ws = openWorkspace(myTest.filename, myTest.sample_name_location,myTest.xmlParserOption);
		gs = ws->to_GatingSet(myTest.group_id, myTest.config);
	}


	vector<string> samples=gs->get_sample_uids();
	for(vector<string>::iterator it=samples.begin();it!=samples.end();it++)
		cout<<*it<<endl;

	GatingHierarchy& gh=*(gs->getGatingHierarchy(samples[0]));


	/*
	 * recompute the gate to check if the pop indices are restored properly
	 */
	if(isLoadArchive)
	{
		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
			cout<<endl<<"Gating ..."<<endl;

		GatingSet cs = gs->get_cytoset();
		CytoFrameView fr = cs.get_cytoframe_view(samples[0]);
		CytoFramePtr frptr = fr.get_cytoframe_ptr();
		MemCytoFrame fr1(*frptr);
		gh.gating(fr1, 0,false, true);

	}

	gh_counts(gh, myTest.isEqual, myTest.tolerance, myTest.skipPops);

	if(isSaveArchive){
		gs->serialize_pb(archiveName, H5Option::copy);

	}

	cout << "===============================================" << endl;
}
