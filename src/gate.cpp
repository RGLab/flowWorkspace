/*
 * gate.cpp
 *
 *  Created on: Apr 14, 2012
 *      Author: mike
 */



#include "include/gate.hpp"
#include <algorithm>
#include <valarray>
/*
 * modified version of original c function
 * double --> float
 * int --> unsigned
 * int * --> POPINDICES &
 * float * --> const vector<coordinate>&
 * float *--> const flowData &
 */
//void inPolygon(const flowData & fdata, const vector<coordinate>& vertices, POPINDICES &res) {
//
//  unsigned nEvents=fdata.nEvents;
//  unsigned nChannls=fdata.nChannls;
//  unsigned nVertex=vertices.size();
//  valarray<float> data(fdata.data,nChannls*nEvents);
//  fdata.params
//
//  unsigned i, j, counter;
//  float xinters;
//  float p1x, p2x, p1y, p2y;
//
//  for(i=0; i<nEvents; i++)
//  {//iterate over points
//    p1x=vertices.at(0).first;
//    p1y=vertices.at(0).second;
//    counter=0;
//    for(j=1; j <= nVertex; j++)
//    {// iterate over vertices
//      /*p1x,p1y and p2x,p2y are the endpoints of the current vertex*/
//      if (j == nVertex)
//      {//the last vertice must "loop around"
//		p2x = vertices.at(0).first;
//		p2y = vertices.at(0).second;
//      }
//      else
//      {
//		p2x = vertices.at(j).first;
//		p2y = vertices.at(j).second;
//      }
//      /*if horizontal ray is in range of vertex find the x coordinate where
//		ray and vertex intersect*/
//      if(data[i+nEvents] >= min(p1y, p2y) && data[i+nEvents] < max(p1y, p2y) &&data[i] <= max(p1x, p2x))
//      {
//		  xinters = (data[i+nEvents]-p1y)*(p2x-p1x)/(p2y-p1y)+p1x;
//		/*if intersection x coordinate == point x coordinate it lies on the
//		  boundary of the polygon, which means "in"*/
//		if(xinters==data[i])
//		{
//		  counter=1;
//		  break;
//		}
//		/*count how many vertices are passed by the ray*/
//		if (xinters > data[i])counter++;
//      }
//      p1x=p2x;
//      p1y=p2y;
//    }
//    /*uneven number of vertices passed means "in"*/
//    if(counter % 2 == 0)
//      res[i]=false;
//    else
//      res[i]=true;
//
//  }
//}

/*
 *  reimplement c++ version of inPolygon_c
 *  indices are allocated within gating function, so it is up to caller to free it
 *  and now it is freed in destructor of its owner "nodeProperties" object
 */

POPINDICES polygonGate::gating(const flowData & fdata){

	//init the indices
//	ind(fdata.nEvents);
	POPINDICES ind(fdata.nEvents);

//	inPolygon(fdata.data,vertices,ind);
	unsigned nEvents=fdata.nEvents;
	unsigned nChannls=fdata.nChannls;
	unsigned nVertex=vertices.size();


	/*
	 * calculate the positions of this gate parameters
	 */
	string xParam=params.at(0);
	string yParam=params.at(1);
	vector<string> channels=fdata.params;
	vector<string>::iterator it1,it2;
	it1=channels.begin();
	it2=channels.end();
	unsigned xParamInd=find(it1,it2,xParam)-it1;
	unsigned yParamInd=find(it1,it2,yParam)-it1;

	/*
	 *only get the data arrays of these two parameters
	 */
	valarray<float> data(fdata.data,nEvents*nChannls);
	valarray<float> xdata=data[slice(xParamInd,nEvents,1)];
	valarray<float> ydata=data[slice(yParamInd,nEvents,1)];

	unsigned counter;
	float xinters;
	float p1x, p2x, p1y, p2y;

	for(unsigned i=0; i<nEvents; i++)
	{//iterate over points
	p1x=vertices.at(0).first;
	p1y=vertices.at(0).second;
	counter=0;
	for(unsigned j=1; j <= nVertex; j++)
	{// iterate over vertices
	  /*p1x,p1y and p2x,p2y are the endpoints of the current vertex*/
	  if (j == nVertex)
	  {//the last vertice must "loop around"
		p2x = vertices.at(0).first;
		p2y = vertices.at(0).second;
	  }
	  else
	  {
		p2x = vertices.at(j).first;
		p2y = vertices.at(j).second;
	  }
	  /*if horizontal ray is in range of vertex find the x coordinate where
		ray and vertex intersect*/
	  if(ydata[i] >= min(p1y, p2y) && ydata[i] < max(p1y, p2y) &&xdata[i] <= max(p1x, p2x))
	  {
		  xinters = (ydata[i]-p1y)*(p2x-p1x)/(p2y-p1y)+p1x;
		/*if intersection x coordinate == point x coordinate it lies on the
		  boundary of the polygon, which means "in"*/
		if(xinters==xdata[i])
		{
		  counter=1;
		  break;
		}
		/*count how many vertices are passed by the ray*/
		if (xinters > xdata[i])counter++;
	  }
	  p1x=p2x;
	  p1y=p2y;
	}
	/*uneven number of vertices passed means "in"*/

	ind[i]=((counter % 2) != 0);

	}
	return ind;
}

/*
 * up to caller to free the memory
 */
polygonGate* rangegate::toPolygon(){

	throw(domain_error("convertion not valid at this point"));
	polygonGate* res=new polygonGate();
//	res->params.push_back(pName)

	return res;
}

POPINDICES rangegate::gating(const flowData & data){

	polygonGate* res=toPolygon();
	POPINDICES ind= res->gating(data);
	delete res;
	return ind;

}
/*
 * up to caller to free the memory
 */
polygonGate* rectGate::toPolygon(){
	polygonGate* res=new polygonGate();
	//covert param names
	res->params.push_back(params.at(0).name);//x
	res->params.push_back(params.at(1).name);//y
	//convert vertices
	res->vertices.push_back(coordinate(params.at(0).min,params.at(1).min));//x1,y1
	res->vertices.push_back(coordinate(params.at(0).max,params.at(1).max));//x2,y2

	res->vertices.push_back(coordinate(params.at(0).max,params.at(1).min));//x2,y1
	res->vertices.push_back(coordinate(params.at(0).min,params.at(1).max));//x1,y2

	return res;
}

POPINDICES rectGate::gating(const flowData & data){

	polygonGate* res=toPolygon();
	POPINDICES ind= res->gating(data);
	delete res;
	return ind;
}

/*
 * up to caller to free the memory
 */
polygonGate * ellipseGate::toPolygon(){

	//gating

	polygonGate* res=new polygonGate();
	return res;
}

POPINDICES ellipseGate::gating(const flowData & data){
	polygonGate* res=toPolygon();
	POPINDICES ind= res->gating(data);
	delete res;
	return ind;
}

