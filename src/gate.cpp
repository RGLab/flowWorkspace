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
 * ellipse gate is parsed as polygon at the parsing xml stage since it is stored in polygon format
 * we do the calculation of major,minor axis and center coordinates here.
 * assume four antipodal points of ellipse is provided as input
 */
gate * polygonGate::toEllipseGate(){
	ellipseGate *g=new ellipseGate();
	//copy parameter names directly from polygate object
	g->params=this->params;
	if(this->vertices.size()!=4)
		throw(domain_error("fail to convert to ellipse since the vertices number is not 4!"));

	/*
	 * cal major,minor axis and center point
	 *  by finding min and max distance among four points
	 */

	g->a=numeric_limits<int>::min();//init major axis
	g->b=numeric_limits<int>::max();//init minor axis
	for(unsigned i=0;i<4;i++)
		for(unsigned j=i+1;j<4;j++)
		{
			coordinate p1=vertices.at(i);
			coordinate p2=vertices.at(j);
			double dist=sqrt(pow(p1.x-p2.x,2)+pow(p1.y-p2.y,2))/2;
			if(dist>g->a)
			{
				//update major axis
				g->a=dist;
				//update center point
				g->center.x=(p1.x+p2.x)/2;
				g->center.y=(p1.y+p2.y)/2;
			}

			if(dist<g->b)
				g->b=dist;//update minor axis
		}


	return g;
}

gate * polygonGate::toRangeGate(){
	rangegate *g=new rangegate();
	if(this->vertices.size()!=2)
			throw(domain_error("fail to convert to Range Gate since the vertices number is not 2!"));


	g->param.name=this->params.at(0);
	coordinate p1=vertices.at(0);
	coordinate p2=vertices.at(1);
	if(p1.x!=p2.x)
	{
		g->param.min=min(p1.x,p2.x);
		g->param.max=max(p1.x,p2.x);
	}
	else
	{
		g->param.min=min(p1.y,p2.y);
		g->param.max=max(p1.y,p2.y);
	}


	return g;
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
	coordinate lb,lt,rb,rt;//left bottom,left top,right bottom,right top
	lb.x=params.at(0).min;
	lb.y=params.at(1).min;

	lt.x=params.at(0).min;
	lt.y=params.at(1).max;

	rb.x=params.at(0).max;
	rb.y=params.at(1).min;

	rt.x=params.at(0).max;
	rt.y=params.at(1).max;

	res->vertices.push_back(lb);
	res->vertices.push_back(lt);
	res->vertices.push_back(rb);
	res->vertices.push_back(rt);

	return res;
}

/*
 * TODO:try within method from boost/geometries forboost.polygon
 *  reimplement c++ version of inPolygon_c
 *  indices are allocated within gating function, so it is up to caller to free it
 *  and now it is freed in destructor of its owner "nodeProperties" object
 */

POPINDICES polygonGate::gating(const flowData & fdata){

	//init the indices
	POPINDICES ind(fdata.nEvents);

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
	valarray<double> data(fdata.data,nEvents*nChannls);
	valarray<double> xdata=data[slice(xParamInd,nEvents,1)];
	valarray<double> ydata=data[slice(yParamInd,nEvents,1)];

	unsigned counter;
	double xinters;
	double p1x, p2x, p1y, p2y;

	for(unsigned i=0; i<nEvents; i++)
	{//iterate over points
	p1x=vertices.at(0).x;
	p1y=vertices.at(0).y;
	counter=0;
	for(unsigned j=1; j <= nVertex; j++)
	{// iterate over vertices
	  /*p1x,p1y and p2x,p2y are the endpoints of the current vertex*/
	  if (j == nVertex)
	  {//the last vertice must "loop around"
		p2x = vertices.at(0).x;
		p2y = vertices.at(0).y;
	  }
	  else
	  {
		p2x = vertices.at(j).x;
		p2y = vertices.at(j).y;
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

POPINDICES rectGate::gating(const flowData & data){

	polygonGate* res=toPolygon();
	POPINDICES ind= res->gating(data);
	delete res;
	return ind;
}

POPINDICES rangegate::gating(const flowData & fdata){

	//init the indices
	POPINDICES ind(fdata.nEvents);

	unsigned nEvents=fdata.nEvents;
	unsigned nChannls=fdata.nChannls;
	//	unsigned nVertex=vertices.size();


	/*
	 * calculate the positions of this gate parameters
	 */
	vector<string> channels=fdata.params;
	vector<string>::iterator it1,it2;
	it1=channels.begin();
	it2=channels.end();
	unsigned paramInd=find(it1,it2,param.name)-it1;


	/*
	 *only get the data arrays of these two parameters
	 */
	valarray<double> data(fdata.data,nEvents*nChannls);
	valarray<double> data_1d=data[slice(paramInd,nEvents,1)];


	/*
	 * actual gating
	 */
	for(unsigned i=0;i<nEvents;i++)
	{
		ind[i]=data_1d[i]<=param.max&&data_1d[i]>=param.min;
	}
	return ind;

}
POPINDICES ellipseGate::gating(const flowData & fdata){

	//init the indices
	POPINDICES ind(fdata.nEvents);

	unsigned nEvents=fdata.nEvents;
	unsigned nChannls=fdata.nChannls;
//	unsigned nVertex=vertices.size();


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
	valarray<double> data(fdata.data,nEvents*nChannls);
	valarray<double> xdata=data[slice(xParamInd,nEvents,1)];
	valarray<double> ydata=data[slice(yParamInd,nEvents,1)];

	/*
	 * actual gating,using
	 */
	for(unsigned i=0;i<nEvents;i++)
	{
		double distance=pow(xdata[i]-center.x,2)/pow(a,2)+pow(ydata[i]-center.y,2)/pow(b,2);
		ind[i]=distance<=1;
	}
	return ind;
}

