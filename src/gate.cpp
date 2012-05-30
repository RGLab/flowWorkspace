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
 * original c version
 */
void inPolygon_c(double *data, int nrd,
            double *vertices, int nrv, int *res) {

  int i, j, counter;
  double xinters;
  double p1x, p2x, p1y, p2y;

  for(i=0; i<nrd; i++){//iterate over points
    p1x=vertices[0];
    p1y=vertices[nrv];
    counter=0;
    for(j=1; j < nrv+1; j++){// iterate over vertices
      /*p1x,p1y and p2x,p2y are the endpoints of the current vertex*/
      if (j == nrv){//the last vertice must "loop around"
	p2x = vertices[0];
	p2y = vertices[0+nrv];
      }//if
      else{
	p2x = vertices[j];
	p2y = vertices[j+nrv];
      }//else
      /*if horizontal ray is in range of vertex find the x coordinate where
	ray and vertex intersect*/
      if(data[i+nrd] >= min(p1y, p2y) && data[i+nrd] < max(p1y, p2y) &&
         data[i] <= max(p1x, p2x)){
	xinters = (data[i+nrd]-p1y)*(p2x-p1x)/(p2y-p1y)+p1x;
	/*if intersection x coordinate == point x coordinate it lies on the
	  boundary of the polygon, which means "in"*/
	if(xinters==data[i]){
	  counter=1;
	  break;
	}//if
	/*count how many vertices are passed by the ray*/
	if (xinters > data[i]){
	  counter++;
	}//if
      }//if
      p1x=p2x;
      p1y=p2y;
    }//for j
    /*uneven number of vertices passed means "in"*/
    if(counter % 2 == 0){
      res[i]=0;
    }//if
    else{
      res[i]=1;
    }//else
  }//for i
}//function

/*
 * when the original gate vertices are at the edge of flowFrame range
 * it is likely that the gates were truncated in flowJo xml
 * currently what we can do is to extend it to the real data range to avoid losing
 * the data points that are below flowFrame range (-111 is the constant that is used in flowCore
 * to cut data range)
 */
void polygonGate::extend(flowData & fdata,unsigned short dMode){
	valarray<double> xdata=fdata.subset(params.at(0));
	valarray<double> ydata=fdata.subset(params.at(1));

	/*
	 * get R_min
	 */
	double xMin=xdata.min();
	double yMin=ydata.min();
	for(unsigned i=0;i<vertices.size();i++)
	{
		if(vertices.at(i).x<=-111)
		{
			if(dMode>=POPULATION_LEVEL)
				cout <<"extending "<<params.at(0)<<"from "<<vertices.at(i).x<<" to :"<<xMin<<endl;
			vertices.at(i).x=xMin;
		}
		if(vertices.at(i).y<=-111)
		{
			if(dMode>=POPULATION_LEVEL)
				cout <<"extending "<<params.at(1)<<"from "<<vertices.at(i).y<<" to :"<<yMin<<endl;
			vertices.at(i).y=yMin;

		}
	}
}
void rangegate::extend(flowData & fdata,unsigned short dMode){
	valarray<double> data_1d=fdata.subset(param.name);

	/*
	 * get R_min
	 */
	double xMin=data_1d.min();
	if(param.min<=-111)
	{
		if(dMode>=POPULATION_LEVEL)
			cout <<"extending "<<param.name<<"from "<<param.min<<" to :"<<xMin<<endl;
		param.min=xMin;
	}


}
/*
 * TODO:try within method from boost/geometries forboost.polygon
 *  reimplement c++ version of inPolygon_c
 *  indices are allocated within gating function, so it is up to caller to free it
 *  and now it is freed in destructor of its owner "nodeProperties" object
 */


POPINDICES polygonGate::gating(flowData & fdata){



	unsigned nVertex=vertices.size();

	valarray<double> xdata=fdata.subset(params.at(0));
	valarray<double> ydata=fdata.subset(params.at(1));

	unsigned nEvents=xdata.size();
	//init the indices
	POPINDICES ind(nEvents);


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

void polygonGate::transforming(trans_local & trans,unsigned short dMode){
	/*
	 * get channel names to select respective transformation functions
	 */
	string channel_x=params.at(0);
	string channel_y=params.at(1);

	//get vertices in valarray format
	vertices_valarray vert(getVertices());

	/*
	 * do the actual transformations
	 */
	transformation * trans_x=trans.getTran(channel_x);
	transformation * trans_y=trans.getTran(channel_y);


	if(trans_x!=NULL)
	{
		if(dMode>=POPULATION_LEVEL)
			cout<<"transforming: "<<channel_x<<endl;;
		valarray<double> output_x(trans_x->transforming(vert.x));
		for(unsigned i=0;i<vertices.size();i++)
			vertices.at(i).x=output_x[i];// yodate coordinates-based vertices
	}
	if(trans_y!=NULL)
	{
		if(dMode>=POPULATION_LEVEL)
			cout<<"transforming: "<<channel_y<<endl;;
		valarray<double> output_y(trans_y->transforming(vert.y));
		for(unsigned i=0;i<vertices.size();i++)
			vertices.at(i).y=output_y[i];
	}
	if(dMode>=POPULATION_LEVEL)
		cout<<endl;

}

void rangegate::transforming(trans_local & trans,unsigned short dMode){

	vertices_valarray vert=getVertices();

	transformation * curTrans=trans.getTran(param.name);
	if(curTrans!=NULL)
	{
		if(dMode>=POPULATION_LEVEL)
			cout<<param.name<<endl;
		valarray<double> output(curTrans->transforming(vert.x));
		param.min=output[0];
		param.max=output[1];
	}



}
POPINDICES rangegate::gating(flowData & fdata){

	valarray<double> data_1d=fdata.subset(param.name);

	unsigned nEvents=data_1d.size();
	//init the indices
	POPINDICES ind(nEvents);

	/*
	 * actual gating
	 */
	for(unsigned i=0;i<nEvents;i++)
	{
		ind[i]=data_1d[i]<=param.max&&data_1d[i]>=param.min;
	}
	return ind;

}
vertices_valarray polygonGate::getVertices(){

	vertices_valarray res;
	unsigned nSize=vertices.size();
	res.resize(nSize);
	for(unsigned i=0;i<nSize;i++)
	{
		res.x[i]=vertices.at(i).x;
		res.y[i]=vertices.at(i).y;
	}
	return res;
}


vertices_valarray rangegate::getVertices(){

	vertices_valarray res;
	res.resize(2);
	res.x[0]=param.min;
	res.x[1]=param.max;

	return res;
}

