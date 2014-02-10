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
* init the bool values in constructors
*/
rangeGate::rangeGate(){isTransformed=false;neg=false;isGained=false;}
polygonGate::polygonGate(){isTransformed=false;neg=false;isGained=false;}
boolGate::boolGate(){isTransformed=false;neg=false;isGained=false;}


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
 * when the original gate vertices are at the threshold
 * it is likely that the gates were truncated in flowJo xml
 * currently what we can do is to extend it to the real data range to avoid losing
 * the data points that are below this theshold range
 * to cut data range)
 */
void polygonGate::extend(flowData & fdata,float extend_val,unsigned short dMode){
	string x=param.xName();
	string y=param.yName();
	valarray<double> xdata(fdata.subset(x));
	valarray<double> ydata(fdata.subset(y));

	vector<coordinate> v=param.getVertices();
	/*
	 * get R_min
	 */
	double xMin=xdata.min();
	double yMin=ydata.min();
	for(unsigned i=0;i<v.size();i++)
	{
		if(v.at(i).x<=extend_val)
		{
			if(dMode>=POPULATION_LEVEL)
				COUT <<"extending "<<x<<"from "<<v.at(i).x<<" to :"<<xMin<<endl;
			v.at(i).x=xMin;
		}
		if(v.at(i).y<=extend_val)
		{
			if(dMode>=POPULATION_LEVEL)
				COUT <<"extending "<<y<<"from "<<v.at(i).y<<" to :"<<yMin<<endl;
			v.at(i).y=yMin;

		}
	}
	param.setVertices(v);
}

void polygonGate::gain(map<string,float> & gains,unsigned short dMode){

	if(!isGained)
		{
			vector<coordinate> vertices=param.getVertices();
			/*
			 * get channel names to select respective transformation functions
			 */
			string channel_x=param.xName();
			string channel_y=param.yName();



			map<string,float>::iterator it=gains.find(channel_x);
			if(it!=gains.end())
			{
				float this_gain = it->second;
				if(dMode>=POPULATION_LEVEL)
					COUT<<"adjusting: "<<channel_x<<endl;;

				for(unsigned i=0;i<vertices.size();i++)
					vertices.at(i).x=vertices.at(i).x/this_gain;
			}

			it=gains.find(channel_y);
			if(it!=gains.end())
			{
				float this_gain = it->second;
				if(dMode>=POPULATION_LEVEL)
					COUT<<"adjusting: "<<channel_y<<endl;;

				for(unsigned i=0;i<vertices.size();i++)
					vertices.at(i).y=vertices.at(i).y/this_gain;
			}


			if(dMode>=POPULATION_LEVEL)
				COUT<<endl;
			param.setVertices(vertices);
			isGained=true;
		}



}
void ellipseGate::extend(flowData & fdata,float extend_val,unsigned short dMode){

	/*
	 * get R_min
	 */
	vector<coordinate> v=param.getVertices();
	for(unsigned i=0;i<v.size();i++)
	{
		if((v.at(i).x<=extend_val)|(v.at(i).y<=extend_val))
		{
			throw(domain_error("try to extend the coordinates for ellipse gate!"));
		}

	}

}

void ellipseGate::gain(map<string,float> & gains,unsigned short dMode){
	if(!isGained)
	{
		/*
		 * get channel names to select respective transformation functions
		 */
		string channel_x=param.xName();
		string channel_y=param.yName();


		map<string,float>::iterator it=gains.find(channel_x);
		if(it!=gains.end())
		{
			float this_gain = it->second;
			if(dMode>=POPULATION_LEVEL)
				COUT<<"adjusting: "<<channel_x<<endl;;
			for(unsigned i=0;i<antipodal_vertices.size();i++)
				antipodal_vertices.at(i).x=antipodal_vertices.at(i).x/this_gain;
		}
		it=gains.find(channel_y);
		if(it!=gains.end())
		{
			float this_gain = it->second;
			if(dMode>=POPULATION_LEVEL)
				COUT<<"adjusting: "<<channel_y<<endl;;
			for(unsigned i=0;i<antipodal_vertices.size();i++)
				antipodal_vertices.at(i).y=antipodal_vertices.at(i).y/this_gain;
		}
		if(dMode>=POPULATION_LEVEL)
			COUT<<endl;

		isGained=true;
	}
}
/*
 * interpolation has to be done on the transformed original 4 coordinates
 * otherwise, the interpolation results will be wrong
 */
void ellipseGate::toPolygon(unsigned nVertices){




	/*
	 * using 4 vertices to fit polygon points
	 */
	vector<coordinate> v=antipodal_vertices;
	vector<coordinate> vertices=param.getVertices();
	vertices.clear();//reset the vertices

	unsigned nSize=v.size();
	/*
	 * scaling and centering the points
	 */
	coordinate mu;
	mu.x=0;
	mu.y=0;
	for(vector<coordinate>::iterator it=v.begin();it!=v.end();it++)
	{
		mu.x+=it->x;
		mu.y+=it->y;
	}
	mu.x=mu.x/nSize;
	mu.y=mu.y/nSize;

	coordinate sd;
	sd.x=0;
	sd.y=0;
	for(vector<coordinate>::iterator it=v.begin();it!=v.end();it++)
	{
		sd.x+=pow((it->x-mu.x),2);
		sd.y+=pow((it->y-mu.y),2);
	}
	sd.x=sqrt(sd.x/nSize);
	sd.y=sqrt(sd.y/nSize);

	for(vector<coordinate>::iterator it=v.begin();it!=v.end();it++)
	{
		it->x=(it->x-mu.x)/sd.x;
		it->y=(it->y-mu.y)/sd.y;
	}

	/*
	 * find the right positions of four antipodals
	 */
	coordinate R=*max_element(v.begin(),v.end(),compare_x);
	coordinate L=*min_element(v.begin(),v.end(),compare_x);

	coordinate T=*max_element(v.begin(),v.end(),compare_y);
	coordinate B=*min_element(v.begin(),v.end(),compare_y);

	/*
	 * calculate the a,b length
	 */
	coordinate E;
	E.x=hypot(L.x-R.x,L.y-R.y)/2;
	E.y=hypot(T.x-B.x,T.y-B.y)/2;

	/*
	 * calculate the rotation angle
	 */
	double phi=tan((R.y-L.y)/(R.x-L.x));
	double CY=(B.y+T.y)/2;
	double CX=(R.x+L.x)/2;

	double delta=2*PI/nVertices;
	/*
	 * fit the polygon points
	 */
	for(unsigned short i=0;i<nVertices;i++)
	{
		double S=i*delta;
		coordinate p;
		p.x=CX+E.x*cos(S)*cos(phi)-E.y*sin(S)*sin(phi);
		p.y=CY+E.x*cos(S)*sin(phi)+E.y*sin(S)*cos(phi);


		/*
		 * scale back
		 */
		p.x=p.x*sd.x+mu.x;
		p.y=p.y*sd.y+mu.y;

		vertices.push_back(p);
	}

	param.setVertices(vertices);

}
void rangeGate::extend(flowData & fdata,float extend_val,unsigned short dMode){
	string pName=param.getName();
	valarray<double> data_1d(fdata.subset(pName));

	/*
	 * get R_min
	 */
	double xMin=data_1d.min();
	if(param.getMin()<=extend_val)
	{
		if(dMode>=POPULATION_LEVEL)
			COUT <<"extending "<<pName<<"from "<<param.getMin()<<" to :"<<xMin<<endl;
		param.setMin(xMin);
	}


}
void rangeGate::gain(map<string,float> & gains,unsigned short dMode){
	if(!isGained)
	{
		vertices_valarray vert(getVertices());

		map<string,float>::iterator it=gains.find(param.getName().c_str());
		if(it!=gains.end())
		{
			float this_gain = it->second;

			if(dMode>=POPULATION_LEVEL)
				COUT<<"adjusting "<<param.getName()<<endl;

			param.setMin(param.getMin()/this_gain);
			param.setMax(param.getMax()/this_gain);
		}
		isGained=true;
	}
}

/*
 * TODO:try within method from boost/geometries forboost.polygon
 *  reimplement c++ version of inPolygon_c
 *  indices are allocated within gating function, so it is up to caller to free it
 *  and now it is freed in destructor of its owner "nodeProperties" object
 */


vector<bool> polygonGate::gating(flowData & fdata){


	/*
	 * must interpolate for ellipse gate
	 */
	if(getType()==ELLIPSEGATE)
	{
		ellipseGate * ep=dynamic_cast<ellipseGate *>(this);
		ep->toPolygon(100);
	}

	vector<coordinate> vertices=param.getVertices();
	unsigned nVertex=vertices.size();

	string x=param.xName();
	string y=param.yName();
	valarray<double> xdata(fdata.subset(x));
	valarray<double> ydata(fdata.subset(y));

	unsigned nEvents=xdata.size();
	//init the indices
	vector<bool> ind(nEvents);


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
	if(isNegate())
		ind.flip();
	return ind;
}
void ellipseGate::transforming(trans_local & trans,unsigned short dMode){
	if(!Transformed())
	{
		/*
		 * get channel names to select respective transformation functions
		 */
		string channel_x=param.xName();
		string channel_y=param.yName();

		//get vertices in valarray format
		vertices_valarray vert(antipodal_vertices);

		/*
		 * do the actual transformations
		 */
		transformation * trans_x=trans.getTran(channel_x);
		transformation * trans_y=trans.getTran(channel_y);


		if(trans_x!=NULL)
		{
			if(dMode>=POPULATION_LEVEL)
				COUT<<"transforming: "<<channel_x<<endl;;

			trans_x->transforming(vert.x);
			for(unsigned i=0;i<antipodal_vertices.size();i++)
				antipodal_vertices.at(i).x=vert.x[i];
		}
		if(trans_y!=NULL)
		{
			if(dMode>=POPULATION_LEVEL)
				COUT<<"transforming: "<<channel_y<<endl;;

			trans_y->transforming(vert.y);
			for(unsigned i=0;i<antipodal_vertices.size();i++)
				antipodal_vertices.at(i).y=vert.y[i];
		}
		if(dMode>=POPULATION_LEVEL)
			COUT<<endl;

		isTransformed=true;
	}
}
void polygonGate::transforming(trans_local & trans,unsigned short dMode){
	if(!Transformed())
	{
		vector<coordinate> vertices=param.getVertices();
		/*
		 * get channel names to select respective transformation functions
		 */
		string channel_x=param.xName();
		string channel_y=param.yName();

		//get vertices in valarray format
		vertices_valarray vert(vertices);

		/*
		 * do the actual transformations
		 */
		transformation * trans_x=trans.getTran(channel_x);
		transformation * trans_y=trans.getTran(channel_y);


		if(trans_x!=NULL)
		{
			if(dMode>=POPULATION_LEVEL)
				COUT<<"transforming: "<<channel_x<<endl;;
	//		valarray<double> output_x(trans_x->transforming(vert.x));
			trans_x->transforming(vert.x);
			for(unsigned i=0;i<vertices.size();i++)
	//			vertices.at(i).x=output_x[i];// yodate coordinates-based vertices
				vertices.at(i).x=vert.x[i];
		}
		if(trans_y!=NULL)
		{
			if(dMode>=POPULATION_LEVEL)
				COUT<<"transforming: "<<channel_y<<endl;;
	//		valarray<double> output_y(trans_y->transforming(vert.y));
			trans_y->transforming(vert.y);
			for(unsigned i=0;i<vertices.size();i++)
	//			vertices.at(i).y=output_y[i];
				vertices.at(i).y=vert.y[i];
		}
		if(dMode>=POPULATION_LEVEL)
			COUT<<endl;
		param.setVertices(vertices);
		isTransformed=true;
	}
}

void rangeGate::transforming(trans_local & trans,unsigned short dMode){
	if(!Transformed())
	{
		vertices_valarray vert(getVertices());

		transformation * curTrans=trans.getTran(param.getName());
		if(curTrans!=NULL)
		{
			if(dMode>=POPULATION_LEVEL)
				COUT<<"transforming "<<param.getName()<<endl;
	//		valarray<double> output(curTrans->transforming(vert.x));
	//		param.min=output[0];
	//		param.max=output[1];
			curTrans->transforming(vert.x);
			param.setMin(vert.x[0]);
			param.setMax(vert.x[1]);
		}
		isTransformed=true;
	}

}

vector<bool> rangeGate::gating(flowData & fdata){

	valarray<double> data_1d(fdata.subset(param.getName()));

	unsigned nEvents=data_1d.size();
	//init the indices
	vector<bool> ind(nEvents);

	/*
	 * actual gating
	 */
	for(unsigned i=0;i<nEvents;i++)
	{
		ind[i]=data_1d[i]<=param.getMax()&&data_1d[i]>=param.getMin();
	}


	if(isNegate())
		ind.flip();

	return ind;

}

vector<bool> rectGate::gating(flowData & fdata){

	vector<coordinate> vertices=param.getVertices();
	unsigned nVertex=vertices.size();
	if(nVertex!=2)
		throw(domain_error("invalid number of vertices for rectgate!"));
	string x=param.xName();
	string y=param.yName();
	valarray<double> xdata(fdata.subset(x));
	valarray<double> ydata(fdata.subset(y));

	unsigned nEvents=xdata.size();
	//init the indices
	vector<bool> ind(nEvents);

	/*
	 * actual gating
	 */
	for(unsigned i=0;i<nEvents;i++)
	{
		bool inX,inY;
		double xMin=vertices.at(0).x;
		double yMin=vertices.at(0).y;

		double xMax=vertices.at(1).x;
		double yMax=vertices.at(1).y;

		if(xMin>xMax||yMin>yMax)
			throw(domain_error("invalid vertices for rectgate!"));

		inX=xdata[i]<=xMax&&xdata[i]>=xMin;
		inY=ydata[i]<=yMax&&ydata[i]>=yMin;
		ind[i]=inX&&inY;
	}


	if(isNegate())
		ind.flip();

	return ind;

}
vertices_valarray paramPoly::toValarray(){

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


vertices_valarray paramRange::toValarray(){

	vertices_valarray res;
	res.resize(2);
	res.x[0]=min;
	res.x[1]=max;

	return res;
}


