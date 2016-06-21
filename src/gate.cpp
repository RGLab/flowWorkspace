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
gate::gate():neg(false),isTransformed(false),isGained(false){};
gate::gate(const pb::gate & gate_pb):neg(gate_pb.neg()),isTransformed(gate_pb.istransformed()),isGained(gate_pb.isgained()){}
void gate::convertToPb(pb::gate & gate_pb){
	//cp basic members
		gate_pb.set_istransformed(isTransformed);
		gate_pb.set_neg(neg);
		gate_pb.set_isgained(isGained);
}
rangeGate::rangeGate():gate(){}
rangeGate::rangeGate(const pb::gate & gate_pb):gate(gate_pb),param(paramRange(gate_pb.rg().param())){}
void rangeGate::convertToPb(pb::gate & gate_pb){
	gate::convertToPb(gate_pb);
	gate_pb.set_type(pb::RANGE_GATE);
	//cp nested gate
	pb::rangeGate * g_pb = gate_pb.mutable_rg();
	//cp its unique member
	pb::paramRange * pr_pb = g_pb->mutable_param();
	param.convertToPb(*pr_pb);
}

polygonGate::polygonGate(const pb::gate & gate_pb):gate(gate_pb),param(paramPoly(gate_pb.pg().param())){}
void polygonGate::convertToPb(pb::gate & gate_pb){
	gate::convertToPb(gate_pb);

	gate_pb.set_type(pb::POLYGON_GATE);
	//cp nested gate
	pb::polygonGate * g_pb = gate_pb.mutable_pg();
	//cp its unique member
	pb::paramPoly * pr_pb = g_pb->mutable_param();
	param.convertToPb(*pr_pb);
}
boolGate::boolGate(const pb::gate & gate_pb):gate(gate_pb){
	const pb::boolGate & bg_pb = gate_pb.bg();
	for(int i = 0; i < bg_pb.boolopspec_size(); i++){
		const pb::BOOL_GATE_OP & thisOP_pb = bg_pb.boolopspec(i);
		BOOL_GATE_OP thisOP = BOOL_GATE_OP(thisOP_pb);
		boolOpSpec.push_back(thisOP);


	}
}
void boolGate::convertToPb(pb::gate & gate_pb){
	gate::convertToPb(gate_pb);

	gate_pb.set_type(pb::BOOL_GATE);
	//cp nested gate
	pb::boolGate * g_pb = gate_pb.mutable_bg();
	//cp its unique member
	for(unsigned i = 0; i < boolOpSpec.size(); i++){
		pb::BOOL_GATE_OP * gop_pb = g_pb->add_boolopspec();
		boolOpSpec.at(i).convertToPb(*gop_pb);
	}

}
ellipseGate::ellipseGate(const pb::gate & gate_pb):polygonGate(gate_pb),mu(coordinate(gate_pb.eg().mu())),dist(gate_pb.eg().dist()){
	const pb::ellipseGate & eg_pb = gate_pb.eg();
	for(int i = 0; i < eg_pb.antipodal_vertices_size(); i++){
		antipodal_vertices.push_back(coordinate(eg_pb.antipodal_vertices(i)));
	}
	for(int i = 0; i < eg_pb.cov_size(); i++){
		cov.push_back(coordinate(eg_pb.cov(i)));
	}
}

void ellipseGate::convertToPb(pb::gate & gate_pb){
	polygonGate::convertToPb(gate_pb);

	gate_pb.set_type(pb::ELLIPSE_GATE);
	//cp nested gate
	pb::ellipseGate * g_pb = gate_pb.mutable_eg();
	//cp its unique member
	g_pb->set_dist(dist);
	pb::coordinate * coor_pb = g_pb->mutable_mu();
	mu.convertToPb(*coor_pb);
	for(unsigned i = 0; i < cov.size(); i++){
		pb::coordinate * coor_pb = g_pb->add_cov();
		cov.at(i).convertToPb(*coor_pb);
	}
	for(unsigned i = 0; i < antipodal_vertices.size(); i++){
		pb::coordinate * coor_pb = g_pb->add_antipodal_vertices();
		antipodal_vertices.at(i).convertToPb(*coor_pb);
	}
}

void ellipsoidGate::convertToPb(pb::gate & gate_pb){
	ellipseGate::convertToPb(gate_pb);
	gate_pb.set_type(pb::ELLIPSOID_GATE);
}
ellipsoidGate::ellipsoidGate(const pb::gate & gate_pb):ellipseGate(gate_pb){
	//deal with legacy archive that did not interpolate ellipsoidGate
	if(param.getVertices().size() == 0)
		toPolygon(100);
};
void rectGate::convertToPb(pb::gate & gate_pb){
	polygonGate::convertToPb(gate_pb);
	gate_pb.set_type(pb::RECT_GATE);
}
rectGate::rectGate(const pb::gate & gate_pb):polygonGate(gate_pb){};

void logicalGate::convertToPb(pb::gate & gate_pb){
	boolGate::convertToPb(gate_pb);
	gate_pb.set_type(pb::LOGICAL_GATE);
}
logicalGate::logicalGate(const pb::gate & gate_pb):boolGate(gate_pb){};
polygonGate::polygonGate():gate(){};
boolGate::boolGate():gate(){};


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
void polygonGate::extend(flowData & fdata,float extend_val){
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
			if(g_loglevel>=POPULATION_LEVEL)
				COUT <<"extending "<<x<<"from "<<v.at(i).x<<" to :"<<xMin<<endl;
			v.at(i).x=min(xMin, v.at(i).x);
		}
		if(v.at(i).y<=extend_val)
		{
			if(g_loglevel>=POPULATION_LEVEL)
				COUT <<"extending "<<y<<"from "<<v.at(i).y<<" to :"<<yMin<<endl;
			v.at(i).y=min(yMin, v.at(i).y);

		}
	}
	param.setVertices(v);
}

void polygonGate::extend(float extend_val, float extend_to){
	string x=param.xName();
	string y=param.yName();

	vector<coordinate> v=param.getVertices();
	/*
	 * get R_min
	 */
	double xMin=extend_to;
	double yMin=extend_to;
	for(unsigned i=0;i<v.size();i++)
	{
		if(v.at(i).x<=extend_val)
		{
			if(g_loglevel>=POPULATION_LEVEL)
				COUT <<"extending "<<x<<"from "<<v.at(i).x<<" to :"<<xMin<<endl;
			v.at(i).x=min(xMin,v.at(i).x);
		}
		if(v.at(i).y<=extend_val)
		{
			if(g_loglevel>=POPULATION_LEVEL)
				COUT <<"extending "<<y<<"from "<<v.at(i).y<<" to :"<<yMin<<endl;
			v.at(i).y=min(yMin, v.at(i).y);

		}
	}
	param.setVertices(v);
}
void polygonGate::gain(map<string,float> & gains){

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
				if(g_loglevel>=POPULATION_LEVEL)
					COUT<<"adjusting: "<<channel_x<<endl;;

				for(unsigned i=0;i<vertices.size();i++)
					vertices.at(i).x=vertices.at(i).x/this_gain;
			}

			it=gains.find(channel_y);
			if(it!=gains.end())
			{
				float this_gain = it->second;
				if(g_loglevel>=POPULATION_LEVEL)
					COUT<<"adjusting: "<<channel_y<<endl;;

				for(unsigned i=0;i<vertices.size();i++)
					vertices.at(i).y=vertices.at(i).y/this_gain;
			}


			if(g_loglevel>=POPULATION_LEVEL)
				COUT<<endl;
			param.setVertices(vertices);
			isGained=true;
		}



}

ellipseGate::ellipseGate(coordinate _mu, vector<coordinate> _cov, double _dist):mu(_mu),cov(_cov), dist(_dist){
	isTransformed = true;
	isGained = true;
	neg = false;
}

ellipseGate::ellipseGate(vector<coordinate> _antipodal, vector<string> _params):antipodal_vertices(_antipodal),dist(1){
	isTransformed = false;
	isGained = false;
	neg = false;

	/*
	 * init the dummy vertices for base class
	 * (this deprecated inheritance exists for the sake of legacy archive)
	 */
	param.setName(_params);

}

void ellipseGate::extend(flowData & fdata,float extend_val){

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
void ellipseGate::extend(float extend_val, float extend_to){

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
void ellipseGate::gain(map<string,float> & gains){
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
			if(g_loglevel>=POPULATION_LEVEL)
				COUT<<"adjusting: "<<channel_x<<endl;;
			for(unsigned i=0;i<antipodal_vertices.size();i++)
				antipodal_vertices.at(i).x=antipodal_vertices.at(i).x/this_gain;
		}
		it=gains.find(channel_y);
		if(it!=gains.end())
		{
			float this_gain = it->second;
			if(g_loglevel>=POPULATION_LEVEL)
				COUT<<"adjusting: "<<channel_y<<endl;;
			for(unsigned i=0;i<antipodal_vertices.size();i++)
				antipodal_vertices.at(i).y=antipodal_vertices.at(i).y/this_gain;
		}
		if(g_loglevel>=POPULATION_LEVEL)
			COUT<<endl;

		isGained=true;
	}
}

/*
 * covert antipodal points to covariance matrix and mean
 * antipodal points must be transformed first.
 */
void ellipseGate::computeCov(){
	if(!Transformed())
		throw(domain_error("antipodal points of ellipseGate must be transformed before computing covariance matrix!"));

	vector<coordinate> v=antipodal_vertices;
	unsigned short nSize = v.size();
	if (nSize != 4)
		throw(domain_error("invalid number of antipodal points"));

	/*
	 * get center and set mu
	 */
	mu.x=0;
	mu.y=0;
	for(vector<coordinate>::iterator it=v.begin();it!=v.end();it++)
	{
		mu.x+=it->x;
		mu.y+=it->y;
	}
	mu.x=mu.x/nSize;
	mu.y=mu.y/nSize;

	//center the antipods
	for(vector<coordinate>::iterator it=v.begin();it!=v.end();it++)
	{
		it->x = it->x - mu.x;
		it->y = it->y - mu.y;
	}

	/*
	 * find the four positions of four antipodals
	 */

	//far right point
	vector<coordinate>::iterator R_it=max_element(v.begin(),v.end(),compare_x);
	coordinate R = *R_it;

	//far left point
	vector<coordinate>::iterator L_it=min_element(v.begin(),v.end(),compare_x);
	coordinate L = *L_it;

	// calculate the a length
	double a = hypot(L.x-R.x,L.y-R.y)/2;

	//use the rest of two points for computing b
	vector<coordinate> Q;
	for(vector<coordinate>::iterator it = v.begin();it!= v.end();it++){
		if(it != R_it && it != L_it)
			Q.push_back(*it);
	}
	coordinate V1 = Q.at(0);
	coordinate V2 = Q.at(1);
	double b = hypot(V1.x-V2.x,V1.y-V2.y)/2;

	double a2 = a * a ;
	double b2 = b * b ;


	//normailize R and V1 first
	double L_norm = hypot(L.x, L.y);
	double x1 = L.x/L_norm;
	double y1 = L.y/L_norm;

	double V1_norm = hypot(V1.x, V1.y);
	double x2 = V1.x/V1_norm;
	double y2 = V1.y/V1_norm;

	coordinate p1;
	p1.x = x1 * x1 * a2 + x2 * x2 * b2;
	p1.y = x1 * y1 * a2 + x2 * y2 * b2;

	coordinate p2;
	p2.x = p1.y;
	p2.y = y1 * y1 * a2 + y2 * y2 * b2;


	//set cov
	cov.push_back(p1);
	cov.push_back(p2);

	//set distance (in this calculation should always be 1)
	dist = 1;
}

/*
 * translated from flowCore::%in% method for ellipsoidGate
 */
vector<bool> ellipseGate::gating(flowData & fdata){


	// get data

	valarray<double> xdata(fdata.subset(param.xName()));
	valarray<double> ydata(fdata.subset(param.yName()));

	//center the data
	xdata = xdata - mu.x;
	ydata = ydata - mu.y;

	//inverse the cov matrix
	/*
	 * 	| a,b |
		| c,d | --> | aa, bb |
					| cc, dd |
	 */
	double a , b, c, d;
	a = cov.at(0).x;
	b = cov.at(0).y;
	c = cov.at(1).x;
	d = cov.at(1).y;

	double det = a* d - b* c;
	double aa, bb, cc, dd;
	aa = d/det;
	bb = -b/det;
	cc = -c/det;
	dd = a/det;

	// if inside of the ellipse
	unsigned nEvents=xdata.size();
	vector<bool> res (nEvents);
	for(unsigned i =0;i<nEvents;i++){
		double x = xdata[i];
		double y = ydata[i];
		res[i] = (x * x * aa + x* y * cc + x* y * bb + y * y * dd) <= pow(dist, 2);
	}

	return res;
}

void rangeGate::extend(flowData & fdata,float extend_val){
	string pName=param.getName();
	valarray<double> data_1d(fdata.subset(pName));

	/*
	 * get R_min
	 */
	double xMin=data_1d.min();
	if(param.getMin()<=extend_val)
	{
		if(g_loglevel>=POPULATION_LEVEL)
			COUT <<"extending "<<pName<<"from "<<param.getMin()<<" to :"<<xMin<<endl;
		param.setMin(min(xMin, param.getMin()));
	}


}
void rangeGate::extend(float extend_val, float extend_to){
	string pName=param.getName();


	double xMin= extend_to;
	if(param.getMin()<=extend_val)
	{
		if(g_loglevel>=POPULATION_LEVEL)
			COUT <<"extending "<<pName<<"from "<<param.getMin()<<" to :"<<xMin<<endl;
		param.setMin(min(xMin, param.getMin()));
	}


}
void rangeGate::gain(map<string,float> & gains){
	if(!isGained)
	{
		vertices_valarray vert(getVertices());

		map<string,float>::iterator it=gains.find(param.getName().c_str());
		if(it!=gains.end())
		{
			float this_gain = it->second;

			if(g_loglevel>=POPULATION_LEVEL)
				COUT<<"adjusting "<<param.getName()<<endl;

			param.setMin(param.getMin()/this_gain);
			param.setMax(param.getMax()/this_gain);
		}
		isGained=true;
	}
}


/*
 *  boost/geometries version some how does not
 *  perform better since some how convex_hull has to be
 *  used to correct the polygon constructor
 *
 */
//vector<bool> polygonGate::gating_bg(flowData & fdata){
//
//
//	vector<coordinate> vertices=param.getVertices();
//	unsigned nVertex=vertices.size();
//
//	string x=param.xName();
//	string y=param.yName();
//	valarray<double> xdata(fdata.subset(x));
//	valarray<double> ydata(fdata.subset(y));
//
//	unsigned nEvents=xdata.size();
//	//init the indices
//	vector<bool> ind(nEvents);
//
//	typedef boost::geometry::model::d2::point_xy<double> point_type;
//	typedef boost::geometry::model::polygon<point_type, true, true> polygon_type;
//
//	//construct boost polygon
//	polygon_type poly;
//	for(unsigned i = 0; i < nVertex; i++){
//		point_type this_point(vertices.at(i).x, vertices.at(i).y);
//		poly.outer().push_back(this_point);
//	}
//	polygon_type hull;
//
//	boost::geometry::convex_hull(poly, hull);
//
//
//
//	for(unsigned i = 0; i < nEvents; i++){
//		point_type p(xdata[i], ydata[i]);
//		ind[i] = boost::geometry::within(p, hull);
//	}
//
//
//	if(isNegate())
//		ind.flip();
//	return ind;
//}

/*
 *
 *  reimplement c++ version of inPolygon_c
 *  indices are allocated within gating function, so it is up to caller to free it
 *  and now it is freed in destructor of its owner "nodeProperties" object
 */
vector<bool> polygonGate::gating(flowData & fdata){




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

	double delta=2*pi/nVertices;
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

void ellipseGate::transforming(trans_local & trans){
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
			if(g_loglevel>=POPULATION_LEVEL)
				COUT<<"transforming: "<<channel_x<<endl;;

			trans_x->transforming(vert.x);
			for(unsigned i=0;i<antipodal_vertices.size();i++)
				antipodal_vertices.at(i).x=vert.x[i];
		}
		if(trans_y!=NULL)
		{
			if(g_loglevel>=POPULATION_LEVEL)
				COUT<<"transforming: "<<channel_y<<endl;;

			trans_y->transforming(vert.y);
			for(unsigned i=0;i<antipodal_vertices.size();i++)
				antipodal_vertices.at(i).y=vert.y[i];
		}
		if(g_loglevel>=POPULATION_LEVEL)
			COUT<<endl;
		isTransformed=true;

		//compute the covariance matrix after transformed
		computeCov();

	}
}
ellipsoidGate::ellipsoidGate(vector<coordinate> _antipodal, vector<string> _params):ellipseGate(_antipodal,_params)
{
	/*
	 * interpolate to polygon gate
	 */

	toPolygon(100);
}
/*
 *
 * we moved the interpolation to polygonGate form gating method to here because
 * gating may not be called when only gates to be extracted
 *
 *
 * ellipsoidGate does not follow the regular transforming process
 * for historical reason, it is defined in 256 * 256 scale.
 * For linear channel, we simply linear scale it back to raw scale
 * For non-linear channel, We need to first inverse transform it back to raw scale
 * before transforming to the ultimate appropriate data scale.
 */
void ellipsoidGate::transforming(trans_local & trans){

	if(!Transformed())
	{
		/*
		 * get channel names to select respective transformation functions
		 */
		string channel_x=param.xName();
		string channel_y=param.yName();

		//get vertices in valarray format
		vertices_valarray vert(antipodal_vertices);


		transformation * trans_x=trans.getTran(channel_x);
		transformation * trans_y=trans.getTran(channel_y);


		/*
		 * re-construct the trans object that was used by flowJo to transform ellipsoid gate to 256 scale
		 */
		boost::scoped_ptr<transformation> trans_gate_x,trans_gate_y;
		if(trans_x == NULL)
			trans_gate_x.reset(new scaleTrans()); //create default scale trans for linear, assuming the max value for linear scale is always 262144
		else
			trans_gate_x.reset(trans_x->clone()); //copy existing trans_x for non-linear

		if(trans_y == NULL)
			trans_gate_y.reset(new scaleTrans()); //create default scale trans for linear
		else
			trans_gate_y.reset(trans_y->clone()); //copy existing trans_y for non-linear

		//set to scale 256
		trans_gate_x->setTransformedScale(256);
		trans_gate_y->setTransformedScale(256);

		//get its inverse
		boost::shared_ptr<transformation> inverseTrans_x = trans_gate_x->getInverseTransformation();
		boost::shared_ptr<transformation> inverseTrans_y = trans_gate_y->getInverseTransformation();


		/*
		 * transform the polygon from 256 to raw
		 */
		polygonGate::transforming(inverseTrans_x.get(), inverseTrans_y.get());



		/*
		 * transform the raw to the actual data scale (for non-linear channel)
		 */
		isTransformed = false;//reset transform flag otherwise the transforming won't get executed
		polygonGate::transforming(trans_x, trans_y);

		isTransformed=true;
	}

}
/*
 * ellipsoidGate can't use ellipseGate gating function due to its special treatment of the scale
 */
vector<bool> ellipsoidGate::gating(flowData & fdata){
	return polygonGate::gating(fdata);
}
/*
 * a wrapper that calls transforming(transformation * , transformation * )
 */
void polygonGate::transforming(trans_local & trans){

		/*
		 * get channel names to select respective transformation functions
		 */
		string channel_x=param.xName();
		string channel_y=param.yName();


		/*
		 * do the actual transformations
		 */
		transformation * trans_x=trans.getTran(channel_x);
		transformation * trans_y=trans.getTran(channel_y);

		transforming(trans_x, trans_y);
}
/*
 * the actual transforming logic for polygonGate, that is shared by polyonGate and ellipsoidGate(due to the special scale)
 */
void polygonGate::transforming(transformation * trans_x, transformation * trans_y){
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


		if(trans_x!=NULL)
		{
			if(g_loglevel>=POPULATION_LEVEL)
				COUT<<"transforming: "<<channel_x<<endl;;
			trans_x->transforming(vert.x);
			for(unsigned i=0;i<vertices.size();i++)
				vertices.at(i).x=vert.x[i];
		}
		if(trans_y!=NULL)
		{
			if(g_loglevel>=POPULATION_LEVEL)
				COUT<<"transforming: "<<channel_y<<endl;;
			trans_y->transforming(vert.y);
			for(unsigned i=0;i<vertices.size();i++)
				vertices.at(i).y=vert.y[i];
		}
		if(g_loglevel>=POPULATION_LEVEL)
			COUT<<endl;
		param.setVertices(vertices);
		isTransformed=true;
	}
}

void rangeGate::transforming(trans_local & trans){
	if(!Transformed())
	{
		vertices_valarray vert(getVertices());

		transformation * curTrans=trans.getTran(param.getName());
		if(curTrans!=NULL)
		{
			if(g_loglevel>=POPULATION_LEVEL)
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


vector<bool> CurlyGuadGate::gating(flowData & fdata){
	if(interpolated)
	{
		return polygonGate::gating(fdata);
	}
	else
	{
		throw(logic_error("CurlyQuad gate has not been converted to polygonGate yet!"));
	}


}


void CurlyGuadGate::interpolate(trans_local & trans){

	string x_chnl = param.xName();
	string y_chnl = param.yName();
	/*
	 * transform intersect back to raw
	 */

	transformation * trans_x = trans.getTran(x_chnl);
	transformation * trans_y = trans.getTran(y_chnl);


	/*
	 * and rescale raw to 256 space
	 */
	boost::scoped_ptr<transformation> trans_gate_x,trans_gate_y;
	if(trans_x == NULL)
		trans_gate_x.reset(new scaleTrans()); //create default scale trans for linear, assuming the max value for linear scale is always 262144
	else
		trans_gate_x.reset(trans_x->clone()); //copy existing trans_x for non-linear

	if(trans_y == NULL)
		trans_gate_y.reset(new scaleTrans()); //create default scale trans for linear
	else
		trans_gate_y.reset(trans_y->clone()); //copy existing trans_y for non-linear

	//set to scale 256
	int displayScale = 255;
	trans_gate_x->setTransformedScale(displayScale);
	trans_gate_y->setTransformedScale(displayScale);
	polygonGate::transforming(trans_gate_x.get(), trans_gate_y.get());

//	/*
//	 * directly map from log scale to 225 space to make the curve smoother
//	 */
//	int displayScale = 255;
//	scaleTrans tx(displayScale, trans_x->getRawScale());
//	scaleTrans ty(displayScale, trans_y->getRawScale());
//	scaleTrans *trans_gate_x = &tx;
//	scaleTrans *trans_gate_y = &ty;
//	polygonGate::transforming(trans_gate_x, trans_gate_y);



	setTransformed(false);//reset flag so that it won't interfere the next transforming

	coordinate center = param.getVertices().at(0);
	double x_mu = center.x;
	double y_mu = center.y;
	//locate the a value
	double multiplier = 0.001;


	/*
	 * interpolate two curves
	 */
	unsigned nLen = 40;
	vector<coordinate> curve1(nLen), curve2(nLen);
	//curve1: round(multiplier * (x - x.mu) ^ 2) + y.mu (horizontal)
	double x_max = displayScale;//xdata.max();
	double y_max = displayScale;//ydata.max();
	double nStep = (x_max - x_mu) / nLen;
	double delta;
	for(auto i = 0; i < nLen; i++){
		delta = nStep * i;
		curve1.at(i).x = x_mu + delta;
		curve1.at(i).y = multiplier * pow(delta, 2) + y_mu;
	}
	//curve2:  (vertical)
	nStep = (y_max - y_mu) / nLen;
	for(auto i = 0; i < nLen; i++){
		delta = nStep * i;
		curve2.at(i).y = y_mu + delta;
		curve2.at(i).x = multiplier * pow(delta, 2) + x_mu;
	}

	vector<coordinate> polyVert; //the interpolated vertices for polygon
	double x_min = -4e3;//-numeric_limits<double>::max();//xdata.min();
	double y_min = -4e3;//-numeric_limits<double>::max();//ydata.min();


	/*
	 * add the other edges
	 */
	switch(quadrant)
	{
	case Q1:
	{
		//start with curv2
		polyVert = curve2;
		//top left
		polyVert.push_back(coordinate(x_min, y_max));
		//bottom left
		polyVert.push_back(coordinate(x_min, y_mu));
		//bottom right
		polyVert.push_back(curve2.front());
	}
		break;
	case Q2:
	{
		//start with curv1
		polyVert = curve1;
		//top right
		polyVert.push_back(coordinate(x_max, y_max));
		//top left
		polyVert.push_back(curve2.back());
		//add curve2 reversely
		unsigned len = polyVert.size();
		polyVert.resize(len+curve2.size());
		reverse_copy(curve2.begin(), curve2.end(), polyVert.begin()+len);
	}
		break;
	case Q3:
	{
		polyVert = curve1;
		//bottom right

		polyVert.push_back(coordinate(x_max,y_min));
		//bottom left
		polyVert.push_back(coordinate(x_mu,y_min));
		//top left
		polyVert.push_back(center);
	}
		break;
	case Q4://quadrant 4 is actually a rectangle
	{
		polyVert.push_back(center);

		polyVert.push_back(coordinate(x_mu, y_min));
		polyVert.push_back(coordinate(x_min, y_min));
		polyVert.push_back(coordinate(x_min, y_mu));
		polyVert.push_back(center);
	}
		break;
	default:
		throw(logic_error("invalid quadrant"));
	}

	param.setVertices(polyVert);

	/*
	 * scale back to the raw scale
	 */
	boost::shared_ptr<transformation> inverseGate_x,inverseGate_y;
	if(trans_gate_x){
		inverseGate_x = trans_gate_x->getInverseTransformation();
	}
	if(trans_gate_y!=NULL){
		inverseGate_y = trans_gate_y->getInverseTransformation();
	}
	polygonGate::transforming(inverseGate_x.get(), inverseGate_y.get());
	setTransformed(false);
	interpolated = true;
}
