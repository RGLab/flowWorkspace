/*
 * gate.cpp
 *
 *  Created on: Apr 14, 2012
 *      Author: mike
 */



#include "include/gate.hpp"
#include <algorithm>


//void inPolygon_c(double *data, int nrd,
//            double *vertices, int nrv, int *res) {
//
//  int i, j, counter;
//  double xinters;
//  double p1x, p2x, p1y, p2y;
//
//  for(i=0; i<nrd; i++){//iterate over points
//    p1x=vertices[0];
//    p1y=vertices[nrv];
//    counter=0;
//    for(j=1; j < nrv+1; j++){// iterate over vertices
//      /*p1x,p1y and p2x,p2y are the endpoints of the current vertex*/
//      if (j == nrv){//the last vertice must "loop around"
//	p2x = vertices[0];
//	p2y = vertices[0+nrv];
//      }//if
//      else{
//	p2x = vertices[j];
//	p2y = vertices[j+nrv];
//      }//else
//      /*if horizontal ray is in range of vertex find the x coordinate where
//	ray and vertex intersect*/
//      if(data[i+nrd] >= min(p1y, p2y) && data[i+nrd] < max(p1y, p2y) &&
//         data[i] <= max(p1x, p2x)){
//	xinters = (data[i+nrd]-p1y)*(p2x-p1x)/(p2y-p1y)+p1x;
//	/*if intersection x coordinate == point x coordinate it lies on the
//	  boundary of the polygon, which means "in"*/
//	if(xinters==data[i]){
//	  counter=1;
//	  break;
//	}//if
//	/*count how many vertices are passed by the ray*/
//	if (xinters > data[i]){
//	  counter++;
//	}//if
//      }//if
//      p1x=p2x;
//      p1y=p2y;
//    }//for j
//    /*uneven number of vertices passed means "in"*/
//    if(counter % 2 == 0){
//      res[i]=0;
//    }//if
//    else{
//      res[i]=1;
//    }//else
//  }//for i
//}//function

/*
 *  c++ version of inPolygon_c
 */

POPINDICES polygonGate::gating(flowData mat){

	//gating

	POPINDICES res(mat.nRow);
	return res;
}


POPINDICES rangegate::gating(flowData mat){

	//gating

	POPINDICES res(mat.nRow);
	return res;
}
POPINDICES rectGate::gating(flowData mat){

	//gating

	POPINDICES res(mat.nRow);
	return res;
}
POPINDICES ellipseGate::gating(flowData mat){

	//gating

	POPINDICES res(mat.nRow);
	return res;
}

