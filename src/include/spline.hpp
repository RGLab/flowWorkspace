/*
 * spline.hpp
 *
 *  Created on: Apr 26, 2012
 *      Author: wjiang2
 */


#ifndef SPLINE_HPP_
#include <Rcpp.h>
using namespace Rcpp;

#define SPLINE_HPP_
#ifdef ROUT
#define COUT Rcout
#endif


#ifndef ROUT
#define COUT cout
#endif

#include <math.h>
#include <stdexcept>
#include <iostream>
#include <valarray>
#define NATURAL 2

using namespace std;
void natural_spline_C(int n, double *x, double *y, double *b, double *c, double *d);

void spline_eval_C(int *method, int *nu, double *u, double *v,
		 int *n, double *x, double *y, double *b, double *c, double *d);


void natural_spline(valarray<double>x, valarray<double> y, valarray<double>& b,valarray<double>& c,valarray<double>& d);
void spline_eval(int method, valarray<double> u, valarray<double> & v,const valarray<double> & x, const valarray<double> & y, const valarray<double> & b, const valarray<double> & c, const valarray<double> & d);
#endif /* SPLINE_HPP_ */
