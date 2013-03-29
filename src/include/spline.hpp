/*
 * spline.hpp
 *
 *  Created on: Apr 26, 2012
 *      Author: wjiang2
 */

#ifndef SPLINE_HPP_
#define SPLINE_HPP_


#include <math.h>
#include <stdexcept>
#include <iostream>
#include <valarray>
#define NATURAL 2

using namespace std;
void natural_spline_C(int n, float *x, float *y, float *b, float *c, float *d);

void spline_eval_C(int *method, int *nu, float *u, float *v,
		 int *n, float *x, float *y, float *b, float *c, float *d);


void natural_spline(valarray<float>x, valarray<float> y, valarray<float>& b,valarray<float>& c,valarray<float>& d);
void spline_eval(int method, valarray<float> u, valarray<float> & v,const valarray<float> & x, const valarray<float> & y, const valarray<float> & b, const valarray<float> & c, const valarray<float> & d);
#endif /* SPLINE_HPP_ */
