/*
 * ellipse2points.hpp
 *
 *  Created on: Feb 16, 2017
 *      Author: wjiang2
 */

#ifndef INCLUDE_ELLIPSE2POINTS_HPP_
#define INCLUDE_ELLIPSE2POINTS_HPP_
#include "flowData.hpp"

struct ellipse_parsed{

  float mu_x, mu_y, a, b, alpha;
  vector<float>x,y;

};
struct matrix{
	vector<float> x;
	vector<float> y;
};
matrix toPoly(ellipse_parsed res, int n);
ellipse_parsed parseEllipse(vector<float> x, vector<float> y);



#endif /* INCLUDE_ELLIPSE2POINTS_HPP_ */
