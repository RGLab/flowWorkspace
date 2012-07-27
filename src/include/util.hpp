/*
 * util.hpp
 *
 *  Created on: Jul 26, 2012
 *      Author: wjiang2
 */

#ifndef UTIL_HPP_
#define UTIL_HPP_
#include <sstream>
#include <vector>
#include <iostream>
#include <string>
#include <valarray>
using namespace std;

string trim(string str);
valarray<double> toArray(string sCalTable);
vector<string> splitString(string source,char token);


#endif /* UTIL_HPP_ */
