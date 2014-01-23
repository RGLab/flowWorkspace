/*
 * spline_test.cpp
 *
 *  Created on: May 15, 2012
 *      Author: wjiang2
 */


#include "test_header.hpp"
void spline_test(){

	double x[4097],y[4097],b[4097],c[4097],d[4097];
	int n=4097;
	/*
	 * read the x,y vector from R output
	 */
    string word;
    ifstream infile_x("../output/R/x.csv");
    int counter=0;
    while (getline(infile_x, word, '\n'))
    {
//    cout << "Word: " << word << "\n";
    	x[counter]=atof(word.c_str());
    counter++;
    }
    ifstream infile_y("../output/R/y.csv");
    cout<<counter<<endl;
    counter=0;
    while (getline(infile_y, word, '\n'))
	{
    	y[counter]=atof(word.c_str());
		counter++;
	}
    cout<<counter<<endl;
	/*
	 * interpolation
	 */
	natural_spline_C(n,x, y, b, c, d);
	int imeth=2,nu=63680;
	double u[63680],v[63680];
	ifstream infile_u("../output/R/u.csv");
	counter=0;
	while (getline(infile_u, word, '\n'))
	{
//    cout << "Word: " << word << "\n";
		u[counter]=atof(word.c_str());
		counter++;
	}
	/*
	 * transformation
	 */
	spline_eval_C(&imeth,&nu,u,v,&n,x, y, b, c, d);
//	for(unsigned i=0;i<20;i++)
//		cout<<v[i]<<",";

	/*
	 * output to text for testing
	 */
	ofstream vOutput("../output/c++/v.csv");
//	ofstream yOutput("../output/c++/y.csv");
	for(int i=0;i<nu;i++)
	{
		vOutput<<v[i]<<",";

	}

}
