/*
 * computeCalibrationTable.cpp
 *
 *  Created on: May 15, 2012
 *      Author: wjiang2
 */

#include "test_header.hpp"

void compCalTbl(){
	biexpTrans t;
	t.computCalTbl();
	calibrationTable cal=t.calTbl;
	/*
	 * output to text for testing
	 */
	int nx=cal.x.size();
	ofstream xOutput("/home/wjiang2/rglab/workspace/flowWorkspace/output/cpp/compCalTbl.txt");

	for(int i=0;i<nx;i++)
	{
		xOutput<<cal.x[i];
		if(i!=nx-1)
			xOutput<<" ";
	}
	xOutput.close();
	cout<<"done!"<<endl;
}

